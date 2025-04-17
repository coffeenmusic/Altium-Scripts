{..............................................................................}
{ Net Current Capacity Analyzer - Complete Path Version                       }
{                                                                             }
{ Analyzes all nets on a PCB and generates a CSV file with                    }
{ net names and their worst-case current capacities                           }
{ Calculates current capacity based on complete paths with the same width     }
{..............................................................................}

// Power function from the original script
Function pow(base, exponent: double): double;
Begin
    result := Exp(Exponent*Ln(Base));
    if base = 0 then result := 0;
End;

// Custom minimum function for doubles
Function MinDouble(Value1, Value2: Double): Double;
Begin
    if Value1 < Value2 then
        Result := Value1
    else
        Result := Value2;
End;

// Helper function to calculate current capacity for a track path
Function CalculatePathCurrentCapacity(Tracks : TObjectList; Board : IPCB_Board; LStack : IPCB_LayerStack) : Double;
Var
    h : Double; // Layer Thickness in mils
    w : Double; // Track Width in mils
    k, c, b : Double; // IPC-2221 Constants
    z : Double; // Area
    L1 : IPCB_LayerObject;
    isMidLayer : Boolean;
    I_10 : Double; // Current for 10°C rise (we'll use this as our reference)
    tk_10 : Double; // Temperature rise constant
    TotalLength : Double; // Total path length in mils
    Track : IPCB_Primitive;
    TrackLength : Double;
    i : Integer;
Begin
    Result := 0; // Initialize with 0

    // No tracks in path
    if (Tracks = nil) or (Tracks.Count = 0) then Exit;

    // Get the first track to determine width and layer
    Track := Tracks[0];

    // Get layer information
    L1 := LStack.LayerObject(Track.Layer);
    if L1 = nil then Exit; // Skip if layer not found

    // Get Layer Thickness in mils
    h := coordToMils(L1.CopperThickness);

    // Determine width based on object type of first track
    Case Track.ObjectID of
        eTrackObject:
            w := coordToMils(Track.Width);
        eArcObject:
            w := coordToMils(Track.LineWidth);
        else
            Exit; // Exit with result = 0
    End;

    // Skip if width or thickness is zero
    if (w <= 0) or (h <= 0) then Exit;

    // Calculate total path length
    TotalLength := 0;
    for i := 0 to Tracks.Count - 1 do
    begin
        Track := Tracks[i];

        // Calculate length based on object type
        Case Track.ObjectID of
            eTrackObject:
                TrackLength := coordToMils(sqrt(sqr(Track.x2 - Track.x1) + sqr(Track.y2 - Track.Y1)));
            eArcObject:
                TrackLength := coordToMils(((Track.StartAngle - Track.EndAngle)/360) * pi * Track.Radius);
            else
                TrackLength := 0;
        End;

        TotalLength := TotalLength + TrackLength;
    end;

    // Assign IPC-2221 constants
    c := 0.725;
    b := 0.44;

    // Check if on middle layer (using the same logic as the original script)
    isMidLayer := (Track.Layer <> 1) and (Track.Layer <> 32);

    // Assign k value based on layer location
    if (isMidLayer) then
        k := 0.024 // Internal layer
    else
        k := 0.048; // External layer

    // Calculate temperature constant for 10°C rise (using the original pow function)
    tk_10 := k * pow(10, b);

    // Calculate cross-sectional area
    z := pow((h * w), c);

    // Calculate current for 10°C rise
    I_10 := z * tk_10;

    Result := I_10;
End;

// Helper function to get the key for a track (used for identifying unique paths)
Function GetTrackKey(Track : IPCB_Primitive) : String;
Begin
    // Create a unique key based on track properties
    Result := IntToStr(Track.Layer) + '-';

    Case Track.ObjectID of
        eTrackObject:
            Result := Result + IntToStr(Track.Width);
        eArcObject:
            Result := Result + IntToStr(Track.LineWidth);
        else
            Result := Result + '0';
    End;
End;

// Helper function to find connected tracks at a point
Procedure FindConnectedTracks(Board : IPCB_Board; Net : IPCB_Net; X, Y : Integer;
                              Layer : TLayer; Width : TCoord; ProcessedTracks : TObjectList;
                              CurrentPath : TObjectList; Tolerance : TCoord);
Var
    SIter : IPCB_SpatialIterator;
    Track : IPCB_Primitive;
    ConnectedX, ConnectedY : Integer;
    IsConnected : Boolean;
Begin
    // Create spatial iterator to find nearby objects
    SIter := Board.SpatialIterator_Create;
    SIter.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject));
    SIter.AddFilter_LayerSet(MkSet(Layer));
    SIter.AddFilter_Area(X - Tolerance, Y - Tolerance, X + Tolerance, Y + Tolerance);

    // Find all connected tracks
    Track := SIter.FirstPCBObject;
    While (Track <> nil) do
    Begin
        // Check if the track is in the same net and has the same width
        if (Track.InNet) and (Track.Net.Name = Net.Name) and
           (((Track.ObjectID = eTrackObject) and (Track.Width = Width)) or
            ((Track.ObjectID = eArcObject) and (Track.LineWidth = Width))) then
        begin
            // Check if this track is already processed
            if ProcessedTracks.IndexOf(Track) = -1 then
            begin
                // Determine connection point based on object type
                IsConnected := False;

                Case Track.ObjectID of
                    eTrackObject:
                    begin
                        // Check if either endpoint connects
                        if (Abs(Track.x1 - X) <= Tolerance) and (Abs(Track.y1 - Y) <= Tolerance) then
                        begin
                            ConnectedX := Track.x2;
                            ConnectedY := Track.y2;
                            IsConnected := True;
                        end
                        else if (Abs(Track.x2 - X) <= Tolerance) and (Abs(Track.y2 - Y) <= Tolerance) then
                        begin
                            ConnectedX := Track.x1;
                            ConnectedY := Track.y1;
                            IsConnected := True;
                        end;
                    end;
                    eArcObject:
                    begin
                        // Check if either endpoint connects
                        if (Abs(Track.StartX - X) <= Tolerance) and (Abs(Track.StartY - Y) <= Tolerance) then
                        begin
                            ConnectedX := Track.EndX;
                            ConnectedY := Track.EndY;
                            IsConnected := True;
                        end
                        else if (Abs(Track.EndX - X) <= Tolerance) and (Abs(Track.EndY - Y) <= Tolerance) then
                        begin
                            ConnectedX := Track.StartX;
                            ConnectedY := Track.StartY;
                            IsConnected := True;
                        end;
                    end;
                end;

                // If connected, add to path and continue tracing
                if IsConnected then
                begin
                    // Add to processed list and current path
                    ProcessedTracks.Add(Track);
                    CurrentPath.Add(Track);

                    // Recursively find next connected track
                    FindConnectedTracks(Board, Net, ConnectedX, ConnectedY, Layer, Width,
                                       ProcessedTracks, CurrentPath, Tolerance);
                end;
            end;
        end;

        Track := SIter.NextPCBObject;
    End;

    // Clean up
    Board.SpatialIterator_Destroy(SIter);
End;

// Helper function to trace a complete path of connected tracks
Function TraceCompletePath(Board : IPCB_Board; StartTrack : IPCB_Primitive;
                          ProcessedTracks : TObjectList) : TObjectList;
Var
    CurrentPath : TObjectList;
    X1, Y1, X2, Y2, i : Integer;
    Width : TCoord;
    Layer : TLayer;
    Tolerance : TCoord;
    Obj : TObject;
Begin
    // Create object list for the current path
    CurrentPath := CreateObject(TObjectList);
    CurrentPath.OwnsObjects := False; // Don't destroy track objects

    // Add starting track to path and processed list
    CurrentPath.Add(StartTrack);
    ProcessedTracks.Add(StartTrack);

    // Get track properties
    Layer := StartTrack.Layer;

    Case StartTrack.ObjectID of
        eTrackObject:
        begin
            Width := StartTrack.Width;
            X1 := StartTrack.x1;
            Y1 := StartTrack.y1;
            X2 := StartTrack.x2;
            Y2 := StartTrack.y2;
        end;
        eArcObject:
        begin
            Width := StartTrack.LineWidth;
            X1 := StartTrack.StartX;
            Y1 := StartTrack.StartY;
            X2 := StartTrack.EndX;
            Y2 := StartTrack.EndY;
        end;
        else
        begin
            // Not a track or arc
            Result := CurrentPath;
            Exit;
        end;
    End;

    // Set tolerance for connection detection (1 mil)
    Tolerance := MilsToCoord(1);

    // Trace in both directions from start track
    FindConnectedTracks(Board, StartTrack.Net, X1, Y1, Layer, Width, ProcessedTracks, CurrentPath, Tolerance);
    FindConnectedTracks(Board, StartTrack.Net, X2, Y2, Layer, Width, ProcessedTracks, CurrentPath, Tolerance);

    // Test: View selected Path
    //For i := 0 to CurrentPath.Count - 1 Do
    //Begin
    //    Obj := CurrentPath[i];
    //    Obj.Selected := True;
    //End;

    //Client.SendMessage('PCB:Zoom', 'Action=Selected' , 255, Client.CurrentView);
    //Client.SendMessage('PCB:DeSelect', 'Scope=All', 255, Client.CurrentView); // Deselect All

    Result := CurrentPath;
End;

// Main procedure to analyze net current capacities and save to CSV
Procedure AnalyzeNetCurrentCapacities;
Var
    Board : IPCB_Board;
    LStack : IPCB_LayerStack;
    NetList : TObjectList;
    NetIterator : IPCB_BoardIterator;
    CurrentNet : IPCB_Net;
    TrackIter : IPCB_GroupIterator;
    Track : IPCB_Primitive;
    ProcessedTracks : TObjectList;
    CurrentPath : TObjectList;
    PathCapacity : Double;
    i : Integer;
    ResultsList : TStringList;
    NetCapacities : TStringList;
    MinNetCapacity : Double;
    HasPaths : Boolean;
Begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then
    begin
        ShowMessage('No board found');
        Exit;
    end;

    // Get the layer stack
    LStack := Board.LayerStack;

    // Create list to store unique nets
    NetList := CreateObject(TObjectList);
    NetList.OwnsObjects := False; // Don't destroy nets when list is freed

    // Create list to track processed tracks
    ProcessedTracks := CreateObject(TObjectList);
    ProcessedTracks.OwnsObjects := False; // Don't destroy tracks

    // Create string list for results
    ResultsList := TStringList.Create;

    // Create string list for net capacities
    NetCapacities := TStringList.Create;

    Try
        // Add CSV header
        ResultsList.Add('Net Name,Current Capacity (A)');

        // Create iterator for net objects
        NetIterator := Board.BoardIterator_Create;
        NetIterator.AddFilter_ObjectSet(MkSet(eNetObject));
        NetIterator.AddFilter_LayerSet(AllLayers);
        NetIterator.AddFilter_Method(eProcessAll);

        // First pass: collect all unique nets
        CurrentNet := NetIterator.FirstPCBObject;
        While (CurrentNet <> Nil) Do
        Begin
            NetList.Add(CurrentNet);
            NetCapacities.Add('999999.9'); // Initialize with a high value
            CurrentNet := NetIterator.NextPCBObject;
        End;

        Board.BoardIterator_Destroy(NetIterator);

        ShowMessage('Found ' + IntToStr(NetList.Count) + ' unique nets');

        // Second pass: find worst-case current capacity for each net
        For i := 0 to NetList.Count - 1 Do
        Begin
            CurrentNet := NetList[i];
            MinNetCapacity := 999999.9;
            HasPaths := False;

            // Create group iterator for this net's tracks
            TrackIter := CurrentNet.GroupIterator_Create;
            TrackIter.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject));

            // Process each track that hasn't been processed yet
            Track := TrackIter.FirstPCBObject;
            While (Track <> Nil) Do
            Begin
                // Only process tracks we haven't seen yet
                if ProcessedTracks.IndexOf(Track) = -1 then
                begin
                    // Trace the complete path starting from this track
                    CurrentPath := TraceCompletePath(Board, Track, ProcessedTracks);

                    // Calculate capacity for this path
                    if CurrentPath.Count > 0 then
                    begin
                        PathCapacity := CalculatePathCurrentCapacity(CurrentPath, Board, LStack);

                        // If valid capacity, update minimum
                        if PathCapacity > 0 then
                        begin
                            MinNetCapacity := MinDouble(MinNetCapacity, PathCapacity);
                            HasPaths := True;
                        end;
                    end;

                    // Free the path list (not the objects)
                    //CurrentPath.Free;
                end;

                Track := TrackIter.NextPCBObject;
            End;

            // Destroy iterator
            CurrentNet.GroupIterator_Destroy(TrackIter);

            // Add result to list
            if HasPaths and (MinNetCapacity < 999999.0) then
            begin
                NetCapacities[i] := FloatToStrF(MinNetCapacity, ffFixed, 10, 4);
            end;
        End;

        // Create the final CSV with results
        For i := 0 to NetList.Count - 1 Do
        Begin
            CurrentNet := NetList[i];

            if NetCapacities[i] = '999999.9' then
                ResultsList.Add(CurrentNet.Name + ',N/A')
            else
                ResultsList.Add(CurrentNet.Name + ',' + NetCapacities[i]);
        End;

        // Save results to CSV file
        ResultsList.SaveToFile('C:\Users\Stephen Thompson\Downloads\NetCurrents.csv');

        ShowMessage('Analysis complete. Results saved to C:\Users\Stephen Thompson\Downloads\NetCurrents.csv');

    Finally
        // NetList.Free; -- Don't free this to avoid access violation
        //ProcessedTracks.Free;
        ResultsList.Free;
        NetCapacities.Free;
    End;
End;
