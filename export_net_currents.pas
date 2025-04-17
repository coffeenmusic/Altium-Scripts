{..............................................................................}
{ Net Current Capacity Analyzer - Complete Path Version                       }
{                                                                             }
{ Analyzes all nets on a PCB and generates a CSV file with                    }
{ net names and their worst-case current capacities                           }
{ Calculates current capacity based on complete paths with the same width     }
{ Traces paths across multiple layers through vias                            }
{..............................................................................}

// Forward declarations to resolve circular references
Procedure FindConnectedObjects(Board : IPCB_Board; Net : IPCB_Net; X, Y : Integer;
                              Layer : TLayer; Width : TCoord; ProcessedTracks : TObjectList;
                              ProcessedVias : TObjectList; CurrentPath : TObjectList;
                              Tolerance : TCoord); Forward;

Procedure FindConnectedLayerTracks(Board : IPCB_Board; Net : IPCB_Net; X, Y : Integer;
                                 Layer : TLayer; Width : TCoord; ProcessedTracks : TObjectList;
                                 ProcessedVias : TObjectList; CurrentPath : TObjectList;
                                 Tolerance : TCoord); Forward;

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
    CurrentLayer : TLayer;
    CurrentWidth : TCoord;
Begin
    Result := 0; // Initialize with 0

    // No tracks in path
    if (Tracks = nil) or (Tracks.Count = 0) then Exit;

    // Get the first track to determine width and layer
    Track := Tracks[0];

    // Skip if not a track or arc
    if not ((Track.ObjectID = eTrackObject) or (Track.ObjectID = eArcObject)) then Exit;

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

    // We need to separate calculations by layer and width
    // Group tracks by layer/width and calculate capacity for each group
    TotalLength := 0;
    CurrentWidth := 0;
    CurrentLayer := 0;

    for i := 0 to Tracks.Count - 1 do
    begin
        Track := Tracks[i];

        // Only process tracks and arcs
        if (Track.ObjectID = eTrackObject) or (Track.ObjectID = eArcObject) then
        begin
            // Calculate length based on object type
            Case Track.ObjectID of
                eTrackObject:
                begin
                    if (CurrentWidth <> Track.Width) or (CurrentLayer <> Track.Layer) then
                    begin
                        CurrentWidth := Track.Width;
                        CurrentLayer := Track.Layer;
                    end;

                    TrackLength := coordToMils(sqrt(sqr(Track.x2 - Track.x1) + sqr(Track.y2 - Track.Y1)));
                end;
                eArcObject:
                begin
                    if (CurrentWidth <> Track.LineWidth) or (CurrentLayer <> Track.Layer) then
                    begin
                        CurrentWidth := Track.LineWidth;
                        CurrentLayer := Track.Layer;
                    end;

                    TrackLength := coordToMils(((Track.StartAngle - Track.EndAngle)/360) * pi * Track.Radius);
                end;
                else
                    TrackLength := 0;
            End;

            TotalLength := TotalLength + TrackLength;
        end;
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

// Helper function to find connected tracks and vias at a point
Procedure FindConnectedObjects(Board : IPCB_Board; Net : IPCB_Net; X, Y : Integer;
                              Layer : TLayer; Width : TCoord; ProcessedTracks : TObjectList;
                              ProcessedVias : TObjectList; CurrentPath : TObjectList;
                              Tolerance : TCoord);
Var
    SIter : IPCB_SpatialIterator;
    Primitive : IPCB_Primitive;
    Via : IPCB_Via;
    Track : IPCB_Primitive;
    ConnectedX, ConnectedY : Integer;
    IsConnected : Boolean;
    TrackWidth : TCoord;
Begin
    // Create spatial iterator to find nearby objects
    SIter := Board.SpatialIterator_Create;
    SIter.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject, eViaObject));
    SIter.AddFilter_Area(X - Tolerance, Y - Tolerance, X + Tolerance, Y + Tolerance);

    // Find all connected objects
    Primitive := SIter.FirstPCBObject;
    While (Primitive <> nil) do
    Begin
        // Check if object is in the same net
        if (Primitive.InNet) and (Primitive.Net.Name = Net.Name) then
        begin
            Case Primitive.ObjectID of
                eTrackObject, eArcObject:
                begin
                    // Only process tracks with same width and same layer
                    if Primitive.ObjectID = eTrackObject then
                        TrackWidth := Primitive.Width
                    else
                        TrackWidth := Primitive.LineWidth;

                    if (Primitive.Layer = Layer) and (TrackWidth = Width) then
                    begin
                        // Check if this track is already processed
                        if ProcessedTracks.IndexOf(Primitive) = -1 then
                        begin
                            Track := Primitive;
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

                                // Recursively find next connected objects
                                FindConnectedObjects(Board, Net, ConnectedX, ConnectedY, Layer, Width, ProcessedTracks, ProcessedVias, CurrentPath, Tolerance);
                            end;
                        end;
                    end;
                end;
                eViaObject:
                begin
                    Via := Primitive;
                    // Check if via is already processed
                    if ProcessedVias.IndexOf(Via) = -1 then
                    begin
                        // Check if via is connected at this point
                        if (Abs(Via.x - X) <= Tolerance) and (Abs(Via.y - Y) <= Tolerance) then
                        begin
                            // Check if via connects to current layer
                            if Via.IntersectLayer(Layer) then
                            begin
                                // Add via to processed list and current path
                                ProcessedVias.Add(Via);
                                CurrentPath.Add(Via);

                                // Find tracks on all other layers this via connects to
                                // Create a spatial iterator for each layer
                                for Layer := eTopLayer to eBottomLayer do
                                begin
                                    if Via.IntersectLayer(Layer) then
                                    begin
                                        // Find tracks on this layer
                                        FindConnectedLayerTracks(Board, Net, Via.x, Via.y, Layer, Width, ProcessedTracks, ProcessedVias, CurrentPath, Tolerance);
                                    end;
                                end;
                            end;
                        end;
                    end;
                end;
            end;
        end;

        Primitive := SIter.NextPCBObject;
    End;

    // Clean up
    Board.SpatialIterator_Destroy(SIter);
End;

// Helper function to find connected tracks on a specific layer
Procedure FindConnectedLayerTracks(Board : IPCB_Board; Net : IPCB_Net; X, Y : Integer;
                                 Layer : TLayer; Width : TCoord; ProcessedTracks : TObjectList;
                                 ProcessedVias : TObjectList; CurrentPath : TObjectList;
                                 Tolerance : TCoord);
Var
    SIter : IPCB_SpatialIterator;
    Primitive : IPCB_Primitive;
    Track : IPCB_Primitive;
    ConnectedX, ConnectedY : Integer;
    IsConnected : Boolean;
    TrackWidth : TCoord;
Begin
    // Create spatial iterator to find nearby tracks on this layer
    SIter := Board.SpatialIterator_Create;
    SIter.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject));
    SIter.AddFilter_LayerSet(MkSet(Layer));
    SIter.AddFilter_Area(X - Tolerance, Y - Tolerance, X + Tolerance, Y + Tolerance);

    // Find all connected tracks
    Primitive := SIter.FirstPCBObject;
    While (Primitive <> nil) do
    Begin
        // Check if track is in the same net
        if (Primitive.InNet) and (Primitive.Net.Name = Net.Name) then
        begin
            // Only process tracks with the width we're looking for
            if Primitive.ObjectID = eTrackObject then
                TrackWidth := Primitive.Width
            else
                TrackWidth := Primitive.LineWidth;

            // Use relaxed width matching for tracks connected via vias
            if (TrackWidth > 0) then // Use the current track width
            begin
                // Check if this track is already processed
                if ProcessedTracks.IndexOf(Primitive) = -1 then
                begin
                    Track := Primitive;
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

                        // Get the actual width of this track
                        if Track.ObjectID = eTrackObject then
                            Width := Track.Width
                        else
                            Width := Track.LineWidth;

                        // Recursively find next connected objects
                        FindConnectedObjects(Board, Net, ConnectedX, ConnectedY, Layer, Width, ProcessedTracks, ProcessedVias, CurrentPath, Tolerance);
                    end;
                end;
            end;
        end;

        Primitive := SIter.NextPCBObject;
    End;

    // Clean up
    Board.SpatialIterator_Destroy(SIter);
End;

// Helper function to trace a complete path of connected tracks
Function TraceCompletePath(Board : IPCB_Board; StartTrack : IPCB_Primitive;
                          ProcessedTracks : TObjectList; ProcessedVias : TObjectList) : TObjectList;
Var
    CurrentPath : TObjectList;
    X1, Y1, X2, Y2, i : Integer;
    Width : TCoord;
    Layer : TLayer;
    Tolerance : TCoord;
    Obj: IPCB_Obj;
Begin
    // Create object list for the current path
    CurrentPath := CreateObject(TObjectList);
    CurrentPath.OwnsObjects := False; // Don't destroy track objects

    // Only process tracks and arcs
    if not ((StartTrack.ObjectID = eTrackObject) or (StartTrack.ObjectID = eArcObject)) then
    begin
        Result := CurrentPath;
        Exit;
    end;

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
    FindConnectedObjects(Board, StartTrack.Net, X1, Y1, Layer, Width,
                        ProcessedTracks, ProcessedVias, CurrentPath, Tolerance);
    FindConnectedObjects(Board, StartTrack.Net, X2, Y2, Layer, Width,
                        ProcessedTracks, ProcessedVias, CurrentPath, Tolerance);

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
    ProcessedVias : TObjectList;
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

    // Create list to track processed tracks and vias
    ProcessedTracks := CreateObject(TObjectList);
    ProcessedTracks.OwnsObjects := False; // Don't destroy tracks

    ProcessedVias := CreateObject(TObjectList);
    ProcessedVias.OwnsObjects := False; // Don't destroy vias

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

        // Clear processed lists
        ProcessedTracks.Clear;
        ProcessedVias.Clear;

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
                if (ProcessedTracks.IndexOf(Track) = -1) then
                begin
                    // Trace the complete path starting from this track
                    CurrentPath := TraceCompletePath(Board, Track, ProcessedTracks, ProcessedVias);

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
        //ProcessedVias.Free;
        ResultsList.Free;
        NetCapacities.Free;
    End;
End;
