{..............................................................................}
{ Net Current Capacity Analyzer                                               }
{                                                                             }
{ Analyzes all nets on a PCB and generates a CSV file with                    }
{ net names and their worst-case current capacities                           }
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

// Helper function to calculate current capacity for a track/arc
Function CalculateCurrentCapacity(Track : IPCB_Primitive; Board : IPCB_Board; LStack : IPCB_LayerStack) : Double;
Var
    h : Double; // Layer Thickness in mils
    w : Double; // Track Width in mils
    k, c, b : Double; // IPC-2221 Constants
    z : Double; // Area
    L1 : IPCB_LayerObject;
    isMidLayer : Boolean;
    I_10 : Double; // Current for 10°C rise (we'll use this as our reference)
    tk_10 : Double; // Temperature rise constant
Begin
    Result := 0; // Initialize with 0

    // Get layer information
    L1 := LStack.LayerObject(Track.Layer);
    if L1 = nil then Exit; // Skip if layer not found

    // Get Layer Thickness in mils
    h := coordToMils(L1.CopperThickness);

    // Determine width based on object type
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

// Main procedure to analyze net current capacities and save to CSV
Procedure AnalyzeNetCurrentCapacities;
Var
    Board : IPCB_Board;
    LStack : IPCB_LayerStack;
    NetList : TObjectList;
    NetIterator : IPCB_BoardIterator;
    CurrentNet : IPCB_Net;
    i : Integer;
    GrpIter : IPCB_GroupIterator;
    TrackObj : IPCB_Primitive;
    CurrentCapacity, TrackCapacity : Double;
    ResultsList : TStringList;
    HasTracks : Boolean;
    NetName: String;
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

    // Create string list for results
    ResultsList := TStringList.Create;

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
            CurrentNet := NetIterator.NextPCBObject;
        End;

        Board.BoardIterator_Destroy(NetIterator);

        ShowMessage('Found ' + IntToStr(NetList.Count) + ' unique nets');

        // Second pass: find worst-case current capacity for each net
        For i := 0 to NetList.Count - 1 Do
        Begin
            CurrentNet := NetList[i];

            If CurrentNet.Name = 'ENET-NC2' Then
            Begin
                 NetName := CurrentNet.Name;
            End;

            // Create group iterator for this net
            GrpIter := CurrentNet.GroupIterator_Create;
            GrpIter.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject));

            // Start with a high value for worst case
            CurrentCapacity := 999999.9; // Force double
            HasTracks := False;

            // Check all tracks/arcs in this net
            TrackObj := GrpIter.FirstPCBObject;
            While (TrackObj <> Nil) Do
            Begin
                // Calculate current capacity for this track
                TrackCapacity := CalculateCurrentCapacity(TrackObj, Board, LStack);

                // Only consider tracks with valid capacities (> 0)
                if TrackCapacity > 0 then
                begin
                    // Use our custom MinDouble function instead of Min
                    CurrentCapacity := MinDouble(CurrentCapacity, TrackCapacity);
                    HasTracks := True;
                end;

                TrackObj := GrpIter.NextPCBObject;
            End;

            // Add to results if tracks were found with valid capacities
            if HasTracks and (CurrentCapacity < 999999.0) then
            begin
                ResultsList.Add(CurrentNet.Name + ',' + FloatToStrF(CurrentCapacity, ffFixed, 10, 4));
            end
            else
            begin
                // No tracks in this net or all had zero capacity
                ResultsList.Add(CurrentNet.Name + ',N/A');
            end;

            // Destroy iterator
            CurrentNet.GroupIterator_Destroy(GrpIter);
        End;

        // Save results to CSV file
        ResultsList.SaveToFile('C:\Users\Stephen Thompson\Downloads\NetCurrents.csv');

        ShowMessage('Analysis complete. Results saved to C:\Users\Stephen Thompson\Downloads\NetCurrents.csv');

    Finally
        //NetList.Free; // Freeing TObjectList causes access error violation
        ResultsList.Free;
    End;
End;
