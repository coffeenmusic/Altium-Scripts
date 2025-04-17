{..............................................................................}
{ Net Current Capacity Visualizer                                             }
{                                                                             }
{ Sets color of net objects based on worst-case current carrying capacity     }
{ Uses IPC-2221 standard for current calculations                             }
{..............................................................................}

// Helper function to calculate current capacity for a track/arc
Function CalculateCurrentCapacity(Track : IPCB_Primitive; Board : IPCB_Board; LStack : IPCB_LayerStack) : Double;
Var
    h : TCoord; // Layer Thickness
    w : Double; // Track Width
    k, c, b : Double; // IPC-2221 Constants
    z : Double; // Area
    L1 : IPCB_LayerObject;
    isMidLayer : Boolean;
    I_10 : Double; // Current for 10°C rise (we'll use this as our reference)
    tk_10 : Double; // Temperature rise constant
Begin
    // Get layer information
    L1 := LStack.LayerObject(Track.Layer);
    h := L1.CopperThickness; // Get Layer Thickness

    // Determine width based on object type
    Case Track.ObjectID of
        eTrackObject:
            w := coordToMils(Track.Width);
        eArcObject:
            w := coordToMils(Track.LineWidth);
        else
            begin
                Result := 0;
                Exit;
            end;
    End;

    // Assign IPC-2221 constants
    c := 0.725;
    b := 0.44;

    // Check if on middle layer
    isMidLayer := (Track.Layer <> 1) and (Track.Layer <> 32);

    // Assign k value based on layer location
    if (isMidLayer) then
        k := 0.024 // Internal layer
    else
        k := 0.048; // External layer

    // Calculate temperature constant for 10°C rise
    tk_10 := k * Power(10, b);

    // Calculate cross-sectional area
    z := Power((coordToMils(h) * w), c);

    // Calculate current for 10°C rise
    I_10 := z * tk_10;

    Result := I_10;
End;

// Helper function to convert hex color to integer
Function HexToInt(HexStr : String) : Integer;
Var
    i, j, Len : Integer;
Begin
    // Remove '0x' or '#' prefix if present
    if Copy(HexStr, 1, 2) = '0x' then
        Delete(HexStr, 1, 2)
    else if Copy(HexStr, 1, 1) = '#' then
        Delete(HexStr, 1, 1);

    Result := 0;
    Len := Length(HexStr);

    for i := 1 to Len do
    begin
        j := Pos(UpCase(HexStr[i]), '0123456789ABCDEF') - 1;
        if j >= 0 then
            Result := Result * 16 + j;
    end;
End;

// Helper function to generate a color based on current capacity
Function GetColorForCurrentCapacity(CurrentCapacity, MinCapacity, MaxCapacity : Double) : Integer;
Var
    ColorValue : String;
    BlueValue, GreenValue, RedValue : Integer;
    Ratio : Double;
Begin
    // Calculate ratio between min and max (0.0 to 1.0)
    Ratio := (CurrentCapacity - MinCapacity) / (MaxCapacity - MinCapacity);
    if Ratio < 0 then Ratio := 0;
    if Ratio > 1 then Ratio := 1;

    // Create a color gradient from red (low capacity) to green (high capacity)
    // For RGB, we'll use values from 0 to 255

    if Ratio < 0.5 then
    begin
        // Red to Yellow gradient
        RedValue := 255;
        GreenValue := Round(255 * (Ratio * 2));
        BlueValue := 0;
    end
    else
    begin
        // Yellow to Green gradient
        RedValue := Round(255 * (1 - (Ratio - 0.5) * 2));
        GreenValue := 255;
        BlueValue := 0;
    end;

    // Altium uses BGR format for colors
    Result := (RedValue) + (GreenValue shl 8) + (BlueValue shl 16);
End;

// Main procedure to color nets based on current capacity
Procedure ColorNetsByCurrentCapacity;
Var
    Board : IPCB_Board;
    LStack : IPCB_LayerStack;
    NetList : TObjectList;
    NetIterator : IPCB_BoardIterator;
    CurrentNet : IPCB_Net;
    NetMap : TStringList; // Maps net names to their indices in NetList
    i, NetIndex : Integer;
    GrpIter : IPCB_GroupIterator;
    TrackObj : IPCB_Primitive;
    CurrentCapacity : Double;
    MinCapacity, MaxCapacity : Double;
    CurrentNetCapacities : TStringList; // Store worst case capacity for each net
    ColorValue : Integer;
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

    // Create string list to map net names to indices
    NetMap := TStringList.Create;

    // Create string list to store current capacities
    CurrentNetCapacities := TStringList.Create;

    Try
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
            NetMap.AddObject(CurrentNet.Name, CurrentNet);
            CurrentNetCapacities.Add('999999'); // Initialize with a high value

            CurrentNet := NetIterator.NextPCBObject;
        End;

        Board.BoardIterator_Destroy(NetIterator);

        ShowMessage('Found ' + IntToStr(NetList.Count) + ' unique nets');

        // Initialize min/max capacity
        MinCapacity := 999999;
        MaxCapacity := 0;

        // Second pass: find worst-case current capacity for each net
        For i := 0 to NetList.Count - 1 Do
        Begin
            CurrentNet := NetList[i];

            // Create group iterator for this net
            GrpIter := CurrentNet.GroupIterator_Create;
            GrpIter.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject));

            // Start with a high value for worst case
            CurrentCapacity := 999999;

            // Check all tracks/arcs in this net
            TrackObj := GrpIter.FirstPCBObject;
            While (TrackObj <> Nil) Do
            Begin
                // Calculate current capacity for this track
                CurrentCapacity := Min(CurrentCapacity, CalculateCurrentCapacity(TrackObj, Board, LStack));

                TrackObj := GrpIter.NextPCBObject;
            End;

            // Store worst-case capacity for this net
            if CurrentCapacity < 999999 then
            begin
                CurrentNetCapacities[i] := FloatToStr(CurrentCapacity);

                // Update min/max for color scaling
                if CurrentCapacity < MinCapacity then MinCapacity := CurrentCapacity;
                if CurrentCapacity > MaxCapacity then MaxCapacity := CurrentCapacity;
            end;

            // Destroy iterator
            CurrentNet.GroupIterator_Destroy(GrpIter);
        End;

        // Add a small buffer to max capacity to avoid divide by zero
        if MinCapacity = MaxCapacity then MaxCapacity := MaxCapacity + 0.001;

        ShowMessage('Min capacity: ' + FloatToStr(MinCapacity) + 'A, Max capacity: ' + FloatToStr(MaxCapacity) + 'A');

        // Final pass: set colors based on current capacity
        PCBServer.PreProcess;

        For i := 0 to NetList.Count - 1 Do
        Begin
            CurrentNet := NetList[i];
            CurrentCapacity := StrToFloat(CurrentNetCapacities[i]);

            // Skip nets with no calculated capacity
            if CurrentCapacity >= 999999 then Continue;

            // Get color based on capacity
            ColorValue := GetColorForCurrentCapacity(CurrentCapacity, MinCapacity, MaxCapacity);

            // Set net color
            CurrentNet.Color := ColorValue;
            CurrentNet.OverrideColorForDraw := True;
        End;

        PCBServer.PostProcess;

        // Refresh display
        Client.SendMessage('PCB:Zoom', 'Action=Redraw', 255, Client.CurrentView);

        ShowMessage('Net coloring complete. Red = lower current capacity, Green = higher current capacity');

    Finally
        NetList.Free;
        NetMap.Free;
        CurrentNetCapacities.Free;
    End;
End;
