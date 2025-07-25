{..............................................................................}
{ Summary: Place all net labels with wires and test points on current sheet,   }
{ grouped by the sheet they originated from, sorted by complexity (least nets  }
{ first), with sheet name headers.                                             }
{..............................................................................}

// Simple bubble sort procedure to sort sheets by net count
Procedure SortSheetsByNetCount(SheetList: TStringList);
Var
    I, J: Integer;
    Count1, Count2: Integer;
    Pos1, Pos2: Integer;
    Temp: String;
    Swapped: Boolean;
Begin
    // Bubble sort - simple but reliable
    For I := 0 to SheetList.Count - 2 Do
    Begin
        Swapped := False;
        For J := 0 to SheetList.Count - 2 - I Do
        Begin
            // Extract net counts from "SheetName=NetCount" format
            Pos1 := Pos('=', SheetList[J]);
            Pos2 := Pos('=', SheetList[J + 1]);

            If (Pos1 > 0) And (Pos2 > 0) Then
            Begin
                Count1 := StrToIntDef(Copy(SheetList[J], Pos1 + 1, Length(SheetList[J]) - Pos1), 0);
                Count2 := StrToIntDef(Copy(SheetList[J + 1], Pos2 + 1, Length(SheetList[J + 1]) - Pos2), 0);

                // Swap if current count is greater than next count (ascending sort)
                If Count1 > Count2 Then
                Begin
                    Temp := SheetList[J];
                    SheetList[J] := SheetList[J + 1];
                    SheetList[J + 1] := Temp;
                    Swapped := True;
                End;
            End;
        End;

        // If no swapping occurred, list is sorted
        If Not Swapped Then Break;
    End;
End;

Procedure Run;
const
    Padding = 500; //mil
    YInc = 100;    //mil
    HeaderYInc = 150; //mil - extra space for sheet headers
    TP_PINLEN = 200; //mil
Var
    I, J         : Integer;
    Doc          : IDocument;
    CurrentSch   : ISch_Document;
    Project      : IProject;
    Net          : INet;
    NetName      : String;
    SheetName    : String;
    SchNetlabel  : ISch_Netlabel;
    SchWire      : ISch_Wire;
    SchLabel     : ISch_Label;
    Iterator     : ISch_Iterator;
    Component    : ISch_Component;
    SelectedTestPoint : ISch_Component;
    NewComponent : ISch_Component;
    PlacedNets   : TStringList;
    SheetNetCounts : TStringList;  // Will hold "SheetName=NetCount" entries
    SheetNetLists  : TStringList;  // Will hold all nets for each sheet
    CurrentSheetNets : TStringList; // Temp list for nets per sheet
    SheetWidth, SheetHeight : Integer;
    CurrentX, CurrentY : Integer;
    MaxY : Integer;
    XInc : Integer;
    NetCount     : Integer;
    SortedSheetName : String;
    SortedNetCount : Integer;
    EqualPos     : Integer;
Begin
    // Check if schematic server exists
    If SchServer = Nil Then Exit;

    // Get the focused project
    Project := GetWorkspace.DM_FocusedProject;
    If Project = Nil Then Exit;

    // Get the current schematic document where we'll place everything
    CurrentSch := SchServer.GetCurrentSchDocument;
    If CurrentSch = Nil Then Exit;

    // Find selected test point component from current sheet
    SelectedTestPoint := Nil;
    Iterator := CurrentSch.SchIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));
    Try
        Component := Iterator.FirstSchObject;
        While Component <> Nil Do
        Begin
            // Check if component is selected and is a test point (designator starts with TP)
            If Component.Selection And (Copy(Component.Designator.Text, 1, 2) = 'TP') Then
            Begin
                SelectedTestPoint := Component;
                Break;
            End;
            Component := Iterator.NextSchObject;
        End;
    Finally
        CurrentSch.SchIterator_Destroy(Iterator);
    End;

    // Check if test point was found and confirm with user
    If SelectedTestPoint = Nil Then
    Begin
        ShowMessage('Please select a test point (TP) component first.');
        Exit;
    End;

    If Not ConfirmNoYes('Place all nets grouped by sheet complexity using selected test point: ' + SelectedTestPoint.Designator.Text + '?') Then
        Exit;

    // Create string lists
    PlacedNets := TStringList.Create;
    PlacedNets.Duplicates := dupIgnore;
    PlacedNets.Sorted := True;

    SheetNetCounts := TStringList.Create;
    SheetNetCounts.Duplicates := dupIgnore;
    SheetNetCounts.Sorted := False; // We'll sort manually by net count

    SheetNetLists := TStringList.Create;
    SheetNetLists.Duplicates := dupIgnore;
    SheetNetLists.Sorted := False;

    CurrentSheetNets := TStringList.Create;
    CurrentSheetNets.Duplicates := dupIgnore;
    CurrentSheetNets.Sorted := True;

    Try
        // FIRST PASS: Count unique nets on each sheet
        ShowMessage('Counting nets on each sheet...');
        For I := 0 to Project.DM_PhysicalDocumentCount - 1 Do
        Begin
            Doc := Project.DM_PhysicalDocuments(I);

            // Check if this is a schematic document
            If Doc.DM_DocumentKind = 'SCH' Then
            Begin
                SheetName := Doc.DM_FileName;
                CurrentSheetNets.Clear;
                NetCount := 0;

                // Count unique nets on this sheet
                For J := 0 to Doc.DM_NetCount - 1 Do
                Begin
                    Net := Doc.DM_Nets(J);
                    NetName := Net.DM_NetName;

                    // Only count non-empty, unique net names
                    If (NetName <> '') And (CurrentSheetNets.IndexOf(NetName) = -1) Then
                    Begin
                        CurrentSheetNets.Add(NetName);
                        NetCount := NetCount + 1;
                    End;
                End;

                // Only add sheets that have nets (skip 0 net sheets)
                If NetCount > 0 Then
                Begin
                    SheetNetCounts.Add(SheetName + '=' + IntToStr(NetCount));

                    // Store all nets for this sheet for later use
                    For J := 0 to CurrentSheetNets.Count - 1 Do
                    Begin
                        SheetNetLists.Add(SheetName + '|' + CurrentSheetNets[J]);
                    End;
                End;
            End;
        End;

        // Sort sheets by net count (ascending - least nets first)
        SortSheetsByNetCount(SheetNetCounts);

        // Show sorted sheet information
        ShowMessage('Found ' + IntToStr(SheetNetCounts.Count) + ' sheets with nets. Processing in order of complexity (least nets first)...');

        // Get sheet dimensions and calculate usable area with padding
        SheetWidth := CoordToMils(CurrentSch.SheetSizeX);
        SheetHeight := CoordToMils(CurrentSch.SheetSizeY);

        // Calculate layout parameters
        CurrentX := Padding;  // Starting X position
        CurrentY := Padding;  // Starting Y position
        MaxY := SheetHeight - Padding;  // Maximum Y position
        XInc := 1200;  // Fixed spacing: 300 (pin) + 700 (wire) + 200 (padding)

        // SECOND PASS: Place objects on current sheet, processing sheets by complexity
        SchServer.ProcessControl.PreProcess(CurrentSch, '');
        Try
            // Process each sheet in sorted order (least nets first)
            For I := 0 to SheetNetCounts.Count - 1 Do
            Begin
                // Extract sheet name and net count
                EqualPos := Pos('=', SheetNetCounts[I]);
                SortedSheetName := Copy(SheetNetCounts[I], 1, EqualPos - 1);
                SortedNetCount := StrToIntDef(Copy(SheetNetCounts[I], EqualPos + 1, Length(SheetNetCounts[I]) - EqualPos), 0);

                // Count how many nets we'll actually place for this sheet
                NetCount := 0;
                For J := 0 to SheetNetLists.Count - 1 Do
                Begin
                    If Copy(SheetNetLists[J], 1, Length(SortedSheetName) + 1) = SortedSheetName + '|' Then
                    Begin
                        NetName := Copy(SheetNetLists[J], Length(SortedSheetName) + 2, Length(SheetNetLists[J]) - Length(SortedSheetName) - 1);
                        If PlacedNets.IndexOf(NetName) = -1 Then
                            NetCount := NetCount + 1;
                    End;
                End;

                // Skip sheets where all nets are already placed
                If NetCount = 0 Then Continue;

                // Create header label for this sheet
                SchLabel := SchServer.SchObjectFactory(eLabel, eCreate_GlobalCopy);
                If SchLabel <> Nil Then
                Begin
                    // Set label properties - show nets being placed vs total nets on sheet
                    SchLabel.Location := Point(MilsToCoord(CurrentX), MilsToCoord(CurrentY));
                    SchLabel.Text := 'Sheet: ' + SortedSheetName + ' (placing ' + IntToStr(NetCount) + ' of ' + IntToStr(SortedNetCount) + ' nets)';
                    SchLabel.Orientation := eRotate0;
                    SchLabel.FontId := SchServer.FontManager.GetFontID(14, 90, False, False, False, False, 'Arial');

                    // Add the label to the current schematic document
                    CurrentSch.RegisterSchObjectInContainer(SchLabel);
                End;

                // Move down for the nets under this sheet
                CurrentY := CurrentY + HeaderYInc;

                // Process nets for this sheet (find them in SheetNetLists)
                For J := 0 to SheetNetLists.Count - 1 Do
                Begin
                    // Check if this net belongs to the current sheet
                    If Copy(SheetNetLists[J], 1, Length(SortedSheetName) + 1) = SortedSheetName + '|' Then
                    Begin
                        // Extract net name
                        NetName := Copy(SheetNetLists[J], Length(SortedSheetName) + 2, Length(SheetNetLists[J]) - Length(SortedSheetName) - 1);

                        // Only place if not already placed
                        If PlacedNets.IndexOf(NetName) = -1 Then
                        Begin
                            // Create a new net label - positioned at end of test point pin
                            SchNetlabel := SchServer.SchObjectFactory(eNetlabel, eCreate_GlobalCopy);
                            If SchNetlabel <> Nil Then
                            Begin
                                SchNetlabel.Location    := Point(MilsToCoord(CurrentX + 300), MilsToCoord(CurrentY));
                                SchNetlabel.Orientation := eRotate0;
                                SchNetlabel.Text        := NetName;
                                CurrentSch.RegisterSchObjectInContainer(SchNetlabel);
                            End;

                            // Create a wire - 700 mil long starting from end of test point pin
                            SchWire := SchServer.SchObjectFactory(eWire, eCreate_GlobalCopy);
                            If SchWire <> Nil Then
                            Begin
                                SchWire.Location := Point(MilsToCoord(CurrentX - 100 + 300), MilsToCoord(CurrentY));
                                SchWire.InsertVertex := 1;
                                SchWire.SetState_Vertex(1, Point(MilsToCoord(CurrentX - 100 + 300), MilsToCoord(CurrentY)));
                                SchWire.InsertVertex := 2;
                                SchWire.SetState_Vertex(2, Point(MilsToCoord(CurrentX - 100 + 300 + 700), MilsToCoord(CurrentY)));
                                SchWire.SetState_LineWidth := eSmall;
                                CurrentSch.RegisterSchObjectInContainer(SchWire);
                            End;

                            // Replicate the selected test point at the start of the wire
                            NewComponent := SelectedTestPoint.Replicate;
                            If NewComponent <> Nil Then
                            Begin
                                NewComponent.Designator.Text := 'TP?';
                                NewComponent.MoveToXY(MilsToCoord(CurrentX - 100), MilsToCoord(CurrentY));
                                CurrentSch.RegisterSchObjectInContainer(NewComponent);
                            End;

                            // Mark this net as placed
                            PlacedNets.Add(NetName);

                            // Calculate next position (move down by YInc mils)
                            CurrentY := CurrentY + YInc;

                            // Check if we've reached the bottom padding, move to next column
                            If CurrentY > MaxY Then
                            Begin
                                CurrentX := CurrentX + XInc;  // Move right using fixed spacing
                                CurrentY := Padding;          // Reset to top position
                            End;
                        End;
                    End;
                End;

                // Add extra space after each sheet group
                CurrentY := CurrentY + HeaderYInc;

                // Check if we need to move to next column after sheet group
                If CurrentY > MaxY Then
                Begin
                    CurrentX := CurrentX + XInc;
                    CurrentY := Padding;
                End;
            End;

            // Refresh the schematic display
            CurrentSch.GraphicallyInvalidate;

        Finally
            // Clean up the robots in Schematic editor
            SchServer.ProcessControl.PostProcess(CurrentSch, '');
        End;

        // Show completion message
        ShowMessage('Placement completed!' + #13 +
                   'Total nets placed: ' + IntToStr(PlacedNets.Count) + #13 +
                   'Sheets processed in order of complexity (least nets first).');

    Finally
        // Clean up
        PlacedNets.Free;
        SheetNetCounts.Free;
        SheetNetLists.Free;
        CurrentSheetNets.Free;
    End;
End;

{..............................................................................}
