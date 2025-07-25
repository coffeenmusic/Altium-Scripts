{..............................................................................}
{ Summary: First collect all nets from all sheets, then iterate each sheet     }
{ and add test points with net labels ONLY for nets that actually exist on    }
{ that specific sheet. Each net is only added once across all sheets.         }
{..............................................................................}

Procedure Run;
const
    Padding = 500; //mil
    YInc = 100;    //mil
    TP_PINLEN = 200; //mil
Var
    I, J         : Integer;
    Doc          : IDocument;
    CurrentSch   : ISch_Document;
    Project      : IProject;
    Net          : INet;
    NetName      : String;
    SchNetlabel  : ISch_Netlabel;
    SchWire      : ISch_Wire;
    Iterator     : ISch_Iterator;
    Component    : ISch_Component;
    SelectedTestPoint : ISch_Component;
    NewComponent : ISch_Component;
    AllProjectNets : TStringList;
    RemainingNets  : TStringList;
    CurrentSheetNets : TStringList;
    SheetWidth, SheetHeight : Integer;
    CurrentX, CurrentY : Integer;
    MaxY : Integer;
    XInc : Integer;
    NetIndex       : Integer;
    OriginalCurrentSch : ISch_Document;
Begin
    // Check if schematic server exists
    If SchServer = Nil Then Exit;

    // Get the focused project
    Project := GetWorkspace.DM_FocusedProject;
    If Project = Nil Then Exit;

    // Get the current schematic document to find selected test point
    OriginalCurrentSch := SchServer.GetCurrentSchDocument;
    If OriginalCurrentSch = Nil Then Exit;

    // Find selected test point component from current sheet
    SelectedTestPoint := Nil;
    Iterator := OriginalCurrentSch.SchIterator_Create;
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
        OriginalCurrentSch.SchIterator_Destroy(Iterator);
    End;

    // Check if test point was found and confirm with user
    If SelectedTestPoint = Nil Then
    Begin
        ShowMessage('Please select a test point (TP) component first.');
        Exit;
    End;

    If Not ConfirmNoYes('Add test points and net labels using selected test point: ' + SelectedTestPoint.Designator.Text + '?') Then
        Exit;

    // Create string lists
    AllProjectNets := TStringList.Create;
    AllProjectNets.Duplicates := dupIgnore;
    AllProjectNets.Sorted := True;

    RemainingNets := TStringList.Create;
    RemainingNets.Duplicates := dupIgnore;
    RemainingNets.Sorted := True;

    CurrentSheetNets := TStringList.Create;
    CurrentSheetNets.Duplicates := dupIgnore;
    CurrentSheetNets.Sorted := True;

    Try
        // FIRST PASS: Collect ALL nets from ALL schematic sheets in the project
        ShowMessage('Collecting all nets from project...');
        For I := 0 to Project.DM_PhysicalDocumentCount - 1 Do
        Begin
            Doc := Project.DM_PhysicalDocuments(I);

            // Check if this is a schematic document
            If Doc.DM_DocumentKind = 'SCH' Then
            Begin
                // Collect all nets from this schematic sheet
                For J := 0 to Doc.DM_NetCount - 1 Do
                Begin
                    Net := Doc.DM_Nets(J);
                    NetName := Net.DM_NetName;

                    // Skip empty net names
                    If NetName <> '' Then
                    Begin
                        AllProjectNets.Add(NetName);
                    End;
                End;
            End;
        End;

        // Copy all nets to remaining nets list
        RemainingNets.Assign(AllProjectNets);
        ISch_
        ShowMessage('Total unique nets found: ' + IntToStr(AllProjectNets.Count) + '. Starting placement...');

        // SECOND PASS: Iterate through all schematic sheets and place test points
        For I := 0 to Project.DM_PhysicalDocumentCount - 1 Do
        Begin
            Doc := Project.DM_PhysicalDocuments(I);


            // Check if this is a schematic document
            If Doc.DM_DocumentKind = 'SCH' Then
            Begin
                // Load and get the schematic document
                CurrentSch := SchServer.LoadSchDocumentByPath(Doc.DM_FullPath);

                If CurrentSch = Nil Then Continue;

                // Skip if no more nets to place
                If RemainingNets.Count = 0 Then Break;

                // Clear the current sheet nets list
                CurrentSheetNets.Clear;

                // Get nets that actually exist on THIS specific sheet
                For J := 0 to Doc.DM_NetCount - 1 Do
                Begin
                    Net := Doc.DM_Nets(J);
                    NetName := Net.DM_NetName;

                    // Only add nets that:
                    // 1. Have a valid name
                    // 2. Are still in our remaining nets list (haven't been placed yet)
                    // 3. Actually exist on this specific sheet
                    If (NetName <> '') And (RemainingNets.IndexOf(NetName) >= 0) Then
                    Begin
                        CurrentSheetNets.Add(NetName);
                    End;
                End;

                // Skip this sheet if no remaining nets exist on it
                If CurrentSheetNets.Count = 0 Then
                Begin
                    ShowMessage('Sheet ' + CurrentSch.DocumentName + ' has no remaining nets to place.');
                    Continue;
                End;

                // Get sheet dimensions and calculate usable area with padding
                SheetWidth := CoordToMils(CurrentSch.SheetSizeX);
                SheetHeight := CoordToMils(CurrentSch.SheetSizeY);

                // Calculate layout parameters for this sheet
                CurrentX := Padding;  // Starting X position
                CurrentY := Padding;  // Starting Y position
                MaxY := SheetHeight - Padding;  // Maximum Y position

                // Calculate dynamic X spacing based on fixed wire length
                // Total width = test point (at CurrentX-100) + pin (300) + wire (700) + padding
                XInc := 1200;  // 300 (pin) + 700 (wire) + 200 (padding)

                // Add test points and net labels for each net that exists on this sheet
                SchServer.ProcessControl.PreProcess(CurrentSch, '');
                Try
                    For J := 0 to CurrentSheetNets.Count - 1 Do
                    Begin
                        NetName := CurrentSheetNets[J];

                        // Create a new net label - shifted over by test point pin length
                        SchNetlabel := SchServer.SchObjectFactory(eNetlabel, eCreate_GlobalCopy);
                        If SchNetlabel <> Nil Then
                        Begin
                            // Set net label properties - positioned at end of test point pin
                            SchNetlabel.Location    := Point(MilsToCoord(CurrentX + 300), MilsToCoord(CurrentY));
                            SchNetlabel.Orientation := eRotate0;
                            SchNetlabel.Text        := NetName;

                            // Add the net label to the current schematic document
                            CurrentSch.RegisterSchObjectInContainer(SchNetlabel);
                        End;

                        // Create a wire under the net label starting from the end of the test point pin - 700 mil long
                        SchWire := SchServer.SchObjectFactory(eWire, eCreate_GlobalCopy);
                        If SchWire <> Nil Then
                        Begin
                            // Set wire starting point at the end of the test point pin (300 mil to the right of test point)
                            SchWire.Location := Point(MilsToCoord(CurrentX - 100 + 300), MilsToCoord(CurrentY));
                            SchWire.InsertVertex := 1;
                            SchWire.SetState_Vertex(1, Point(MilsToCoord(CurrentX - 100 + 300), MilsToCoord(CurrentY)));

                            // Set wire ending point 700 mil to the right of start point
                            SchWire.InsertVertex := 2;
                            SchWire.SetState_Vertex(2, Point(MilsToCoord(CurrentX - 100 + 300 + 700), MilsToCoord(CurrentY)));

                            // Set wire properties
                            SchWire.SetState_LineWidth := eSmall;

                            // Add the wire to the current schematic document
                            CurrentSch.RegisterSchObjectInContainer(SchWire);
                        End;

                        // Replicate the selected test point at the start of the wire (left side)
                        NewComponent := SelectedTestPoint.Replicate;
                        If NewComponent <> Nil Then
                        Begin
                            NewComponent.Designator.Text := 'TP?';
                            // Position test point at the start of the wire (left side)
                            NewComponent.MoveToXY(MilsToCoord(CurrentX - 100), MilsToCoord(CurrentY));

                            // Add the test point to the current schematic document
                            CurrentSch.RegisterSchObjectInContainer(NewComponent);
                        End;

                        // Remove this net from the remaining nets list since it's now placed
                        NetIndex := RemainingNets.IndexOf(NetName);
                        If NetIndex >= 0 Then
                            RemainingNets.Delete(NetIndex);

                        // Calculate next position (move down by YInc mils)
                        CurrentY := CurrentY + YInc;

                        // Check if we've reached the bottom padding, move to next column
                        If CurrentY > MaxY Then
                        Begin
                            CurrentX := CurrentX + XInc;  // Move right using fixed spacing
                            CurrentY := Padding;          // Reset to top position
                        End;
                    End;

                    // Refresh the schematic display for this sheet
                    CurrentSch.GraphicallyInvalidate;

                Finally
                    // Clean up the robots in Schematic editor
                    SchServer.ProcessControl.PostProcess(CurrentSch, '');
                End;

                // Show progress message
                ShowMessage('Added ' + IntToStr(CurrentSheetNets.Count) + ' test points to sheet: ' + CurrentSch.DocumentName +
                           '. Remaining nets: ' + IntToStr(RemainingNets.Count));
            End;
        End;

        // Show completion message
        ShowMessage('Process completed!' + #13 +
                   'Total unique nets found: ' + IntToStr(AllProjectNets.Count) + #13 +
                   'Total nets placed: ' + IntToStr(AllProjectNets.Count - RemainingNets.Count) + #13 +
                   'Remaining unplaced: ' + IntToStr(RemainingNets.Count));

    Finally
        // Clean up
        AllProjectNets.Free;
        RemainingNets.Free;
        CurrentSheetNets.Free;
    End;
End;

{..............................................................................}
