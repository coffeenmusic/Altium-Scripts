{..............................................................................}
{ Summary: Create net labels OR ports for all unique nets from ALL schematic  }
{ sheets in the project. Objects placed on current sheet in column/row layout }
{ with 200 mil padding and intelligent wrapping to fit within sheet bounds.   }
{..............................................................................}

Procedure Run;
const
    Padding = 500; //mil
    YInc = 100;    //mil
    TP_PINLEN = 300; // Test Point Pin Length
Var
    I, J         : Integer;
    Doc          : IDocument;
    CurrentSch   : ISch_Document;
    Project      : IProject;
    Net          : INet;
    NetName      : String;
    SchNetlabel  : ISch_Netlabel;
    SchPort      : ISch_Port;
    SchWire      : ISch_Wire;
    Iterator     : ISch_Iterator;
    Component    : ISch_Component;
    SelectedTestPoint : ISch_Component;
    NewComponent : ISch_Component;
    UniqueNetNames : TStringList;
    SheetWidth, SheetHeight : Integer;
    CurrentX, CurrentY : Integer;
    MaxY : Integer;
    XInc : Integer;
    MaxNetNameLength : Integer;
    MaxNetNameWidth : Integer;
    mode         : String;
    PortWidthStr : String;
    PortWidth    : Integer;
    PlacePorts   : Boolean;
Begin
    // Check if schematic server exists
    If SchServer = Nil Then Exit;

    // Prompt user to select mode
    mode := InputBox('Net/Port Placer', 'Select object type (Net = Net Labels, Port = Ports):', 'Net');

    // Determine if we should place ports or net labels
    PlacePorts := (UpperCase(mode) = 'PORT') or (UpperCase(mode) = 'PORTS');

    // If placing ports, prompt for port width
    If PlacePorts Then
    Begin
        PortWidthStr := InputBox('Port Width', 'Enter port width in mils:', '1000');
        PortWidth := StrToIntDef(PortWidthStr, 1000);  // Default to 1000 if invalid input
    End;

    // Get the focused project
    Project := GetWorkspace.DM_FocusedProject;
    If Project = Nil Then Exit;

    // Get the current schematic document for placing objects
    CurrentSch := SchServer.GetCurrentSchDocument;
    If CurrentSch = Nil Then Exit;

    // Get sheet dimensions and calculate usable area with padding
    SheetWidth := CoordToMils(CurrentSch.SheetSizeX);
    SheetHeight := CoordToMils(CurrentSch.SheetSizeY);

    // Calculate layout parameters
    CurrentX := Padding;  // Starting X position
    CurrentY := Padding;  // Starting Y position
    MaxY := SheetHeight - Padding;  // Maximum Y position

    // Create string list to track unique net names from all schematic sheets
    UniqueNetNames := TStringList.Create;
    UniqueNetNames.Duplicates := dupIgnore;
    UniqueNetNames.Sorted := True;

    // Initialize maximum net name length tracker
    MaxNetNameLength := 0;

    // Iterate through all physical documents in the project to collect nets
    For I := 0 to Project.DM_PhysicalDocumentCount - 1 Do
    Begin
        Doc := Project.DM_PhysicalDocuments(I);

        // Check if this is a schematic document
        If Doc.DM_DocumentKind = 'SCH' Then
        Begin
            // Collect nets from this schematic sheet
            For J := 0 to Doc.DM_NetCount - 1 Do
            Begin
                Net := Doc.DM_Nets(J);
                NetName := Net.DM_NetName;

                // Skip empty net names
                If NetName <> '' Then
                Begin
                    UniqueNetNames.Add(NetName);
                    // Track maximum net name length
                    If Length(NetName) > MaxNetNameLength Then
                        MaxNetNameLength := Length(NetName);
                End;
            End;
        End;
    End;

    // Calculate dynamic X spacing based on maximum net name length
    // Formula: (MaxLength * 50 mil per character, rounded up to nearest 100 mil) + 200 mil margin
    MaxNetNameWidth := ((MaxNetNameLength * 50 + 99) div 100) * 100;
    XInc := MaxNetNameWidth + 200;

    // Find selected test point component if placing net labels
    SelectedTestPoint := Nil;
    If Not PlacePorts Then
    Begin
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

        // If test point found, ask user for confirmation
        If SelectedTestPoint <> Nil Then
        Begin
            If Not ConfirmNoYes('Would you like to add the selected test point to all nets?') Then
                SelectedTestPoint := Nil;
        End;
    End;

    // Create objects for each unique net name using column/row layout
    For I := 0 to UniqueNetNames.Count - 1 Do
    Begin
        NetName := UniqueNetNames[I];

        If PlacePorts Then
        Begin
            // Create a new port object
            SchPort := SchServer.SchObjectFactory(ePort, eCreate_GlobalCopy);
            If SchPort = Nil Then Continue;

            // Set port properties
            SchPort.Location  := Point(MilsToCoord(CurrentX), MilsToCoord(CurrentY));
            SchPort.Style     := ePortRight;
            SchPort.IOType    := ePortBidirectional;
            SchPort.Alignment := eHorizontalCentreAlign;
            SchPort.Width     := MilsToCoord(PortWidth);
            SchPort.AreaColor := 0;
            SchPort.TextColor := $FFFFFF;
            SchPort.Name      := NetName;

            // Add the port to the current schematic document
            CurrentSch.RegisterSchObjectInContainer(SchPort);
        End
        Else
        Begin
            // Create a new net label
            SchNetlabel := SchServer.SchObjectFactory(eNetlabel, eCreate_GlobalCopy);
            If SchNetlabel = Nil Then Continue;

            // Set net label properties
            SchNetlabel.Location    := Point(MilsToCoord(CurrentX), MilsToCoord(CurrentY));
            SchNetlabel.Orientation := eRotate0;
            SchNetlabel.Text        := NetName;
            SchNetlabel.Selection   := True;  // Select the net label

            // Add the net label to the current schematic document
            CurrentSch.RegisterSchObjectInContainer(SchNetlabel);

            // Create a wire under the net label starting 100 mil to the left
            SchWire := SchServer.SchObjectFactory(eWire, eCreate_GlobalCopy);
            If SchWire <> Nil Then
            Begin
                // Set wire starting point 100 mil to the left of net label
                SchWire.Location := Point(MilsToCoord(CurrentX - 100), MilsToCoord(CurrentY));
                SchWire.InsertVertex := 1;
                SchWire.SetState_Vertex(1, Point(MilsToCoord(CurrentX - 100), MilsToCoord(CurrentY)));

                // Set wire ending point extending right by the calculated max net name width
                SchWire.InsertVertex := 2;
                SchWire.SetState_Vertex(2, Point(MilsToCoord(CurrentX + MaxNetNameWidth), MilsToCoord(CurrentY)));

                // Set wire properties
                SchWire.SetState_LineWidth := eSmall;

                // Add the wire to the current schematic document
                CurrentSch.RegisterSchObjectInContainer(SchWire);
            End;

            // If test point is selected, replicate it at the start of the wire (left side)
            If SelectedTestPoint <> Nil Then
            Begin
                NewComponent := SelectedTestPoint.Replicate;
                If NewComponent <> Nil Then
                Begin
                    // Position test point at the start of the wire (left side)
                    NewComponent.MoveToXY(MilsToCoord(CurrentX - 100 - TP_PINLEN), MilsToCoord(CurrentY));
                    NewComponent.Designator.Text := 'TP?';

                    // Add the test point to the current schematic document
                    CurrentSch.RegisterSchObjectInContainer(NewComponent);
                End;
            End;
        End;

        // Calculate next position (move down by YInc mils)
        CurrentY := CurrentY + YInc;

        // Check if we've reached the bottom padding, move to next column
        If CurrentY > MaxY Then
        Begin
            If SelectedTestPoint <> Nil Then
            Begin
                 CurrentX := CurrentX + XInc + TP_PINLEN + 100;  // Move right by XInc mils
            End
            Else
            Begin
                 CurrentX := CurrentX + XInc;  // Move right by XInc mils
            End;

            CurrentY := Padding;          // Reset to top position
        End;
    End;

    // Clean up
    UniqueNetNames.Free;

    // Refresh the schematic display
    CurrentSch.GraphicallyInvalidate;
End;

{..............................................................................}
