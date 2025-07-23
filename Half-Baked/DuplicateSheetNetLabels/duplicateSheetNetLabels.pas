{..............................................................................}
{ Summary: Create net labels OR ports for all unique nets from ALL schematic  }
{ sheets in the project. Objects placed on current sheet in column/row layout }
{ with 200 mil padding and intelligent wrapping to fit within sheet bounds.   }
{..............................................................................}

Procedure Run;
const
    Padding = 500; //mil
    XInc = 2000;   //mil
    YInc = 100;    //mil
Var
    I, J         : Integer;
    Doc          : IDocument;
    CurrentSch   : ISch_Document;
    Project      : IProject;
    Net          : INet;
    NetName      : String;
    SchNetlabel  : ISch_Netlabel;
    SchPort      : ISch_Port;
    UniqueNetNames : TStringList;
    SheetWidth, SheetHeight : Integer;
    CurrentX, CurrentY : Integer;
    MaxY : Integer;
    mode         : String;
    PlacePorts   : Boolean;
Begin
    // Check if schematic server exists
    If SchServer = Nil Then Exit;

    // Prompt user to select mode
    mode := InputBox('Net/Port Placer', 'Select object type (Net = Net Labels, Port = Ports):', 'Net');
    
    // Determine if we should place ports or net labels
    PlacePorts := (UpperCase(mode) = 'PORT') or (UpperCase(mode) = 'PORTS');

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
                    UniqueNetNames.Add(NetName);
            End;
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
            SchPort.Width     := MilsToCoord(1000);
            SchPort.AreaColor := $80ffff;
            SchPort.TextColor := $00080;
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
        End;

        // Calculate next position (move down by YInc mils)
        CurrentY := CurrentY + YInc;

        // Check if we've reached the bottom padding, move to next column
        If CurrentY > MaxY Then
        Begin
            CurrentX := CurrentX + XInc;  // Move right by XInc mils
            CurrentY := Padding;          // Reset to top position
        End;
    End;

    // Clean up
    UniqueNetNames.Free;

    // Refresh the schematic display
    CurrentSch.GraphicallyInvalidate;
End;

{..............................................................................}
