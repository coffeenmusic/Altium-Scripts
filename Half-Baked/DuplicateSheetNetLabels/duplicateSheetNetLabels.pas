{..............................................................................}
{ Summary: Create net labels for all unique nets from ALL schematic sheets     }
{ in the project. Net labels placed on current sheet in column/row layout     }
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
    UniqueNetNames : TStringList;
    SheetWidth, SheetHeight : Integer;
    CurrentX, CurrentY : Integer;
    MaxY : Integer;
Begin
    // Check if schematic server exists
    If SchServer = Nil Then Exit;

    // Get the focused project
    Project := GetWorkspace.DM_FocusedProject;
    If Project = Nil Then Exit;

    // Get the current schematic document for placing net labels
    CurrentSch := SchServer.GetCurrentSchDocument;
    If CurrentSch = Nil Then Exit;

    // Get sheet dimensions and calculate usable area with 200 mil padding
    SheetWidth := CoordToMils(CurrentSch.SheetSizeX);
    SheetHeight := CoordToMils(CurrentSch.SheetSizeY);

    // Calculate layout parameters
    CurrentX := Padding;  // Starting X position (200 mil padding)
    CurrentY := Padding;  // Starting Y position (200 mil padding)
    MaxY := SheetHeight - Padding;  // Maximum Y position (200 mil padding from bottom)

    // Get the current schematic document for placing net labels
    CurrentSch := SchServer.GetCurrentSchDocument;
    If CurrentSch = Nil Then Exit;

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

    // Create net labels for each unique net name using column/row layout
    For I := 0 to UniqueNetNames.Count - 1 Do
    Begin
        NetName := UniqueNetNames[I];

        // Create a new net label
        SchNetlabel := SchServer.SchObjectFactory(eNetlabel, eCreate_GlobalCopy);
        If SchNetlabel = Nil Then Continue;

        // Set net label properties at current position
        SchNetlabel.Location    := Point(MilsToCoord(CurrentX), MilsToCoord(CurrentY));
        SchNetlabel.Orientation := eRotate0;
        SchNetlabel.Text        := NetName;
        SchNetlabel.Selection   := True;  // Select the net label

        // Add the net label to the current schematic document
        CurrentSch.RegisterSchObjectInContainer(SchNetlabel);

        // Calculate next position (move down by 100 mils)
        CurrentY := CurrentY + YInc;

        // Check if we've reached the bottom padding, move to next column
        If CurrentY > MaxY Then
        Begin
            CurrentX := CurrentX + XInc;  // Move right by 1000 mils
            CurrentY := Padding;              // Reset to top position
        End;
    End;

    // Clean up
    UniqueNetNames.Free;

    // Refresh the schematic display
    CurrentSch.GraphicallyInvalidate;
End;

{..............................................................................}
