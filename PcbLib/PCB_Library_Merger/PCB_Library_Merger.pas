{..............................................................................}
{ Summary Consolidates footprints from multiple PCB libraries into a single library }
{                                                                                    }
{ This script takes a directory path with multiple PCB libraries (.PcbLib)           }
{ and copies all footprints from those libraries into a single destination library   }
{..............................................................................}

{..............................................................................}
Var
    DestinationLib : IPCB_Library;
{..............................................................................}

{..............................................................................}
Procedure ConsolidateFootprintLibraries;
Var
    SourcePath      : TString;
    DestinationPath : TString;
    FileList        : TStringList;
    SourceLib       : IPCB_Library;
    Iterator        : IPCB_LibraryIterator;
    Footprint       : IPCB_LibComponent;
    NewFootprint    : IPCB_LibComponent;
    ChildIterator   : IPCB_GroupIterator;
    ChildObject     : IPCB_Primitive;
    NewChild        : IPCB_Primitive;
    I               : Integer;
    FileExt         : TString;
    MsgReply        : Integer;
    LibPathStr      : TString;

    OpenDialog      : TOpenDialog;
    SaveDialog      : TSaveDialog;
Begin
    // Check if we have access to PCB Server
    If PCBServer = Nil Then
    Begin
        ShowMessage('PCB Server is not available.');
        Exit;
    End;

    // Get source directory path from user using dialog
    SourcePath := '';
    OpenDialog := TOpenDialog.Create(nil);
    Try
        OpenDialog.Title := 'Select Directory with PCB Libraries';
        Include(OpenDialog.Options, ofNoValidate);
        Include(OpenDialog.Options, ofPathMustExist);
        Include(OpenDialog.Options, ofEnableSizing);
        OpenDialog.FileName := 'Select folder';
        OpenDialog.Filter := 'Folders|*.';

        If OpenDialog.Execute Then
            SourcePath := ExtractFilePath(OpenDialog.FileName)
        Else
            Exit;
    Finally
        OpenDialog.Free;
    End;

    // Check if we have a current PCB Library open
    DestinationLib := PCBServer.GetCurrentPCBLibrary;
    If DestinationLib = Nil Then
    Begin
        // No library currently open, prompt for destination
        DestinationPath := '';
        SaveDialog := TSaveDialog.Create(nil);
        Try
            SaveDialog.Title := 'Select Destination Library';
            SaveDialog.Filter := 'PCB Libraries (*.PcbLib)|*.PcbLib';
            SaveDialog.DefaultExt := 'PcbLib';
            SaveDialog.InitialDir := SourcePath;
            SaveDialog.FileName := 'Consolidated.PcbLib';

            If SaveDialog.Execute Then
                DestinationPath := SaveDialog.FileName
            Else
                Exit;
        Finally
            SaveDialog.Free;
        End;

        // Create a new PCB Library
        DestinationLib := PcbServer.CreatePCBLibrary;
        DestinationLib.Board.FileName := DestinationPath;
    End
    Else
    Begin
        // We already have an open library, use it as destination
        ShowMessage('Using currently open library "' + ExtractFileName(DestinationLib.Board.FileName) + '" as destination.');
    End;

    // Create a new PCB Library or open existing one
    DestinationLib := PCBServer.GetCurrentPCBLibrary;

    // Get all PCB Library files from the source directory
    FileList := TStringList.Create;
    Try
        // Find all PCB Library files in the source directory
        FindFiles(SourcePath, '*.PcbLib', faAnyFile, False, FileList);

        If FileList.Count = 0 Then
        Begin
            ShowMessage('No PCB Library files found in the specified directory.');
            Exit;
        End;

        // Process each PCB Library file
        For I := 0 To FileList.Count - 1 Do
        Begin
            LibPathStr := FileList[I];



            // Open the source library
            SourceLib := PcbServer.LoadPCBLibraryByPath(LibPathStr);
            If SourceLib = Nil Then Continue;

            Try
                // Create an iterator for the source library
                Iterator := SourceLib.LibraryIterator_Create;
                Iterator.SetState_FilterAll;

                // Iterate through each footprint in the source library
                Footprint := Iterator.FirstPCBObject;
                While Footprint <> Nil Do
                Begin
                    PCBServer.PreProcess;

                    // Register the new footprint in the destination library
                    DestinationLib.RegisterComponent(Footprint);

                    // Register the new footprint with the board
                    PCBServer.SendMessageToRobots(DestinationLib.Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Footprint.I_ObjectAddress);

                    PCBServer.PostProcess;

                    // Move to the next footprint in the source library
                    Footprint := Iterator.NextPCBObject;
                End;         

                // Clean up the source library iterator
                SourceLib.LibraryIterator_Destroy(Iterator);
            Finally
                // Close the source library (optional)
                Client.CloseDocument(SourceLib);
            End;
        End;

        // Save the destination library
        DestinationLib.Board.ViewManager_FullUpdate;
        //DestinationLib.Board.SaveToFile(DestinationLib.Board.FileName);

        ShowMessage('Consolidation completed. ' + IntToStr(DestinationLib.ComponentCount) + ' footprints consolidated into ' + DestinationLib.Board.FileName);
    Finally
        FileList.Free;
    End;
End;
{..............................................................................}

{..............................................................................}
End.
