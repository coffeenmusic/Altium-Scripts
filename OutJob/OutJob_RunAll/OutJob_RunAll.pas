// Made by Stephen Thompson
// References: Jeff Collins has an XIA_Release_Manager.pas script that taught me the art of the Output Job
//                 See his post on the Altium Forums: https://forum.live.altium.com/#/posts/189423

Const
    DEFAULT = 'Blank';
Var
    IniFile: TIniFile;

Function ReadIni(Section: String, Parameter: String, ID: Integer, Suffix: String = ''): String;
Var
    Identifier: String;
Begin
    Identifier := Parameter + IntToStr(ID) + Suffix;
    result := IniFile.ReadString(Section, Identifier, DEFAULT);
End;

Function ReadPdfSetting(Parameter: String, ID: Integer, DefaultValue: String = ''): String;
Begin
    Result := ReadIni('PublishSettings', Parameter, ID);
    If Result = DEFAULT Then  // If not found in INI
        Result := DefaultValue;
End;

Function RunInit(ContainerName: String, OutputPath: String, Action: String = 'Run');
Begin
    // Gerber
    ResetParameters;
    AddStringParameter ('Action'        , Action);
    AddStringParameter ('OutputMedium'  , ContainerName);
    AddStringParameter ('ObjectKind'    , 'OutputBatch');
    AddStringParameter ('OutputBasePath', OutputPath); // Or alternatively OutputFilePath
End;

Function RunGenerateFiles(ContainerName: String, OutputPath: String);
Begin
    RunInit(ContainerName, OutputPath);
    RunProcess('WorkspaceManager:GenerateReport');  // Output Generation
End;

Function RunPublishToPDF(ContainerName: String, J: Integer, OutputPath: String, Action: String = 'PublishToPDF');
    // J = Job/ContainerNumber
Begin
    RunInit(ContainerName, OutputPath, Action);
    AddIntegerParameter('ReleaseManaged',          StrToInt(ReadPdfSetting('ReleaseManaged', J, '0')));
    AddStringParameter ('OutputPathMedia',         ReadPdfSetting('OutputPathMedia', J, ''));
    AddStringParameter ('OutputPathOutputer',      ReadPdfSetting('OutputPathOutputer', J, ''));
    AddStringParameter ('OutputFileName',          ReadPdfSetting('OutputFileName', J, ''));
    AddStringParameter ('OpenOutput',              ReadPdfSetting('OpenOutput', J, 'False'));
    AddStringParameter ('PromptOverwrite',         ReadPdfSetting('PromptOverwrite', J, 'False'));
    AddIntegerParameter('PublishMethod',           StrToInt(ReadPdfSetting('PublishMethod', J, '0')));
    AddIntegerParameter('ZoomLevel',               StrToInt(ReadPdfSetting('ZoomLevel', J, '90')));
    AddStringParameter ('FitSCHPrintSizeToDoc',    ReadPdfSetting('FitSCHPrintSizeToDoc', J, 'False'));
    AddStringParameter ('FitPCBPrintSizeToDoc',    ReadPdfSetting('FitPCBPrintSizeToDoc', J, 'False'));
    AddStringParameter ('GenerateNetsInfo',        ReadPdfSetting('GenerateNetsInfo', J, 'True'));
    AddStringParameter ('MarkPins',                ReadPdfSetting('MarkPins', J, 'True'));
    AddStringParameter ('MarkNetLabels',           ReadPdfSetting('MarkNetLabels', J, 'True'));
    AddStringParameter ('MarkPortsId',             ReadPdfSetting('MarkPortsId', J, 'True'));
    AddStringParameter ('GenerateTOC',             ReadPdfSetting('GenerateTOC', J, 'True'));
    AddStringParameter ('ShowComponentParameters', ReadPdfSetting('ShowComponentParameters', J, 'True'));
    AddStringParameter ('GlobalBookmarks',         ReadPdfSetting('GlobalBookmarks', J, '0'));
    AddStringParameter ('PDFACompliance',          ReadPdfSetting('PDFACompliance', J, 'Disabled'));
    AddStringParameter ('PDFVersion',              ReadPdfSetting('PDFVersion', J, 'Default'));
    AddStringParameter ('DisableDialog',           'True'); // Always disable dialog in batch mode

    RunProcess('WorkspaceManager:Print');
End;

Procedure ReadOutputJob(AFileName : String);
Var
    G,J     : Integer;
    S: String;
    ContainerName, ContainerAction, OutputPath, OutputType : String;
    VariantName, VariantScope: String;
Begin
    IniFile := TIniFile.Create(AFileName);

    ContainerName := '';
    ContainerAction := '';
    G := 1; // Group Number
    J := 1; // Job/Container Number

    // Iterate each Output Group: I haven't seen an example output job with more than 1 group
    While (G = 1) Or (ContainerName <> DEFAULT) Do
    Begin
        S := 'OutputGroup'+IntToStr(G); // S stands for Section (aka OutputGroup)

        // Iterate each Output Container
        While (J = 1) Or (ContainerAction <> DEFAULT) Do
        Begin
            ContainerName :=   ReadIni(S, 'OutputMedium', J);
            ContainerAction := ReadIni(S, 'OutputMedium', J, '_Type');
            OutputType :=      ReadIni(S, 'OutputType',   J); // Not using, but could be useful
            OutputPath := ReadIni('PublishSettings', 'OutputBasePath', J); // Or alternatively OutputFilePath as Absolute path

            // TODO: Handle Variant
            VariantName  := ReadIni(S, 'VariantName', J);
            VariantScope := ReadIni(S, 'VariantScope', J);


            If ContainerAction = 'GeneratedFiles' Then
            Begin
                RunGenerateFiles(ContainerName, OutputPath);
            End
            Else If ContainerAction = 'Publish' Then
            Begin
                RunPublishToPDF(ContainerName, J, OutputPath);
            End;

            // Note: There is also a Printer Type that I am not interested in

            Inc(J);
        End;

        Inc(G);
    End;



    IniFile.Free;
End;

Function FilePathDialog(Dummy: String): String;
Var
    OpenDialog     : TOpenDialog;
    FilePath       : String;
Begin
     FilePath := '';

     // Create Open File Dialog to select an OutJob file
     OpenDialog := TOpenDialog.Create(nil);
     Try
         OpenDialog.Title := 'Select OutJob File';
         OpenDialog.Filter := 'OutJob files (*.OutJob)|*.OutJob';
         OpenDialog.FilterIndex := 1;

         // Show dialog and exit if canceled
         If Not OpenDialog.Execute Then
         Begin
            Exit;
         End;

         FilePath := OpenDialog.FileName;
         //BaseFileName := ExtractFileName(FilePath);
         //BaseFileName := Copy(BaseFileName, 1, Length(BaseFileName) - Length(ExtractFileExt(BaseFileName)));
     Finally
        OpenDialog.Free;
     End;

     result := FilePath;
End;

Function GetOpenOutputJob(Doc: IDocument;): String;
Var
    Project     : IProject;
    ProjectIdx, I  : Integer;
    DocKind: String;
Begin
    result := '';

    For ProjectIdx:=0 To GetWorkspace.DM_ProjectCount - 1 Do
    Begin
        Project := GetWorkspace.DM_Projects(ProjectIdx);
        If Project = Nil Then Exit;

        // Iterate Schematic Sheets
        For I := 0 to Project.DM_LogicalDocumentCount - 1 Do
        Begin
            Doc := Project.DM_LogicalDocuments(I);
            DocKind := Doc.DM_DocumentKind;
            If Doc.DM_DocumentKind = 'OUTPUTJOB' Then
            Begin
                 Result :=  Doc.DM_FullPath;
                 Exit;
            End;
        End;
    End;
End;

Procedure Run;
Var
    I           : Integer;
    Workspace   : IWorkspace;
    Doc         : IDocument;
    CurrentSch  : ISch_Document;
    SchematicId : Integer;
    Document : IServerDocument;
    FilePath : String;
Begin
    // 1. Try to find open Output Job
    FilePath := GetOpenOutputJob(Doc);

    // 2. Else Prompt User for Output Job file path
    If FilePath = '' Then
    Begin
        // Prompt user for OutputJob
        FilePath := FilePathDialog('Dummy');
    End;

    Document := Client.OpenDocument('OUTPUTJOB', FilePath); // Open Document

    If Document <> Nil Then
    Begin
        Client.ShowDocument(Document);
        ReadOutputJob(FilePath); // OutputJob is just an INI file and Delphi has a built in TIniFile class
    End;

    // For anyone reading this code, it looks like there may be an alternative way to generate output jobs using the methods below. I didn't want to take the time to explore this.
    //GetWorkspace.DM_GetOutputJobDocumentByPath(
    //IWSM_OutputJobDocument.Outputer
    //IOutputer.DM_Generate_OutputFiles(
    //GetWorkspace.DM_GetOutputJobDocumentByPath(
End;
