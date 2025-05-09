{..............................................................................}
{ Summary Demo the use of Integrated Library Manager and Model Type Manager    }
{         interfaces to extract data associated with each interface            }
{                                                                              }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}
{..............................................................................}
Function BooleanToString (Value : LongBool) : String;
Begin
    Result := 'True';
    If Value = True Then Result := 'True'
                    Else Result := 'False';
End;
{..............................................................................}

Function  TryOther(Db:IDatabaseLibDocument);
Var
    ConnStr     : WideString;
    TableIdx    : Integer;
    SQL         : WideString;
    ErrMsg      : WideString;
    RowCnt      : Integer;
begin
    TableIdx := Db.GetTableIndex('CAPACITORS_Query');        // name as shown in the Table Browser tab
    SQL      := Db.GetCommandString(TableIdx, '',      // no filter-text (use the full table)
                                    'WHERE Mfgr1=''KEMET''');

    {---(3)  Execute it just to see how many parts match ---}
    RowCnt := Db.GetItemCount(SQL, ErrMsg);
    If ErrMsg <> '' Then
        ShowMessage('SQL error: '+ErrMsg)
    Else
        ShowMessage('Rows found = '+IntToStr(RowCnt));

end;


{..............................................................................}
Procedure Run;
Var
    LibIdx,TableIdx,FieldIdx,KeyIdx          : Integer;
    IntMan         : IIntegratedLibraryManager;
    doc            : IDocument;
    WS             : IWorkspace;
    Prj            : IProject;
    DbLibReport    : TStringList;
    TableName, FieldName, KeyField, FiltText,LibPathFieldName      : String;
    LibPath, ParameterName, DesignParameterName,CmpParams        : String;
    DbLib          : IDatabaseLibDocument;
    AllCmps: IStrings;
    CmpKeys: TStringList; //AComponentKeys;
    CmpCnt,KeyFieldCnt: Integer;
    DbCmpParams, NewRecord: String;
    CommaDelimitedFieldValues : String;
    ALibCompReader : ILibCompInfoReader;
Begin
    WS  := GetWorkspace;
    If WS = Nil Then Exit;
    Prj := WS.DM_FocusedProject;
    If Prj = Nil Then Exit;
    // Compile the project to fetch the connectivity
    // information for the design.
    Prj.DM_Compile;
    // Get current schematic document.
    Doc := WS.DM_FocusedDocument;
    If Doc.DM_DocumentKind <> 'SCH' Then
    Begin
        ShowWarning('This is not a schematic document');
        Exit;
    End;

    CmpKeys := TStringList.Create;

    IntMan := IntegratedLibraryManager;
    If IntMan = Nil Then Exit;

    DbLibReport := TStringList.Create;

    For LibIdx := 0 to IntMan.InstalledLibraryCount - 1 Do // IntMan.AvailableLibraryCount
    Begin
        LibPath := IntMan.InstalledLibraryPath(LibIdx);
        DbLibReport.Add(LibPath);

        ALibCompReader := SchServer.CreateLibCompInfoReader(LibPath);

        If ALibCompReader = Nil Then Exit;

        ALibCompReader.ReadAllComponentInfo;
        // Obtain the number of components in the specified sch library.
        CmpCnt := ALibCompReader.NumComponentInfos;

        CmpCnt := IntMan.GetComponentCount(LibPath);


        If (LibPath = '') Or (Not(AnsiEndsStr('.DbLib', LibPath))) Then Continue;

        DbLib := IntMan.GetAvailableDBLibDocAtPath(LibPath);  // IDatabaseLibDocument

        TryOther(DbLib);

        For TableIdx := 0 to DbLib.GetTableCount - 1 Do
        Begin
             TableName := DbLib.GetTableNameAt(TableIdx);
             DbLib.GetAllComponentKeys(TableIdx, CmpKeys);
             //CmpParams := DbLib.GetParametersForComponent(TableIdx, CmpKeys);
             //SchServer.LoadComponentFromDatabaseLibrary(
             //IModelTypeManager.

             KeyFieldCnt := DbLib.GetKeyFieldCount(TableIdx);

             If KeyFieldCnt = 0 Then Continue;

             For KeyIdx := 0 to KeyFieldCnt - 1 Do
             Begin
                  KeyField := DbLib.GetKeyField(False, TableIdx, KeyIdx);
             End;

             If DbLib.GetFieldCount(TableIdx) = 0 Then Continue;

             DbLibReport.Add('- ' + TableName);

             For FieldIdx :=  0 to DbLib.GetFieldCount(TableIdx) - 1 Do
             Begin
                 FieldName := DbLib.GetFieldNameAt(TableIdx, FieldIdx);
                 DesignParameterName := DbLib.GetDesignParameterName(TableIdx, FieldName);
                 ParameterName := DbLib.GetParameterNameAt(TableIdx, FieldIdx);

                 //CommaDelimitedFieldValues := IDataBaseLibCommands.GetCommaDelimitedFieldValues(TableIdx, FieldIdx);

                 NewRecord := DbLib.GetCommandString(TableIdx, '*', 'Description LIKE "*CERAMIC*"');

                 If (ParameterName = '') or (ParameterName = FieldName) Then
                 Begin
                     DbLibReport.Add('  + ' + FieldName);
                 End
                 Else
                 Begin
                     DbLibReport.Add('  + ' + FieldName + ' (' + ParameterName + ')');
                 End;


                 //For KeyIdx := 0 To DbLib.GetKeyFieldCount(TableIdx) - 1 Do
                 //Begin
                 //     KeyField := DbLib.GetKeyField(DesignParameterName, TableIdx, KeyIdx);
                 //
                 //     If KeyField = '' Then Continue;
                 //
                 //     DbLibReport.Add('    -- ' + KeyField);
                 //End;


             End;
        End;
    End;

    CmpKeys.Free;
    DbLibReport.SaveToFile('C:\Users\Stephen Thompson\Downloads\DbLib_Report.Txt');
End;
