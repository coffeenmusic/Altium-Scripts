{..............................................................................}
{ Summary: Unique Parts Lister for Altium Designer Schematics                  }
{                                                                              }
{ This script iterates through all components in the current schematic         }
{ project, collects all available parameter names, allows the user to          }
{ select which parameter to use as a unique identifier, and displays           }
{ a table of unique parts with selected additional columns.                    }
{                                                                              }
{ Entry Points:                                                                }
{   RunGUI    - Uses saved INI settings (fast launch)                          }
{   RunConfig - Always shows parameter selection dialog                        }
{                                                                              }
{ Version 1.2 - Separated configuration from main run for faster launches      }
{ Version 1.1 - Added INI file to remember parameter selections                }
{ Created with assistance from Claude AI                                       }
{..............................................................................}

Var
    // Global lists for parameter collection
    AllParameterNames    : TStringList;
    UniquePartsData      : TStringList;

    // INI file path for saving settings
    SettingsFilePath     : String;

{..............................................................................}
{ Helper function to check if a string exists in a StringList                  }
{..............................................................................}
Function StringExistsInList(AList : TStringList; AString : String) : Boolean;
Var
    I : Integer;
Begin
    Result := False;
    For I := 0 To AList.Count - 1 Do
    Begin
        If UpperCase(AList.Strings[I]) = UpperCase(AString) Then
        Begin
            Result := True;
            Exit;
        End;
    End;
End;

{..............................................................................}
{ Get the INI file path (same directory as current project)                    }
{..............................................................................}
Function GetSettingsFilePath : String;
Var
    Project : IProject;
    ProjectPath : String;
Begin
    Result := '';
    Project := GetWorkspace.DM_FocusedProject;
    If Project <> Nil Then
    Begin
        ProjectPath := ExtractFilePath(Project.DM_ProjectFullPath);
        Result := ProjectPath + 'UniquePartsLister.ini';
    End;
End;

{..............................................................................}
{ Load settings from INI file                                                  }
{..............................................................................}
Procedure LoadSettingsFromINI(Var SavedUniqueID : String; SavedColumns : TStringList);
Var
    IniFile    : TIniFile;
    ColumnStr  : String;
    StartPos   : Integer;
    CommaPos   : Integer;
    ParamName  : String;
Begin
    SavedUniqueID := '';
    SavedColumns.Clear;

    If SettingsFilePath = '' Then Exit;
    If Not FileExists(SettingsFilePath) Then Exit;

    IniFile := TIniFile.Create(SettingsFilePath);
    Try
        SavedUniqueID := IniFile.ReadString('Settings', 'UniqueIDParam', '');
        ColumnStr := IniFile.ReadString('Settings', 'ColumnParams', '');

        // Parse comma-separated column names
        If ColumnStr <> '' Then
        Begin
            StartPos := 1;
            While StartPos <= Length(ColumnStr) Do
            Begin
                CommaPos := StartPos;
                While (CommaPos <= Length(ColumnStr)) And (Copy(ColumnStr, CommaPos, 1) <> ',') Do
                    Inc(CommaPos);

                ParamName := Trim(Copy(ColumnStr, StartPos, CommaPos - StartPos));
                If ParamName <> '' Then
                    SavedColumns.Add(ParamName);

                StartPos := CommaPos + 1;
            End;
        End;
    Finally
        IniFile.Free;
    End;
End;

{..............................................................................}
{ Save settings to INI file                                                    }
{..............................................................................}
Procedure SaveSettingsToINI(UniqueIDParam : String; ColumnParams : TStringList);
Var
    IniFile    : TIniFile;
    ColumnStr  : String;
    I          : Integer;
Begin
    If SettingsFilePath = '' Then Exit;

    // Build comma-separated column list
    ColumnStr := '';
    For I := 0 To ColumnParams.Count - 1 Do
    Begin
        If I > 0 Then
            ColumnStr := ColumnStr + ',';
        ColumnStr := ColumnStr + ColumnParams.Strings[I];
    End;

    IniFile := TIniFile.Create(SettingsFilePath);
    Try
        IniFile.WriteString('Settings', 'UniqueIDParam', UniqueIDParam);
        IniFile.WriteString('Settings', 'ColumnParams', ColumnStr);
    Finally
        IniFile.Free;
    End;
End;

{..............................................................................}
{ Get parameter value from a component by parameter name                       }
{..............................................................................}
Function GetParameterValue(AComponent : ISch_Component; ParamName : String) : String;
Var
    PIterator : ISch_Iterator;
    Parameter : ISch_Parameter;
Begin
    Result := '';

    // Check for built-in properties first
    If UpperCase(ParamName) = 'DESIGNATOR' Then
    Begin
        Result := AComponent.Designator.Text;
        Exit;
    End;

    If UpperCase(ParamName) = 'COMMENT' Then
    Begin
        If AComponent.Comment <> Nil Then
            Result := AComponent.Comment.Text;
        Exit;
    End;

    If UpperCase(ParamName) = 'LIBRARY REFERENCE' Then
    Begin
        Result := AComponent.LibReference;
        Exit;
    End;

    If UpperCase(ParamName) = 'LIBRARY PATH' Then
    Begin
        Result := AComponent.SourceLibraryName;
        Exit;
    End;

    If UpperCase(ParamName) = 'DESCRIPTION' Then
    Begin
        Result := AComponent.ComponentDescription;
        Exit;
    End;

    // Iterate through component parameters
    Try
        PIterator := AComponent.SchIterator_Create;
        PIterator.AddFilter_ObjectSet(MkSet(eParameter));

        Parameter := PIterator.FirstSchObject;
        While Parameter <> Nil Do
        Begin
            If UpperCase(Parameter.Name) = UpperCase(ParamName) Then
            Begin
                Result := Parameter.Text;
                AComponent.SchIterator_Destroy(PIterator);
                Exit;
            End;
            Parameter := PIterator.NextSchObject;
        End;
    Finally
        AComponent.SchIterator_Destroy(PIterator);
    End;
End;

{..............................................................................}
{ Collect all parameter names from a single schematic document                 }
{..............................................................................}
Procedure CollectParametersFromSheet(SchDoc : ISch_Document);
Var
    Iterator   : ISch_Iterator;
    PIterator  : ISch_Iterator;
    AComponent : ISch_Component;
    Parameter  : ISch_Parameter;
Begin
    If SchDoc = Nil Then Exit;

    // Create iterator for components
    Iterator := SchDoc.SchIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    Try
        AComponent := Iterator.FirstSchObject;
        While AComponent <> Nil Do
        Begin
            // Add built-in properties if not already added
            If Not StringExistsInList(AllParameterNames, 'Designator') Then
                AllParameterNames.Add('Designator');
            If Not StringExistsInList(AllParameterNames, 'Comment') Then
                AllParameterNames.Add('Comment');
            If Not StringExistsInList(AllParameterNames, 'Library Reference') Then
                AllParameterNames.Add('Library Reference');
            If Not StringExistsInList(AllParameterNames, 'Library Path') Then
                AllParameterNames.Add('Library Path');
            If Not StringExistsInList(AllParameterNames, 'Description') Then
                AllParameterNames.Add('Description');

            // Iterate through component's parameters
            Try
                PIterator := AComponent.SchIterator_Create;
                PIterator.AddFilter_ObjectSet(MkSet(eParameter));

                Parameter := PIterator.FirstSchObject;
                While Parameter <> Nil Do
                Begin
                    // Add parameter name if not already in list
                    If Not StringExistsInList(AllParameterNames, Parameter.Name) Then
                        AllParameterNames.Add(Parameter.Name);

                    Parameter := PIterator.NextSchObject;
                End;
            Finally
                AComponent.SchIterator_Destroy(PIterator);
            End;

            AComponent := Iterator.NextSchObject;
        End;
    Finally
        SchDoc.SchIterator_Destroy(Iterator);
    End;
End;

{..............................................................................}
{ Collect all parameters from all schematic sheets in the project              }
{ (Dummy parameter hides from user menu)                                       }
{..............................................................................}
Procedure CollectAllParameters(Dummy : Integer);
Var
    I           : Integer;
    Project     : IProject;
    Doc         : IDocument;
    SchDoc      : ISch_Document;
Begin
    Project := GetWorkspace.DM_FocusedProject;
    If Project = Nil Then
    Begin
        ShowMessage('No project is currently open.');
        Exit;
    End;

    // Compile the project to ensure all documents are accessible
    Project.DM_Compile;

    // Iterate through all documents in the project
    For I := 0 To Project.DM_LogicalDocumentCount - 1 Do
    Begin
        Doc := Project.DM_LogicalDocuments(I);
        If Doc.DM_DocumentKind = 'SCH' Then
        Begin
            SchDoc := SchServer.GetSchDocumentByPath(Doc.DM_FullPath);
            If SchDoc <> Nil Then
                CollectParametersFromSheet(SchDoc);
        End;
    End;
End;

{..............................................................................}
{ Build unique parts list from all schematic sheets                            }
{..............................................................................}
Procedure BuildUniquePartsList(UniqueIDParam : String; ColumnParams : TStringList);
Var
    I, J, K      : Integer;
    Project      : IProject;
    Doc          : IDocument;
    SchDoc       : ISch_Document;
    Iterator     : ISch_Iterator;
    AComponent   : ISch_Component;
    UniqueID     : String;
    DataLine     : String;
    FoundIndex   : Integer;
    ParamValue   : String;
    UniqueIDs    : TStringList;
Begin
    UniquePartsData.Clear;
    UniqueIDs := TStringList.Create;

    Try
        Project := GetWorkspace.DM_FocusedProject;
        If Project = Nil Then Exit;

        // Iterate through all schematic documents
        For I := 0 To Project.DM_LogicalDocumentCount - 1 Do
        Begin
            Doc := Project.DM_LogicalDocuments(I);
            If Doc.DM_DocumentKind = 'SCH' Then
            Begin
                SchDoc := SchServer.GetSchDocumentByPath(Doc.DM_FullPath);
                If SchDoc <> Nil Then
                Begin
                    Iterator := SchDoc.SchIterator_Create;
                    Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

                    Try
                        AComponent := Iterator.FirstSchObject;
                        While AComponent <> Nil Do
                        Begin
                            // Get the unique ID value for this component
                            UniqueID := GetParameterValue(AComponent, UniqueIDParam);

                            // Skip if unique ID is empty
                            If UniqueID <> '' Then
                            Begin
                                // Check if this unique ID already exists
                                FoundIndex := -1;
                                For J := 0 To UniqueIDs.Count - 1 Do
                                Begin
                                    If UniqueIDs.Strings[J] = UniqueID Then
                                    Begin
                                        FoundIndex := J;
                                        Break;
                                    End;
                                End;

                                // If not found, add new entry
                                If FoundIndex = -1 Then
                                Begin
                                    // Build data line with all selected columns
                                    DataLine := UniqueID;
                                    For K := 0 To ColumnParams.Count - 1 Do
                                    Begin
                                        ParamValue := GetParameterValue(AComponent, ColumnParams.Strings[K]);
                                        DataLine := DataLine + #9 + ParamValue;
                                    End;

                                    UniqueIDs.Add(UniqueID);
                                    UniquePartsData.Add(DataLine);
                                End;
                            End;

                            AComponent := Iterator.NextSchObject;
                        End;
                    Finally
                        SchDoc.SchIterator_Destroy(Iterator);
                    End;
                End;
            End;
        End;
    Finally
        UniqueIDs.Free;
    End;
End;

{..............................................................................}
{ Populate the parameter selection form                                        }
{ (Dummy parameter hides from user menu)                                       }
{..............................................................................}
Procedure PopulateParamSelectForm(Dummy : Integer);
Var
    I              : Integer;
    SavedUniqueID  : String;
    SavedColumns   : TStringList;
    ParamName      : String;
Begin
    FormParamSelect.ListBoxUniqueID.Clear;
    FormParamSelect.CheckListBoxColumns.Clear;

    // Sort parameters alphabetically for easier selection
    AllParameterNames.Sort;

    For I := 0 To AllParameterNames.Count - 1 Do
    Begin
        FormParamSelect.ListBoxUniqueID.Items.Add(AllParameterNames.Strings[I]);
        FormParamSelect.CheckListBoxColumns.Items.Add(AllParameterNames.Strings[I]);
    End;

    FormParamSelect.Label3.Caption := 'Parameters found: ' + IntToStr(AllParameterNames.Count);

    // Load and apply saved settings
    SavedColumns := TStringList.Create;
    Try
        LoadSettingsFromINI(SavedUniqueID, SavedColumns);

        // Select the saved unique ID parameter
        If SavedUniqueID <> '' Then
        Begin
            For I := 0 To FormParamSelect.ListBoxUniqueID.Items.Count - 1 Do
            Begin
                If FormParamSelect.ListBoxUniqueID.Items[I] = SavedUniqueID Then
                Begin
                    FormParamSelect.ListBoxUniqueID.ItemIndex := I;
                    Break;
                End;
            End;
        End;

        // Check the saved column parameters
        For I := 0 To FormParamSelect.CheckListBoxColumns.Items.Count - 1 Do
        Begin
            ParamName := FormParamSelect.CheckListBoxColumns.Items[I];
            If StringExistsInList(SavedColumns, ParamName) Then
                FormParamSelect.CheckListBoxColumns.Checked[I] := True;
        End;
    Finally
        SavedColumns.Free;
    End;
End;

{..............................................................................}
{ Navigate to a component in the schematic that matches the parameter value    }
{..............................................................................}
Procedure NavigateToComponent(ParamName : String; ParamValue : String);
Var
    Project      : IProject;
    DocIndex     : Integer;
    SchDoc       : ISch_Document;
    Iterator     : ISch_Iterator;
    AComponent   : ISch_Component;
    CompValue    : String;
    Found        : Boolean;
    DocPath      : String;
Begin
    If ParamValue = '' Then Exit;

    Project := GetWorkspace.DM_FocusedProject;
    If Project = Nil Then Exit;

    Found := False;

    // Iterate through all schematic documents
    For DocIndex := 0 To Project.DM_LogicalDocumentCount - 1 Do
    Begin
        If Found Then Break;

        If Project.DM_LogicalDocuments(DocIndex).DM_DocumentKind = 'SCH' Then
        Begin
            DocPath := Project.DM_LogicalDocuments(DocIndex).DM_FullPath;
            SchDoc := SchServer.GetSchDocumentByPath(DocPath);

            If SchDoc = Nil Then
            Begin
                // Document not open, try to open it
                Client.OpenDocument('SCH', DocPath);
                SchDoc := SchServer.GetSchDocumentByPath(DocPath);
            End;

            If SchDoc <> Nil Then
            Begin
                Iterator := SchDoc.SchIterator_Create;
                Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

                Try
                    AComponent := Iterator.FirstSchObject;
                    While AComponent <> Nil Do
                    Begin
                        // Get the parameter value for this component
                        CompValue := GetParameterValue(AComponent, ParamName);

                        If CompValue = ParamValue Then
                        Begin
                            // Found the component - navigate to it
                            Found := True;

                            // Make this document the active document
                            Client.ShowDocument(Client.OpenDocument('SCH', DocPath));

                            // Deselect all first
                            ResetParameters;
                            RunProcess('Sch:DeSelect');

                            // Select this component
                            AComponent.SetState_Selection(True);

                            // Refresh and zoom to selected component
                            SchDoc.GraphicallyInvalidate;

                            ResetParameters;
                            RunProcess('Sch:ZoomSelected');

                            Break;
                        End;

                        AComponent := Iterator.NextSchObject;
                    End;
                Finally
                    SchDoc.SchIterator_Destroy(Iterator);
                End;
            End;
        End;
    End;

    If Not Found Then
        ShowMessage('Component with ' + ParamName + ' = "' + ParamValue + '" not found.');
End;

{..............................................................................}
{ Populate the results form with unique parts data                             }
{..............................................................................}
Procedure PopulateResultsForm(UniqueIDParam : String; ColumnParams : TStringList);
Var
    I, J       : Integer;
    DataLine   : String;
    ColCount   : Integer;
    StartPos   : Integer;
    EndPos     : Integer;
    FieldValue : String;
Begin
    ColCount := 1 + ColumnParams.Count;

    // Setup the grid
    FormResults.StringGridResults.ColCount := ColCount;
    FormResults.StringGridResults.RowCount := UniquePartsData.Count + 1;

    // Set column headers
    FormResults.StringGridResults.Cells[0, 0] := UniqueIDParam;
    For I := 0 To ColumnParams.Count - 1 Do
        FormResults.StringGridResults.Cells[I + 1, 0] := ColumnParams.Strings[I];

    // Populate data rows
    For I := 0 To UniquePartsData.Count - 1 Do
    Begin
        DataLine := UniquePartsData.Strings[I];

        // Parse tab-separated values manually
        J := 0;
        StartPos := 1;
        While (StartPos <= Length(DataLine)) And (J < ColCount) Do
        Begin
            EndPos := StartPos;
            While (EndPos <= Length(DataLine)) And (Copy(DataLine, EndPos, 1) <> #9) Do
                Inc(EndPos);

            FieldValue := Copy(DataLine, StartPos, EndPos - StartPos);
            FormResults.StringGridResults.Cells[J, I + 1] := FieldValue;

            Inc(J);
            StartPos := EndPos + 1;
        End;
    End;

    // Set column widths
    For I := 0 To ColCount - 1 Do
        FormResults.StringGridResults.ColWidths[I] := 150;

    // Setup filter combobox
    FormResults.ComboBoxFilterColumn.Clear;
    FormResults.ComboBoxFilterColumn.Items.Add('All Columns');
    FormResults.ComboBoxFilterColumn.Items.Add(UniqueIDParam);
    For I := 0 To ColumnParams.Count - 1 Do
        FormResults.ComboBoxFilterColumn.Items.Add(ColumnParams.Strings[I]);
    FormResults.ComboBoxFilterColumn.ItemIndex := 0;

    // Reset filter state to force re-initialization
    AllDataRowCount := 0;

    // Force filter to run by setting non-empty then empty
    // (ensures OnChange fires even if text was already empty)
    FormResults.EditFilter.Text := ' ';
    FormResults.EditFilter.Text := '';

    FormResults.LabelCount.Caption := 'Unique parts: ' + IntToStr(UniquePartsData.Count);
End;

{..............................................................................}
{ Show results for given parameters - shared by both entry points              }
{..............................................................................}
Procedure ShowResultsForParams(UniqueIDParam : String; ColumnParams : TStringList);
Begin
    // Build unique parts list
    BeginHourGlass;
    BuildUniquePartsList(UniqueIDParam, ColumnParams);
    EndHourGlass;

    If UniquePartsData.Count = 0 Then
    Begin
        ShowMessage('No unique parts found with the selected parameter.');
        Exit;
    End;

    // Show results dialog
    PopulateResultsForm(UniqueIDParam, ColumnParams);

    // Show form and handle result
    If FormResults.ShowModal = mrNavigate Then
    Begin
        // User double-clicked to navigate - find and zoom to component
        NavigateToComponent(NavigateParamName, NavigateParamValue);
    End;
End;

{..............................................................................}
{ RunConfig - Always shows parameter selection dialog                          }
{ Use this entry point to change which parameters are displayed                }
{..............................................................................}
Procedure RunConfig;
Var
    I               : Integer;
    UniqueIDParam   : String;
    ColumnParams    : TStringList;
Begin
    // Check if schematic server exists
    If SchServer = Nil Then
    Begin
        ShowMessage('Schematic Server not available.');
        Exit;
    End;

    // Initialize settings file path
    SettingsFilePath := GetSettingsFilePath;

    // Initialize lists
    AllParameterNames := TStringList.Create;
    UniquePartsData := TStringList.Create;
    ColumnParams := TStringList.Create;

    Try
        // Collect all parameter names from the project
        BeginHourGlass;
        CollectAllParameters(0);
        EndHourGlass;

        If AllParameterNames.Count = 0 Then
        Begin
            ShowMessage('No parameters found in the schematic project.' + #13 +
                       'Please ensure a project with schematic documents is open.');
            Exit;
        End;

        // Show parameter selection dialog
        PopulateParamSelectForm(0);

        If FormParamSelect.ShowModal <> mrOK Then
            Exit;

        // Get selected unique ID parameter
        If FormParamSelect.ListBoxUniqueID.ItemIndex < 0 Then
        Begin
            ShowMessage('Please select a parameter for the Unique ID.');
            Exit;
        End;

        UniqueIDParam := FormParamSelect.ListBoxUniqueID.Items[FormParamSelect.ListBoxUniqueID.ItemIndex];

        // Get selected additional columns
        For I := 0 To FormParamSelect.CheckListBoxColumns.Items.Count - 1 Do
        Begin
            If FormParamSelect.CheckListBoxColumns.Checked[I] Then
            Begin
                If FormParamSelect.CheckListBoxColumns.Items[I] <> UniqueIDParam Then
                    ColumnParams.Add(FormParamSelect.CheckListBoxColumns.Items[I]);
            End;
        End;

        // Save settings to INI file for next time
        SaveSettingsToINI(UniqueIDParam, ColumnParams);

        // Show results
        ShowResultsForParams(UniqueIDParam, ColumnParams);

    Finally
        AllParameterNames.Free;
        UniquePartsData.Free;
        ColumnParams.Free;
    End;
End;

{..............................................................................}
{ RunGUI - Main entry point using saved INI settings                           }
{ If no settings saved, automatically runs RunConfig                           }
{..............................................................................}
Procedure RunGUI;
Var
    UniqueIDParam   : String;
    ColumnParams    : TStringList;
Begin
    // Check if schematic server exists
    If SchServer = Nil Then
    Begin
        ShowMessage('Schematic Server not available.');
        Exit;
    End;

    // Initialize settings file path
    SettingsFilePath := GetSettingsFilePath;

    // Try to load saved settings from INI
    ColumnParams := TStringList.Create;
    Try
        LoadSettingsFromINI(UniqueIDParam, ColumnParams);

        // If no saved settings, run the configuration dialog instead
        If UniqueIDParam = '' Then
        Begin
            ColumnParams.Free;
            ColumnParams := Nil;  // Prevent double-free in Finally block
            RunConfig;
            Exit;
        End;

        // Initialize lists for results
        AllParameterNames := TStringList.Create;
        UniquePartsData := TStringList.Create;

        Try
            // Show results using saved parameters
            ShowResultsForParams(UniqueIDParam, ColumnParams);
        Finally
            AllParameterNames.Free;
            UniquePartsData.Free;
        End;

    Finally
        ColumnParams.Free;
    End;
End;

{..............................................................................}
End.
