{..............................................................................}
{ Summary: Update Component Design Item IDs from CSV File                      }
{          This script reads a CSV file with two columns (old part, new part)  }
{          and updates all components in the project that match the old part   }
{          number with the new part number from the database.                  }
{          After updating, all modified components are selected.               }
{                                                                              }
{ Usage:                                                                        }
{  1. Have a CSV file ready with two columns: old part number, new part number }
{  2. Open your Altium project                                                 }
{  3. Run the script                                                          }
{  4. Select the CSV file when prompted                                        }
{  5. The script will update all matching components across all schematics     }
{  6. All updated components will be selected for easy identification          }
{..............................................................................}

{..............................................................................}
Function UpdateComponentParts(Project: IProject);
Var
    I, J, K          : Integer;
    Doc              : IDocument;
    CurrentSch       : ISch_Document;
    Cmp, NewCmp      : ISch_Component;
    CmpIterator      : ISch_Iterator;
    DesignItemId     : String;
    DBLibName        : String;
    DBTableName      : String;
    NewPartNumber    : String;
    UpdateCount      : Integer;
    ComponentCount   : Integer;
    SchematicCount   : Integer;

    // CSV related variables
    OpenDialog       : TOpenDialog;
    CSVFileName      : String;
    StrList          : TStringList;
    OldPartList      : TStringList;
    NewPartList      : TStringList;
    csv_row          : Integer;
    TempStr          : String;
    CommaPos         : Integer;
    OldPart          : String;
    NewPart          : String;
    DiffDesc         : String;
    DiffInt          : Integer;
    ChangeManager    : IChangeManager;

    // Lists to store updated components for selection
    UpdatedComponents : TObjectList;
    UpdatedSchematics : TObjectList;
    SchDoc            : ISch_Document;
    UpdatedCmp        : ISch_Component;
Begin
    // Create and show file open dialog
    OpenDialog := TOpenDialog.Create(Nil);
    OpenDialog.Title := 'Select Part Number Mapping CSV File';
    OpenDialog.Filter := 'CSV Files (*.csv)|*.csv|All Files (*.*)|*.*';
    OpenDialog.DefaultExt := 'csv';
    OpenDialog.FilterIndex := 0;

    If Not OpenDialog.Execute Then
    Begin
        ShowMessage('No file selected. Script cancelled.');
        Exit;
    End;

    CSVFileName := OpenDialog.FileName;

    If Not FileExists(CSVFileName) Then
    Begin
        ShowMessage('CSV file does not exist: ' + CSVFileName);
        Exit;
    End;

    // Load CSV file
    StrList := TStringList.Create;
    OldPartList := TStringList.Create;
    NewPartList := TStringList.Create;

    // Initialize lists to store updated components
    UpdatedComponents := TObjectList.Create;
    UpdatedComponents.OwnsObjects := False; // Don't destroy the component objects
    UpdatedSchematics := TObjectList.Create;
    UpdatedSchematics.OwnsObjects := False; // Don't destroy the schematic objects

    Try
        StrList.LoadFromFile(CSVFileName);

        ShowMessage('Loading CSV file with ' + IntToStr(StrList.Count-1) + ' part mappings...');

        // Parse CSV - skip header row (row 0), start from row 1
        For csv_row := 1 To StrList.Count - 1 Do
        Begin
            TempStr := Trim(StrList[csv_row]);
            If TempStr = '' Then Continue;

            // Find comma position
            CommaPos := Pos(',', TempStr);
            If CommaPos > 0 Then
            Begin
                OldPart := Trim(Copy(TempStr, 1, CommaPos - 1));
                NewPart := Trim(Copy(TempStr, CommaPos + 1, Length(TempStr)));

                // Remove quotes if present
                If (Length(OldPart) > 1) Then
                Begin
                    If OldPart[1] = '"' Then
                        OldPart := Copy(OldPart, 2, Length(OldPart) - 2);
                End;

                If (Length(NewPart) > 1) Then
                Begin
                    If NewPart[1] = '"' Then
                        NewPart := Copy(NewPart, 2, Length(NewPart) - 2);
                End;

                OldPartList.Add(UpperCase(OldPart));
                NewPartList.Add(NewPart);
            End;
        End;

        ShowMessage('Loaded ' + IntToStr(OldPartList.Count) + ' part number mappings');

    Finally
        StrList.Free;
    End;

    If Project = Nil Then
    Begin
        ShowMessage('No project is currently open.');
        OldPartList.Free;
        NewPartList.Free;
        UpdatedComponents.Free;
        UpdatedSchematics.Free;
        Exit;
    End;

    ChangeManager := GetWorkspace.DM_ChangeManager;

    UpdateCount := 0;
    ComponentCount := 0;
    SchematicCount := 0;

    // Iterate through all schematic documents in the project
    For I := 0 To Project.DM_LogicalDocumentCount - 1 Do
    Begin
        Doc := Project.DM_LogicalDocuments(I);
        If Doc.DM_DocumentKind = 'SCH' Then
        Begin
            SchematicCount := SchematicCount + 1;

            // Open the schematic document
            Client.OpenDocument('SCH', Doc.DM_FullPath);
            CurrentSch := SchServer.GetSchDocumentByPath(Doc.DM_FullPath);

            If CurrentSch <> Nil Then
            Begin
                // Initialize processing
                SchServer.ProcessControl.PreProcess(CurrentSch, '');

                // Create iterator for components
                CmpIterator := CurrentSch.SchIterator_Create;
                CmpIterator.AddFilter_ObjectSet(MkSet(eSchComponent));

                Try
                    Cmp := CmpIterator.FirstSchObject;
                    While Cmp <> Nil Do
                    Begin
                        ComponentCount := ComponentCount + 1;

                        // Get current component information
                        DesignItemId := Cmp.DesignItemId;
                        DBLibName := Cmp.DatabaseLibraryName;
                        DBTableName := Cmp.DatabaseTableName;

                        // Check if this part needs to be updated
                        NewPartNumber := '';
                        For J := 0 To OldPartList.Count - 1 Do
                        Begin
                            If UpperCase(Trim(DesignItemId)) = OldPartList[J] Then
                            Begin
                                NewPartNumber := NewPartList[J];
                                Break;
                            End;
                        End;

                        If NewPartNumber <> '' Then
                        Begin
                            // Use UpdatePart process to update the component
                            Cmp.UpdatePart_PreProcess;
                            Cmp.DesignItemId := NewPartNumber;
                            Cmp.UpdatePart_PostProcess;

                            UpdateCount := UpdateCount + 1;

                            Cmp.Selection := True;

                            // Add the schematic to the list if not already there
                            If UpdatedSchematics.IndexOf(CurrentSch) = -1 Then
                                UpdatedSchematics.Add(CurrentSch);
                        End;

                        Cmp := CmpIterator.NextSchObject;
                    End;
                Finally
                    CurrentSch.SchIterator_Destroy(CmpIterator);
                End;

                // Finalize processing
                SchServer.ProcessControl.PostProcess(CurrentSch, '');
                CurrentSch.GraphicallyInvalidate;
            End;
        End;
    End;

    // Clean up
    OldPartList.Free;
    NewPartList.Free;
    UpdatedComponents.Free;
    UpdatedSchematics.Free;

    // Display summary
    ShowMessage('Update Complete!' + #13#10 +
                'Schematics processed: ' + IntToStr(SchematicCount) + #13#10 +
                'Total components checked: ' + IntToStr(ComponentCount) + #13#10 +
                'Components updated: ' + IntToStr(UpdateCount) + #13#10 + #13#10 +
                'All updated components have been selected.');
End;
{..............................................................................}

{..............................................................................}
Procedure Run;
Begin
    If SchServer = Nil Then
    Begin
        ShowMessage('Schematic server not available.');
        Exit;
    End;

    UpdateComponentParts(GetWorkspace.DM_FocusedProject); // Pass something to hide function from Altium script manager
End;
{..............................................................................}
