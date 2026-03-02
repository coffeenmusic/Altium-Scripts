{..............................................................................}
{ Voltage Divider Calculator for Altium Designer                               }
{                                                                              }
{ Reads resistor values from the current schematic project (filtered by        }
{ package size) or generates standard E-series values, then calculates         }
{ all R1/R2 combinations for a target voltage divider.                         }
{                                                                              }
{ Entry Points:                                                                }
{   Run       - Shows settings form, calculates, displays results              }
{                                                                              }
{ On first launch, the script scans the schematic for all parameter names      }
{ and lets the user map which parameter holds:                                 }
{   - Resistor identification (e.g. DESCRIPTION starts with "RES")             }
{   - Resistance value (e.g. VALUE = "10k")                                    }
{   - Package size (e.g. PKG_STYLE = "0603")                                   }
{ These selections are saved in an INI file for future runs.                   }
{                                                                              }
{ Created with assistance from Claude AI                                       }
{..............................................................................}

Var
    SettingsFilePath   : String;
    AllParameterNames  : TStringList;

{..............................................................................}
{ Math helper: natural log                                                     }
{..............................................................................}
Function LnSafe(X : Double) : Double;
Begin
    If X > 0 Then
        Result := Ln(X)
    Else
        Result := 0;
End;

{..............................................................................}
{ Math helper: log base 10                                                     }
{..............................................................................}
Function Log10(X : Double) : Double;
Begin
    Result := LnSafe(X) / LnSafe(10);
End;

{..............................................................................}
{ Math helper: power function                                                  }
{..............................................................................}
Function Power(Base, Exponent : Double) : Double;
Begin
    If Base > 0 Then
        Result := Exp(Exponent * Ln(Base))
    Else
        Result := 0;
End;

{..............................................................................}
{ Math helper: floor function                                                  }
{..............................................................................}
Function Floor(X : Double) : Integer;
Begin
    Result := Trunc(X);
    If (X < 0) And (Frac(X) <> 0) Then
        Result := Result - 1;
End;

{..............................................................................}
{ Round a number to N significant figures                                      }
{..............................................................................}
Function RoundToSigFigs(Num : Double; SigFigs : Integer) : Double;
Var
    D : Integer;
Begin
    If Num = 0 Then
    Begin
        Result := 0;
        Exit;
    End;
    D := SigFigs - Floor(Log10(Abs(Num))) - 1;
    Result := Round(Num * Power(10, D)) / Power(10, D);
End;

{..............................................................................}
{ Format a resistance value as a human-readable string                         }
{..............................................................................}
Function FormatResistance(Value : Double) : String;
Begin
    If Value = 0 Then
        Result := '0'
    Else If Value >= 1000000 Then
        Result := FormatFloat('0.###', Value / 1000000) + 'M'
    Else If Value >= 1000 Then
        Result := FormatFloat('0.###', Value / 1000) + 'k'
    Else
        Result := FormatFloat('0.###', Value);
End;

{..............................................................................}
{ Parse a resistance string like "10k", "4.7M", "100" to ohms                 }
{..............................................................................}
Function ParseResistanceValue(S : String) : Double;
Var
    Multiplier : Double;
    KPos, MPos : Integer;
Begin
    Result := -1;
    S := Trim(S);
    If S = '' Then Exit;
    If Pos('JUMPER', UpperCase(S)) > 0 Then Exit;

    S := UpperCase(S);

    // Remove trailing 'OHM', 'OHMS'
    S := StringReplace(S, 'OHMS', '', rfReplaceAll);
    S := StringReplace(S, 'OHM', '', rfReplaceAll);
    S := Trim(S);

    Multiplier := 1;
    KPos := Pos('K', S);
    MPos := Pos('M', S);

    If (MPos > 0) And (Pos('MEG', S) > 0) Then
    Begin
        Multiplier := 1000000;
        S := StringReplace(S, 'MEG', '', rfReplaceAll);
        S := StringReplace(S, 'M', '.', rfReplaceAll);
    End
    Else If MPos > 0 Then
    Begin
        Multiplier := 1000000;
        If (MPos > 1) And (MPos < Length(S)) Then
            S := Copy(S, 1, MPos - 1) + '.' + Copy(S, MPos + 1, Length(S) - MPos)
        Else
            S := StringReplace(S, 'M', '', rfReplaceAll);
    End
    Else If KPos > 0 Then
    Begin
        Multiplier := 1000;
        If (KPos > 1) And (KPos < Length(S)) Then
            S := Copy(S, 1, KPos - 1) + '.' + Copy(S, KPos + 1, Length(S) - KPos)
        Else
            S := StringReplace(S, 'K', '', rfReplaceAll);
    End
    Else If Pos('R', S) > 0 Then
    Begin
        Multiplier := 1;
        If (Pos('R', S) > 1) And (Pos('R', S) < Length(S)) Then
            S := Copy(S, 1, Pos('R', S) - 1) + '.' + Copy(S, Pos('R', S) + 1, Length(S) - Pos('R', S))
        Else
            S := StringReplace(S, 'R', '', rfReplaceAll);
    End;

    S := Trim(S);
    Result := StrToFloatDef(S, -1);
    If Result >= 0 Then
        Result := Result * Multiplier;
End;

{..............................................................................}
{ Helper: check if a string exists in a StringList (case-insensitive)          }
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
{ Get a component parameter value by name                                      }
{..............................................................................}
Function GetParameterValue(AComponent : ISch_Component; ParamName : String) : String;
Var
    PIterator  : ISch_Iterator;
    Parameter  : ISch_Parameter;
    UpperParam : String;
Begin
    Result := '';
    UpperParam := UpperCase(ParamName);

    // Handle DESIGNATOR as a built-in property
    If UpperParam = 'DESIGNATOR' Then
    Begin
        Result := AComponent.Designator.Text;
        Exit;
    End;

    // Handle COMMENT as a built-in property
    If UpperParam = 'COMMENT' Then
    Begin
        If AComponent.Comment <> Nil Then
            Result := AComponent.Comment.Text;
        Exit;
    End;

    // Search user-defined parameters (covers DESCRIPTION, VALUE, PKG_STYLE, etc.)
    PIterator := AComponent.SchIterator_Create;
    PIterator.AddFilter_ObjectSet(MkSet(eParameter));
    Try
        Parameter := PIterator.FirstSchObject;
        While Parameter <> Nil Do
        Begin
            If UpperCase(Parameter.Name) = UpperParam Then
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

    // Fallback for DESCRIPTION: try the built-in ComponentDescription
    If (UpperParam = 'DESCRIPTION') And (Result = '') Then
        Result := AComponent.ComponentDescription;
End;

{..............................................................................}
{ Get the INI settings file path (same directory as current project)           }
{..............................................................................}
Function GetSettingsFilePath : String;
Var
    Project     : IProject;
    ProjectPath : String;
Begin
    Result := '';
    Project := GetWorkspace.DM_FocusedProject;
    If Project <> Nil Then
    Begin
        ProjectPath := ExtractFilePath(Project.DM_ProjectFullPath);
        Result := ProjectPath + 'VoltageDivider.ini';
    End;
End;

{..............................................................................}
{ Collect all parameter names from all schematic sheets in the project         }
{..............................................................................}
Procedure CollectAllParameterNames;
Var
    I           : Integer;
    Project     : IProject;
    Doc         : IDocument;
    SchDoc      : ISch_Document;
    Iterator    : ISch_Iterator;
    PIterator   : ISch_Iterator;
    AComponent  : ISch_Component;
    Parameter   : ISch_Parameter;
Begin
    AllParameterNames := TStringList.Create;

    Project := GetWorkspace.DM_FocusedProject;
    If Project = Nil Then Exit;

    Project.DM_Compile;

    For I := 0 To Project.DM_LogicalDocumentCount - 1 Do
    Begin
        Doc := Project.DM_LogicalDocuments(I);
        If Doc.DM_DocumentKind = 'SCH' Then
        Begin
            Client.OpenDocument('SCH', Doc.DM_FullPath);
            SchDoc := SchServer.GetSchDocumentByPath(Doc.DM_FullPath);

            If SchDoc <> Nil Then
            Begin
                // Add built-in properties once
                If Not StringExistsInList(AllParameterNames, 'Designator') Then
                    AllParameterNames.Add('Designator');
                If Not StringExistsInList(AllParameterNames, 'Comment') Then
                    AllParameterNames.Add('Comment');
                If Not StringExistsInList(AllParameterNames, 'Description') Then
                    AllParameterNames.Add('Description');

                Iterator := SchDoc.SchIterator_Create;
                Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

                AComponent := Iterator.FirstSchObject;
                While AComponent <> Nil Do
                Begin
                    // Iterate user-defined parameters
                    PIterator := AComponent.SchIterator_Create;
                    PIterator.AddFilter_ObjectSet(MkSet(eParameter));

                    Parameter := PIterator.FirstSchObject;
                    While Parameter <> Nil Do
                    Begin
                        If Not StringExistsInList(AllParameterNames, Parameter.Name) Then
                            AllParameterNames.Add(Parameter.Name);
                        Parameter := PIterator.NextSchObject;
                    End;
                    AComponent.SchIterator_Destroy(PIterator);

                    AComponent := Iterator.NextSchObject;
                End;
                SchDoc.SchIterator_Destroy(Iterator);
            End;
        End;
    End;

    AllParameterNames.Sort;
End;

{..............................................................................}
{ Select a combo box item by text (case-insensitive), returns True if found    }
{..............................................................................}
Function SelectComboItem(Combo : TComboBox; ItemText : String) : Boolean;
Var
    I : Integer;
Begin
    Result := False;
    For I := 0 To Combo.Items.Count - 1 Do
    Begin
        If UpperCase(Combo.Items[I]) = UpperCase(ItemText) Then
        Begin
            Combo.ItemIndex := I;
            Result := True;
            Exit;
        End;
    End;
End;

{..............................................................................}
{ Populate the parameter mapping combo boxes                                   }
{..............................................................................}
Procedure PopulateParamComboBoxes;
Var
    I : Integer;
Begin
    FormMain.ComboBoxDescParam.Clear;
    FormMain.ComboBoxValueParam.Clear;
    FormMain.ComboBoxPkgParam.Clear;

    For I := 0 To AllParameterNames.Count - 1 Do
    Begin
        FormMain.ComboBoxDescParam.Items.Add(AllParameterNames.Strings[I]);
        FormMain.ComboBoxValueParam.Items.Add(AllParameterNames.Strings[I]);
        FormMain.ComboBoxPkgParam.Items.Add(AllParameterNames.Strings[I]);
    End;
End;

{..............................................................................}
{ Save user settings to INI file                                               }
{..............................................................................}
Procedure SaveSettings;
Var
    IniFile : TIniFile;
Begin
    If SettingsFilePath = '' Then Exit;

    IniFile := TIniFile.Create(SettingsFilePath);
    Try
        // Parameter mapping
        If FormMain.ComboBoxDescParam.ItemIndex >= 0 Then
            IniFile.WriteString('ParamMapping', 'DescParam', FormMain.ComboBoxDescParam.Text);
        IniFile.WriteString('ParamMapping', 'DescPrefix', FormMain.EditDescPrefix.Text);
        If FormMain.ComboBoxValueParam.ItemIndex >= 0 Then
            IniFile.WriteString('ParamMapping', 'ValueParam', FormMain.ComboBoxValueParam.Text);
        If FormMain.ComboBoxPkgParam.ItemIndex >= 0 Then
            IniFile.WriteString('ParamMapping', 'PkgParam', FormMain.ComboBoxPkgParam.Text);

        // Voltages
        IniFile.WriteString('Voltages', 'Vin', FormMain.EditVin.Text);
        IniFile.WriteString('Voltages', 'Vout', FormMain.EditVout.Text);

        // R1
        IniFile.WriteString('R1', 'Min', FormMain.EditR1Min.Text);
        IniFile.WriteString('R1', 'Max', FormMain.EditR1Max.Text);
        IniFile.WriteBool('R1', 'TwoResistors', FormMain.CheckBoxTwoR1.Checked);

        // R2
        IniFile.WriteString('R2', 'Min', FormMain.EditR2Min.Text);
        IniFile.WriteString('R2', 'Max', FormMain.EditR2Max.Text);
        IniFile.WriteBool('R2', 'TwoResistors', FormMain.CheckBoxTwoR2.Checked);

        // Current
        IniFile.WriteString('Current', 'MaxCurrent', FormMain.EditMaxCurrent.Text);

        // Packages
        IniFile.WriteBool('Packages', '0201', FormMain.CheckBox0201.Checked);
        IniFile.WriteBool('Packages', '0402', FormMain.CheckBox0402.Checked);
        IniFile.WriteBool('Packages', '0603', FormMain.CheckBox0603.Checked);
        IniFile.WriteBool('Packages', '0805', FormMain.CheckBox0805.Checked);
        IniFile.WriteBool('Packages', '1206', FormMain.CheckBox1206.Checked);
        IniFile.WriteBool('Packages', '1210', FormMain.CheckBox1210.Checked);
        IniFile.WriteBool('Packages', '2010', FormMain.CheckBox2010.Checked);
        IniFile.WriteBool('Packages', '2512', FormMain.CheckBox2512.Checked);
        IniFile.WriteBool('Packages', 'Other', FormMain.CheckBoxOther.Checked);

        // Source
        IniFile.WriteBool('Source', 'UseSchematic', FormMain.RadioButtonSchematic.Checked);

        // Tolerance
        IniFile.WriteInteger('Source', 'ToleranceIndex', FormMain.ComboBoxTolerance.ItemIndex);

        // Ignore list
        IniFile.WriteString('Ignore', 'Values', FormMain.EditIgnoreList.Text);

        // Max results
        IniFile.WriteString('Results', 'MaxResults', FormMain.EditMaxResults.Text);
    Finally
        IniFile.Free;
    End;
End;

{..............................................................................}
{ Load user settings from INI file                                             }
{..............................................................................}
Procedure LoadSettings;
Var
    IniFile        : TIniFile;
    SavedDescParam : String;
    SavedValueParam: String;
    SavedPkgParam  : String;
Begin
    If SettingsFilePath = '' Then Exit;
    If Not FileExists(SettingsFilePath) Then
    Begin
        // No INI yet - try to auto-detect defaults from scanned parameters
        SelectComboItem(FormMain.ComboBoxDescParam, 'DESCRIPTION');
        SelectComboItem(FormMain.ComboBoxValueParam, 'VALUE');
        SelectComboItem(FormMain.ComboBoxPkgParam, 'PKG_STYLE');
        Exit;
    End;

    IniFile := TIniFile.Create(SettingsFilePath);
    Try
        // Parameter mapping - read saved names and select them in combos
        SavedDescParam := IniFile.ReadString('ParamMapping', 'DescParam', 'DESCRIPTION');
        FormMain.EditDescPrefix.Text := IniFile.ReadString('ParamMapping', 'DescPrefix', 'RES');
        SavedValueParam := IniFile.ReadString('ParamMapping', 'ValueParam', 'VALUE');
        SavedPkgParam := IniFile.ReadString('ParamMapping', 'PkgParam', 'PKG_STYLE');

        SelectComboItem(FormMain.ComboBoxDescParam, SavedDescParam);
        SelectComboItem(FormMain.ComboBoxValueParam, SavedValueParam);
        SelectComboItem(FormMain.ComboBoxPkgParam, SavedPkgParam);

        // Voltages
        FormMain.EditVin.Text  := IniFile.ReadString('Voltages', 'Vin', '12');
        FormMain.EditVout.Text := IniFile.ReadString('Voltages', 'Vout', '3.3');

        // R1
        FormMain.EditR1Min.Text      := IniFile.ReadString('R1', 'Min', '10');
        FormMain.EditR1Max.Text      := IniFile.ReadString('R1', 'Max', '1000000');
        FormMain.CheckBoxTwoR1.Checked := IniFile.ReadBool('R1', 'TwoResistors', False);

        // R2
        FormMain.EditR2Min.Text      := IniFile.ReadString('R2', 'Min', '10');
        FormMain.EditR2Max.Text      := IniFile.ReadString('R2', 'Max', '1000000');
        FormMain.CheckBoxTwoR2.Checked := IniFile.ReadBool('R2', 'TwoResistors', False);

        // Current
        FormMain.EditMaxCurrent.Text := IniFile.ReadString('Current', 'MaxCurrent', '5');

        // Packages
        FormMain.CheckBox0201.Checked := IniFile.ReadBool('Packages', '0201', False);
        FormMain.CheckBox0402.Checked := IniFile.ReadBool('Packages', '0402', True);
        FormMain.CheckBox0603.Checked := IniFile.ReadBool('Packages', '0603', True);
        FormMain.CheckBox0805.Checked := IniFile.ReadBool('Packages', '0805', True);
        FormMain.CheckBox1206.Checked := IniFile.ReadBool('Packages', '1206', False);
        FormMain.CheckBox1210.Checked := IniFile.ReadBool('Packages', '1210', False);
        FormMain.CheckBox2010.Checked := IniFile.ReadBool('Packages', '2010', False);
        FormMain.CheckBox2512.Checked := IniFile.ReadBool('Packages', '2512', False);
        FormMain.CheckBoxOther.Checked := IniFile.ReadBool('Packages', 'Other', False);

        // Source
        FormMain.RadioButtonSchematic.Checked := IniFile.ReadBool('Source', 'UseSchematic', True);
        FormMain.RadioButtonStandard.Checked  := Not FormMain.RadioButtonSchematic.Checked;

        // Tolerance
        FormMain.ComboBoxTolerance.ItemIndex := IniFile.ReadInteger('Source', 'ToleranceIndex', 0);

        // Ignore list
        FormMain.EditIgnoreList.Text := IniFile.ReadString('Ignore', 'Values', '0');

        // Max results
        FormMain.EditMaxResults.Text := IniFile.ReadString('Results', 'MaxResults', '100');
    Finally
        IniFile.Free;
    End;
End;

{..............................................................................}
{ Build the list of allowed packages from the form checkboxes                  }
{..............................................................................}
Function BuildPackageFilter : TStringList;
Begin
    Result := TStringList.Create;
    If FormMain.CheckBox0201.Checked Then Result.Add('0201');
    If FormMain.CheckBox0402.Checked Then Result.Add('0402');
    If FormMain.CheckBox0603.Checked Then Result.Add('0603');
    If FormMain.CheckBox0805.Checked Then Result.Add('0805');
    If FormMain.CheckBox1206.Checked Then Result.Add('1206');
    If FormMain.CheckBox1210.Checked Then Result.Add('1210');
    If FormMain.CheckBox2010.Checked Then Result.Add('2010');
    If FormMain.CheckBox2512.Checked Then Result.Add('2512');
End;

{..............................................................................}
{ Check if a package matches the filter                                        }
{..............................................................................}
Function PackageMatchesFilter(PkgStyle : String; AllowedPkgs : TStringList; AllowOther : Boolean) : Boolean;
Var
    I : Integer;
Begin
    Result := False;
    PkgStyle := UpperCase(Trim(PkgStyle));

    For I := 0 To AllowedPkgs.Count - 1 Do
    Begin
        If Pos(AllowedPkgs.Strings[I], PkgStyle) > 0 Then
        Begin
            Result := True;
            Exit;
        End;
    End;

    If AllowOther Then
    Begin
        If (Pos('0201', PkgStyle) = 0) And (Pos('0402', PkgStyle) = 0) And
           (Pos('0603', PkgStyle) = 0) And (Pos('0805', PkgStyle) = 0) And
           (Pos('1206', PkgStyle) = 0) And (Pos('1210', PkgStyle) = 0) And
           (Pos('2010', PkgStyle) = 0) And (Pos('2512', PkgStyle) = 0) Then
        Begin
            Result := True;
        End;
    End;
End;

{..............................................................................}
{ Parse the ignore list string into a list of numeric values                   }
{..............................................................................}
Function ParseIgnoreList(IgnoreStr : String) : TStringList;
Var
    StartPos, CommaPos : Integer;
    Token              : String;
Begin
    Result := TStringList.Create;
    If IgnoreStr = '' Then Exit;

    StartPos := 1;
    While StartPos <= Length(IgnoreStr) Do
    Begin
        CommaPos := StartPos;
        While (CommaPos <= Length(IgnoreStr)) And (Copy(IgnoreStr, CommaPos, 1) <> ',') Do
            Inc(CommaPos);

        Token := Trim(Copy(IgnoreStr, StartPos, CommaPos - StartPos));
        If Token <> '' Then
            Result.Add(Token);

        StartPos := CommaPos + 1;
    End;
End;

{..............................................................................}
{ Check if a resistance value is in the ignore list                            }
{..............................................................................}
Function IsInIgnoreList(Value : Double; IgnoreList : TStringList) : Boolean;
Var
    I        : Integer;
    IgnoreVal: Double;
Begin
    Result := False;
    For I := 0 To IgnoreList.Count - 1 Do
    Begin
        IgnoreVal := StrToFloatDef(IgnoreList.Strings[I], -1);
        If (IgnoreVal >= 0) And (Abs(Value - IgnoreVal) < 0.01) Then
        Begin
            Result := True;
            Exit;
        End;
    End;
End;

{..............................................................................}
{ Collect unique resistor values from all schematic sheets using user-selected }
{ parameter names for description, value, and package.                         }
{..............................................................................}
Function CollectSchematicResistors(DescParamName, DescPrefix, ValueParamName, PkgParamName : String;
                                   AllowedPkgs : TStringList; AllowOther : Boolean;
                                   IgnoreList : TStringList) : TStringList;
Var
    I             : Integer;
    Project       : IProject;
    Doc           : IDocument;
    SchDoc        : ISch_Document;
    Iterator      : ISch_Iterator;
    AComponent    : ISch_Component;
    DescStr       : String;
    ValueStr      : String;
    PkgStr        : String;
    ResValue      : Double;
    ResStr        : String;
    Found         : Boolean;
    J             : Integer;
    PrefixLen     : Integer;
    UpperPrefix   : String;
Begin
    Result := TStringList.Create;

    Project := GetWorkspace.DM_FocusedProject;
    If Project = Nil Then
    Begin
        ShowMessage('No project is currently open.');
        Exit;
    End;

    UpperPrefix := UpperCase(DescPrefix);
    PrefixLen := Length(UpperPrefix);

    Project.DM_Compile;

    For I := 0 To Project.DM_LogicalDocumentCount - 1 Do
    Begin
        Doc := Project.DM_LogicalDocuments(I);
        If Doc.DM_DocumentKind = 'SCH' Then
        Begin
            Client.OpenDocument('SCH', Doc.DM_FullPath);
            SchDoc := SchServer.GetSchDocumentByPath(Doc.DM_FullPath);

            If SchDoc <> Nil Then
            Begin
                Iterator := SchDoc.SchIterator_Create;
                Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));
                Try
                    AComponent := Iterator.FirstSchObject;
                    While AComponent <> Nil Do
                    Begin
                        // Check if component matches the resistor ID filter
                        DescStr := UpperCase(GetParameterValue(AComponent, DescParamName));

                        If Copy(DescStr, 1, PrefixLen) = UpperPrefix Then
                        Begin
                            // Get package and value using user-selected parameter names
                            PkgStr := GetParameterValue(AComponent, PkgParamName);
                            ValueStr := GetParameterValue(AComponent, ValueParamName);

                            // Check package filter
                            If PackageMatchesFilter(PkgStr, AllowedPkgs, AllowOther) Then
                            Begin
                                ResValue := ParseResistanceValue(ValueStr);

                                If (ResValue >= 0) And (Not IsInIgnoreList(ResValue, IgnoreList)) Then
                                Begin
                                    // Add only unique values
                                    ResStr := FormatFloat('0.##', ResValue);
                                    Found := False;
                                    For J := 0 To Result.Count - 1 Do
                                    Begin
                                        If Result.Strings[J] = ResStr Then
                                        Begin
                                            Found := True;
                                            Break;
                                        End;
                                    End;
                                    If Not Found Then
                                        Result.Add(ResStr);
                                End;
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
End;

{..............................................................................}
{ Generate standard E-series resistor values                                   }
{..............................................................................}
Function GenerateStandardValues(ToleranceIndex : Integer; ResMin, ResMax : Double;
                                 IgnoreList : TStringList) : TStringList;
Var
    N, SigFigs   : Integer;
    Decade       : Double;
    DecadeIdx    : Integer;
    I            : Integer;
    ResValue     : Double;
    ResStr       : String;
Begin
    Result := TStringList.Create;

    If ToleranceIndex = 1 Then
    Begin
        N := 48;  SigFigs := 3;
    End
    Else If ToleranceIndex = 2 Then
    Begin
        N := 24;  SigFigs := 2;
    End
    Else If ToleranceIndex = 3 Then
    Begin
        N := 12;  SigFigs := 2;
    End
    Else
    Begin
        N := 96;  SigFigs := 3;
    End;

    For DecadeIdx := 1 To 7 Do
    Begin
        Decade := Power(10, DecadeIdx);
        For I := 0 To N - 1 Do
        Begin
            ResValue := RoundToSigFigs(Decade * Power(10, I / N), SigFigs);

            If ResValue > ResMax Then Break;
            If ResValue < ResMin Then Continue;

            If Not IsInIgnoreList(ResValue, IgnoreList) Then
            Begin
                ResStr := FormatFloat('0.##', ResValue);
                Result.Add(ResStr);
            End;
        End;
    End;
End;

{..............................................................................}
{ Populate the results grid with voltage divider calculations                  }
{..............................................................................}
Procedure CalculateAndDisplay(ResistorValues : TStringList;
                               Vin, Vout, R1Min, R1Max, R2Min, R2Max, MaxCurrent : Double;
                               TwoR1, TwoR2 : Boolean; MaxResults : Integer);
Var
    I, J, K, L     : Integer;
    R1, R2         : Double;
    R1b, R2b       : Double;
    Res1, Res2     : Double;
    ApproxVin      : Double;
    ApproxVout     : Double;
    DeltaVin       : Double;
    DeltaVout      : Double;
    Current        : Double;
    VoutPct        : Double;
    ResCount       : Integer;
    ResultCount    : Integer;
    Grid           : TStringGrid;
    ColHeaders     : Array[0..10] Of String;

    ArrDeltaVin    : Array[0..10000] Of Double;
    ArrDeltaVout   : Array[0..10000] Of Double;
    ArrVin         : Array[0..10000] Of Double;
    ArrVout        : Array[0..10000] Of Double;
    ArrR1          : Array[0..10000] Of Double;
    ArrR1b         : Array[0..10000] Of Double;
    ArrR2          : Array[0..10000] Of Double;
    ArrR2b         : Array[0..10000] Of Double;
    ArrCurrent     : Array[0..10000] Of Double;
    ArrVoutPct     : Array[0..10000] Of Double;
    ArrRatio       : Array[0..10000] Of Double;

    InsertPos      : Integer;
Begin
    Grid := FormResults.StringGridResults;
    ResCount := ResistorValues.Count;
    ResultCount := 0;

    BeginHourGlass;
    Try
        For I := 0 To ResCount - 1 Do
        Begin
            R1 := StrToFloatDef(ResistorValues.Strings[I], -1);
            If R1 < 0 Then Continue;

            For K := -1 To ResCount - 1 Do
            Begin
                If Not TwoR1 Then
                Begin
                    If K > -1 Then Break;
                    R1b := 0;
                End
                Else
                Begin
                    If K = -1 Then
                        R1b := 0
                    Else
                    Begin
                        R1b := StrToFloatDef(ResistorValues.Strings[K], -1);
                        If R1b < 0 Then Continue;
                    End;
                End;

                Res1 := R1 + R1b;
                If (Res1 < R1Min) Or (Res1 > R1Max) Then Continue;

                For J := 0 To ResCount - 1 Do
                Begin
                    R2 := StrToFloatDef(ResistorValues.Strings[J], -1);
                    If R2 < 0 Then Continue;

                    For L := -1 To ResCount - 1 Do
                    Begin
                        If Not TwoR2 Then
                        Begin
                            If L > -1 Then Break;
                            R2b := 0;
                        End
                        Else
                        Begin
                            If L = -1 Then
                                R2b := 0
                            Else
                            Begin
                                R2b := StrToFloatDef(ResistorValues.Strings[L], -1);
                                If R2b < 0 Then Continue;
                            End;
                        End;

                        Res2 := R2 + R2b;
                        If (Res2 < R2Min) Or (Res2 > R2Max) Then Continue;
                        If (Res1 + Res2) = 0 Then Continue;

                        ApproxVin := Vout * ((Res1 + Res2) / Res2);
                        ApproxVout := Vin * (Res2 / (Res1 + Res2));
                        DeltaVin := Abs(ApproxVin - Vin);
                        DeltaVout := Abs(ApproxVout - Vout);
                        Current := Vin / (Res1 + Res2);

                        If Current > MaxCurrent Then Continue;

                        If Vout <> 0 Then
                            VoutPct := (DeltaVout / Vout) * 100
                        Else
                            VoutPct := 0;

                        If (ResultCount < MaxResults) Or (DeltaVout < ArrDeltaVout[ResultCount - 1]) Then
                        Begin
                            InsertPos := ResultCount;
                            While (InsertPos > 0) And (DeltaVout < ArrDeltaVout[InsertPos - 1]) Do
                                Dec(InsertPos);

                            If ResultCount < MaxResults Then
                                Inc(ResultCount)
                            Else If InsertPos >= MaxResults Then
                                Continue;

                            If InsertPos < ResultCount - 1 Then
                            Begin
                                For K := ResultCount - 1 DownTo InsertPos + 1 Do
                                Begin
                                    ArrDeltaVin[K]  := ArrDeltaVin[K - 1];
                                    ArrDeltaVout[K] := ArrDeltaVout[K - 1];
                                    ArrVin[K]       := ArrVin[K - 1];
                                    ArrVout[K]      := ArrVout[K - 1];
                                    ArrR1[K]        := ArrR1[K - 1];
                                    ArrR1b[K]       := ArrR1b[K - 1];
                                    ArrR2[K]        := ArrR2[K - 1];
                                    ArrR2b[K]       := ArrR2b[K - 1];
                                    ArrCurrent[K]   := ArrCurrent[K - 1];
                                    ArrVoutPct[K]   := ArrVoutPct[K - 1];
                                    ArrRatio[K]     := ArrRatio[K - 1];
                                End;
                            End;

                            ArrDeltaVin[InsertPos]  := DeltaVin;
                            ArrDeltaVout[InsertPos] := DeltaVout;
                            ArrVin[InsertPos]       := ApproxVin;
                            ArrVout[InsertPos]      := ApproxVout;
                            ArrR1[InsertPos]        := R1;
                            ArrR1b[InsertPos]       := R1b;
                            ArrR2[InsertPos]        := R2;
                            ArrR2b[InsertPos]       := R2b;
                            ArrCurrent[InsertPos]   := Current;
                            ArrVoutPct[InsertPos]   := VoutPct;
                            If Res2 <> 0 Then
                                ArrRatio[InsertPos] := Res1 / Res2
                            Else
                                ArrRatio[InsertPos] := 0;
                        End;
                    End;
                End;
            End;
        End;
    Finally
        EndHourGlass;
    End;

    If ResultCount = 0 Then
    Begin
        ShowMessage('No valid voltage divider combinations found.' + #13 +
                   'Try adjusting the R1/R2 range or current limit.');
        Exit;
    End;

    ColHeaders[0]  := 'Delta Vtop';
    ColHeaders[1]  := 'Delta Vdiv';
    ColHeaders[2]  := 'Vtop (V)';
    ColHeaders[3]  := 'Vdivider (V)';
    ColHeaders[4]  := 'Error %';
    ColHeaders[5]  := 'R1';
    ColHeaders[6]  := 'R1b';
    ColHeaders[7]  := 'R2';
    ColHeaders[8]  := 'R2b';
    ColHeaders[9]  := 'Current (mA)';
    ColHeaders[10] := 'R1/R2 Ratio';

    Grid.ColCount := 11;
    Grid.RowCount := ResultCount + 1;

    For I := 0 To 10 Do
        Grid.Cells[I, 0] := ColHeaders[I];

    For I := 0 To ResultCount - 1 Do
    Begin
        Grid.Cells[0, I + 1]  := FormatFloat('0.000000', ArrDeltaVin[I]);
        Grid.Cells[1, I + 1]  := FormatFloat('0.000000', ArrDeltaVout[I]);
        Grid.Cells[2, I + 1]  := FormatFloat('0.0000', ArrVin[I]);
        Grid.Cells[3, I + 1]  := FormatFloat('0.0000', ArrVout[I]);
        Grid.Cells[4, I + 1]  := FormatFloat('0.000', ArrVoutPct[I]);
        Grid.Cells[5, I + 1]  := FormatResistance(ArrR1[I]);
        Grid.Cells[6, I + 1]  := FormatResistance(ArrR1b[I]);
        Grid.Cells[7, I + 1]  := FormatResistance(ArrR2[I]);
        Grid.Cells[8, I + 1]  := FormatResistance(ArrR2b[I]);
        Grid.Cells[9, I + 1]  := FormatFloat('0.000', ArrCurrent[I] * 1000);
        Grid.Cells[10, I + 1] := FormatFloat('0.000', ArrRatio[I]);
    End;

    Grid.ColWidths[0]  := 80;
    Grid.ColWidths[1]  := 80;
    Grid.ColWidths[2]  := 70;
    Grid.ColWidths[3]  := 70;
    Grid.ColWidths[4]  := 60;
    Grid.ColWidths[5]  := 75;
    Grid.ColWidths[6]  := 75;
    Grid.ColWidths[7]  := 75;
    Grid.ColWidths[8]  := 75;
    Grid.ColWidths[9]  := 85;
    Grid.ColWidths[10] := 80;

    FormResults.ComboBoxFilterColumn.Clear;
    FormResults.ComboBoxFilterColumn.Items.Add('All Columns');
    For I := 0 To 10 Do
        FormResults.ComboBoxFilterColumn.Items.Add(ColHeaders[I]);
    FormResults.ComboBoxFilterColumn.ItemIndex := 0;

    AllDataRowCount := 0;
    FormResults.EditFilter.Text := '';
    FormResults.LabelCount.Caption := 'Results: ' + IntToStr(ResultCount);
End;

{..............................................................................}
{ Main entry point                                                             }
{..............................................................................}
Procedure Run;
Var
    Vin, Vout           : Double;
    R1Min, R1Max        : Double;
    R2Min, R2Max        : Double;
    MaxCurrent          : Double;
    TwoR1, TwoR2       : Boolean;
    MaxResults          : Integer;
    ResistorValues      : TStringList;
    AllowedPkgs         : TStringList;
    IgnoreList          : TStringList;
    ModalResult         : Integer;
    UseSchematic        : Boolean;
    ToleranceIdx        : Integer;
    ResMin, ResMax      : Double;
    InputValid          : Boolean;
    ResistorsOK         : Boolean;
    DescParamName       : String;
    DescPrefix          : String;
    ValueParamName      : String;
    PkgParamName        : String;
Begin
    // Check for schematic server
    If SchServer = Nil Then
    Begin
        ShowMessage('Schematic Server not available.');
        Exit;
    End;

    // Initialize settings file path
    SettingsFilePath := GetSettingsFilePath;

    // Scan all schematic parameters to populate combo boxes
    BeginHourGlass;
    CollectAllParameterNames;
    EndHourGlass;

    If AllParameterNames.Count = 0 Then
    Begin
        ShowMessage('No parameters found in the schematic project.' + #13 +
                   'Ensure a project with schematic documents is open.');
        Exit;
    End;

    // Populate parameter mapping combo boxes
    PopulateParamComboBoxes;

    // Load saved settings (including saved parameter selections)
    LoadSettings;

    // Show main form in a loop (allows "Back" from results)
    While True Do
    Begin
        ModalResult := FormMain.ShowModal;
        If ModalResult <> mrOK Then
        Begin
            AllParameterNames := Nil;
            Exit;
        End;

        // Save settings on every Calculate click
        SaveSettings;

        // Validate parameter selections
        If (FormMain.ComboBoxDescParam.ItemIndex < 0) Or
           (FormMain.ComboBoxValueParam.ItemIndex < 0) Or
           (FormMain.ComboBoxPkgParam.ItemIndex < 0) Then
        Begin
            ShowMessage('Please select parameters for Resistor ID, Value, and Package.');
        End
        Else
        Begin
            // Get the user-selected parameter names
            DescParamName := FormMain.ComboBoxDescParam.Text;
            DescPrefix := FormMain.EditDescPrefix.Text;
            ValueParamName := FormMain.ComboBoxValueParam.Text;
            PkgParamName := FormMain.ComboBoxPkgParam.Text;

            // Parse numeric input values
            Vin := StrToFloatDef(FormMain.EditVin.Text, 0);
            Vout := StrToFloatDef(FormMain.EditVout.Text, 0);
            R1Min := StrToFloatDef(FormMain.EditR1Min.Text, 10);
            R1Max := StrToFloatDef(FormMain.EditR1Max.Text, 1000000);
            R2Min := StrToFloatDef(FormMain.EditR2Min.Text, 10);
            R2Max := StrToFloatDef(FormMain.EditR2Max.Text, 1000000);
            MaxCurrent := StrToFloatDef(FormMain.EditMaxCurrent.Text, 5) / 1000;
            TwoR1 := FormMain.CheckBoxTwoR1.Checked;
            TwoR2 := FormMain.CheckBoxTwoR2.Checked;
            MaxResults := StrToIntDef(FormMain.EditMaxResults.Text, 100);
            UseSchematic := FormMain.RadioButtonSchematic.Checked;
            ToleranceIdx := FormMain.ComboBoxTolerance.ItemIndex;

            // Validate voltages
            InputValid := True;
            If (Vin <= 0) Or (Vout <= 0) Then
            Begin
                ShowMessage('Vtop and Vdivider must be positive values.');
                InputValid := False;
            End;

            If InputValid And (Vout >= Vin) Then
            Begin
                ShowMessage('Vdivider must be less than Vtop for a voltage divider.');
                InputValid := False;
            End;

            If InputValid Then
            Begin
                If MaxResults < 1 Then MaxResults := 100;
                If MaxResults > 10000 Then MaxResults := 10000;

                IgnoreList := ParseIgnoreList(FormMain.EditIgnoreList.Text);
                ResistorValues := Nil;
                ResistorsOK := True;

                If UseSchematic Then
                Begin
                    AllowedPkgs := BuildPackageFilter;
                    ResistorValues := CollectSchematicResistors(
                        DescParamName, DescPrefix, ValueParamName, PkgParamName,
                        AllowedPkgs, FormMain.CheckBoxOther.Checked, IgnoreList);
                    AllowedPkgs.Free;

                    If ResistorValues.Count = 0 Then
                    Begin
                        ShowMessage('No resistors found in schematic.' + #13 +
                                   'Check parameter mapping and package selections.');
                        ResistorsOK := False;
                    End;
                End
                Else
                Begin
                    ResMin := R1Min;
                    If R2Min < ResMin Then ResMin := R2Min;
                    ResMax := R1Max;
                    If R2Max > ResMax Then ResMax := R2Max;
                    ResistorValues := GenerateStandardValues(ToleranceIdx, ResMin, ResMax, IgnoreList);
                End;

                If ResistorsOK Then
                Begin
                    CalculateAndDisplay(ResistorValues, Vin, Vout, R1Min, R1Max, R2Min, R2Max,
                                        MaxCurrent, TwoR1, TwoR2, MaxResults);
                    ModalResult := FormResults.ShowModal;
                End;

                If ResistorValues <> Nil Then ResistorValues.Free;
                IgnoreList.Free;

                If ResistorsOK And (ModalResult <> mrAbort) Then
                Begin
                    If AllParameterNames <> Nil Then AllParameterNames.Free;
                    AllParameterNames := Nil;
                    Exit;
                End;
            End;
        End;
    End;
End;

End.
