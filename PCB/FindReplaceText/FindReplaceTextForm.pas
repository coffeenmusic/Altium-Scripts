{..............................................................................}
{ Summary   Find and Replace Text Objects on PCB with Layer Filter             }
{                                                                              }
{           This script provides a dialog-based interface to find and replace  }
{           text content in text objects on a PCB document. Users can filter   }
{           by layer, use case-sensitive matching, and choose between whole    }
{           word or partial match modes.                                       }
{                                                                              }
{ Features:                                                                    }
{           - Find all text objects containing specified text                  }
{           - Replace text in all matching text objects                        }
{           - Filter by specific layer or search all layers                    }
{           - Case-sensitive or case-insensitive matching                      }
{           - Whole word or partial match modes                                }
{           - Preview function to see matches before replacing                 }
{           - Proper undo system integration                                   }
{                                                                              }
{ Usage:    Run FindAndReplaceTextObjects procedure to launch the dialog       }
{                                                                              }
{ Version:  1.0                                                                }
{ Created:  2025                                                               }
{..............................................................................}

Var
    Board              : IPCB_Board;
    LayerIndexArray    : TList;

{..............................................................................}
{ Populates the layer combo box with all available layers                      }
{..............................................................................}
Procedure PopulateLayerComboBox;
Var
    I : Integer;
Begin
    LayerIndexArray := TList.Create;
    FormFindReplaceText.ComboBoxLayer.Items.Clear;
    
    // Add "All Layers" option first
    FormFindReplaceText.ComboBoxLayer.Items.Add('-- All Layers --');
    LayerIndexArray.Add(0);  // 0 represents "All Layers"
    
    // Add signal layers that are used
    For I := eTopLayer To eBottomLayer Do
    Begin
        If Board.LayerIsUsed[I] Then
        Begin
            FormFindReplaceText.ComboBoxLayer.Items.Add(Layer2String(I));
            LayerIndexArray.Add(I);
        End;
    End;
    
    // Add overlay layers
    FormFindReplaceText.ComboBoxLayer.Items.Add(Layer2String(eTopOverlay));
    LayerIndexArray.Add(eTopOverlay);
    
    FormFindReplaceText.ComboBoxLayer.Items.Add(Layer2String(eBottomOverlay));
    LayerIndexArray.Add(eBottomOverlay);
    
    // Add mechanical layers that are used
    For I := eMechanical1 To eMechanical16 Do
    Begin
        If Board.LayerIsUsed[I] Then
        Begin
            FormFindReplaceText.ComboBoxLayer.Items.Add(Layer2String(I));
            LayerIndexArray.Add(I);
        End;
    End;
    
    // Select "All Layers" by default
    FormFindReplaceText.ComboBoxLayer.ItemIndex := 0;
End;

{..............................................................................}
{ Gets the selected layer from the combo box                                   }
{ Returns 0 if "All Layers" is selected                                        }
{..............................................................................}
Function GetSelectedLayer : TLayer;
Var
    Index : Integer;
Begin
    Result := 0;  // Default to all layers
    Index := FormFindReplaceText.ComboBoxLayer.ItemIndex;
    
    If Index < 0 Then Exit;
    If Index >= LayerIndexArray.Count Then Exit;
    
    Result := LayerIndexArray.Items(Index);
End;

{..............................................................................}
{ Checks if the text matches based on current settings                         }
{..............................................................................}
Function TextMatches(SourceText, SearchText : String; CaseSensitive, WholeWord : Boolean) : Boolean;
Var
    CompareSource : String;
    CompareSearch : String;
    FoundPos      : Integer;
Begin
    Result := False;
    
    If SearchText = '' Then Exit;
    
    // Handle case sensitivity
    If CaseSensitive Then
    Begin
        CompareSource := SourceText;
        CompareSearch := SearchText;
    End
    Else
    Begin
        CompareSource := UpperCase(SourceText);
        CompareSearch := UpperCase(SearchText);
    End;
    
    If WholeWord Then
    Begin
        // Whole word match - exact match only
        Result := (CompareSource = CompareSearch);
    End
    Else
    Begin
        // Partial match - search text appears anywhere
        FoundPos := Pos(CompareSearch, CompareSource);
        Result := (FoundPos > 0);
    End;
End;

{..............................................................................}
{ Replaces text within source string                                           }
{..............................................................................}
Function ReplaceTextInString(SourceText, SearchText, ReplaceWithText : String; CaseSensitive : Boolean) : String;
Var
    CompareSource : String;
    CompareSearch : String;
    FoundPos      : Integer;
    ResultText    : String;
Begin
    Result := SourceText;
    
    If SearchText = '' Then Exit;
    
    ResultText := SourceText;
    
    // Handle case sensitivity for finding
    If CaseSensitive Then
    Begin
        CompareSource := SourceText;
        CompareSearch := SearchText;
    End
    Else
    Begin
        CompareSource := UpperCase(SourceText);
        CompareSearch := UpperCase(SearchText);
    End;
    
    // Find and replace all occurrences
    FoundPos := Pos(CompareSearch, CompareSource);
    While FoundPos > 0 Do
    Begin
        // Replace in the original result text (preserving case of non-matched parts)
        ResultText := Copy(ResultText, 1, FoundPos - 1) + 
                      ReplaceWithText + 
                      Copy(ResultText, FoundPos + Length(SearchText), Length(ResultText));
        
        // Update compare source for next search
        CompareSource := Copy(CompareSource, 1, FoundPos - 1) + 
                         UpperCase(ReplaceWithText) + 
                         Copy(CompareSource, FoundPos + Length(SearchText), Length(CompareSource));
        
        FoundPos := Pos(CompareSearch, CompareSource);
    End;
    
    Result := ResultText;
End;

{..............................................................................}
{ Converts coordinates to a display string                                     }
{..............................................................................}
Function CoordToDisplayString(Coord : TCoord) : String;
Begin
    If Board.DisplayUnit = eImperial Then
        Result := FloatToStr(CoordToMils(Coord)) + ' mils'
    Else
        Result := FloatToStr(CoordToMMs(Coord)) + ' mm';
End;

{..............................................................................}
{ Counts and lists all matching text objects                                   }
{..............................................................................}
Procedure TFormFindReplaceText.ButtonFindAllClick(Sender: TObject);
Var
    Iterator      : IPCB_BoardIterator;
    TextObj       : IPCB_Text;
    SelectedLayer : TLayer;
    SearchText    : String;
    CaseSensitive : Boolean;
    WholeWord     : Boolean;
    MatchCount    : Integer;
    ReportList    : TStringList;
    ReportFile    : String;
    ReportDoc     : IServerDocument;
Begin
    If Board = Nil Then Exit;
    
    SearchText := FormFindReplaceText.EditFind.Text;
    If SearchText = '' Then
    Begin
        ShowMessage('Please enter text to find.');
        Exit;
    End;
    
    SelectedLayer := GetSelectedLayer;
    CaseSensitive := FormFindReplaceText.CheckBoxCaseSensitive.Checked;
    WholeWord := FormFindReplaceText.CheckBoxWholeWord.Checked;
    
    MatchCount := 0;
    ReportList := TStringList.Create;
    ReportList.Add('Find Results for: "' + SearchText + '"');
    ReportList.Add('=====================================');
    ReportList.Add('');
    
    // Create iterator
    Iterator := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eTextObject));
    
    If SelectedLayer = 0 Then
        Iterator.AddFilter_LayerSet(AllLayers)
    Else
        Iterator.AddFilter_LayerSet(MkSet(SelectedLayer));
    
    Iterator.AddFilter_Method(eProcessAll);
    
    // Search for matching text objects
    TextObj := Iterator.FirstPCBObject;
    While TextObj <> Nil Do
    Begin
        If TextMatches(TextObj.Text, SearchText, CaseSensitive, WholeWord) Then
        Begin
            Inc(MatchCount);
            ReportList.Add('Match ' + IntToStr(MatchCount) + ':');
            ReportList.Add('  Text: "' + TextObj.Text + '"');
            ReportList.Add('  Layer: ' + Layer2String(TextObj.Layer));
            ReportList.Add('  Location: X=' + CoordToDisplayString(TextObj.XLocation) + 
                          ', Y=' + CoordToDisplayString(TextObj.YLocation));
            ReportList.Add('');
        End;
        
        TextObj := Iterator.NextPCBObject;
    End;
    
    Board.BoardIterator_Destroy(Iterator);
    
    ReportList.Add('=====================================');
    ReportList.Add('Total matches found: ' + IntToStr(MatchCount));
    
    // Update status label
    FormFindReplaceText.LabelStatus.Caption := 'Found ' + IntToStr(MatchCount) + ' matching text object(s).';
    
    // Save and open report
    ReportFile := ChangeFileExt(Board.FileName, '') + '_FindResults.txt';
    ReportList.SaveToFile(ReportFile);
    ReportList.Free;
    
    ReportDoc := Client.OpenDocument('Text', ReportFile);
    If ReportDoc <> Nil Then
        Client.ShowDocument(ReportDoc);
End;

{..............................................................................}
{ Replaces text in all matching text objects                                   }
{..............................................................................}
Procedure TFormFindReplaceText.ButtonReplaceAllClick(Sender: TObject);
Var
    Iterator        : IPCB_BoardIterator;
    TextObj         : IPCB_Text;
    TextObjList     : TInterfaceList;
    SelectedLayer   : TLayer;
    SearchText      : String;
    ReplaceWithText : String;
    CaseSensitive   : Boolean;
    WholeWord       : Boolean;
    ReplaceCount    : Integer;
    NewText         : String;
    I               : Integer;
Begin
    If Board = Nil Then Exit;
    
    SearchText := FormFindReplaceText.EditFind.Text;
    ReplaceWithText := FormFindReplaceText.EditReplace.Text;
    
    If SearchText = '' Then
    Begin
        ShowMessage('Please enter text to find.');
        Exit;
    End;
    
    SelectedLayer := GetSelectedLayer;
    CaseSensitive := FormFindReplaceText.CheckBoxCaseSensitive.Checked;
    WholeWord := FormFindReplaceText.CheckBoxWholeWord.Checked;
    
    ReplaceCount := 0;
    TextObjList := TInterfaceList.Create;
    
    // Create iterator
    Iterator := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eTextObject));
    
    If SelectedLayer = 0 Then
        Iterator.AddFilter_LayerSet(AllLayers)
    Else
        Iterator.AddFilter_LayerSet(MkSet(SelectedLayer));
    
    Iterator.AddFilter_Method(eProcessAll);
    
    // Collect all matching text objects first
    TextObj := Iterator.FirstPCBObject;
    While TextObj <> Nil Do
    Begin
        If TextMatches(TextObj.Text, SearchText, CaseSensitive, WholeWord) Then
            TextObjList.Add(TextObj);
        
        TextObj := Iterator.NextPCBObject;
    End;
    
    Board.BoardIterator_Destroy(Iterator);
    
    // Now modify the collected objects
    If TextObjList.Count > 0 Then
    Begin
        Try
            PCBServer.PreProcess;
            
            For I := 0 To TextObjList.Count - 1 Do
            Begin
                TextObj := TextObjList.Items[I];
                
                // Notify modification start
                PCBServer.SendMessageToRobots(TextObj.I_ObjectAddress, c_Broadcast, PCBM_BeginModify, c_NoEventData);
                
                // Calculate new text
                If WholeWord Then
                    NewText := ReplaceWithText
                Else
                    NewText := ReplaceTextInString(TextObj.Text, SearchText, ReplaceWithText, CaseSensitive);
                
                // Apply the change
                TextObj.Text := NewText;
                
                // Notify modification end
                PCBServer.SendMessageToRobots(TextObj.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_NoEventData);
                
                Inc(ReplaceCount);
            End;
        Finally
            PCBServer.PostProcess;
            TextObjList.Free;
        End;
        
        // Refresh the PCB view
        Board.ViewManager_FullUpdate;
        Client.SendMessage('PCB:Zoom', 'Action=Redraw', 255, Client.CurrentView);
    End
    Else
        TextObjList.Free;
    
    // Update status
    FormFindReplaceText.LabelStatus.Caption := 'Replaced ' + IntToStr(ReplaceCount) + ' text object(s).';
    ShowMessage('Replaced text in ' + IntToStr(ReplaceCount) + ' object(s).');
End;

{..............................................................................}
{ Preview function - shows what will be replaced without making changes        }
{..............................................................................}
Procedure TFormFindReplaceText.ButtonPreviewClick(Sender: TObject);
Var
    Iterator        : IPCB_BoardIterator;
    TextObj         : IPCB_Text;
    SelectedLayer   : TLayer;
    SearchText      : String;
    ReplaceWithText : String;
    CaseSensitive   : Boolean;
    WholeWord       : Boolean;
    MatchCount      : Integer;
    NewText         : String;
    ReportList      : TStringList;
    ReportFile      : String;
    ReportDoc       : IServerDocument;
Begin
    If Board = Nil Then Exit;
    
    SearchText := FormFindReplaceText.EditFind.Text;
    ReplaceWithText := FormFindReplaceText.EditReplace.Text;
    
    If SearchText = '' Then
    Begin
        ShowMessage('Please enter text to find.');
        Exit;
    End;
    
    SelectedLayer := GetSelectedLayer;
    CaseSensitive := FormFindReplaceText.CheckBoxCaseSensitive.Checked;
    WholeWord := FormFindReplaceText.CheckBoxWholeWord.Checked;
    
    MatchCount := 0;
    ReportList := TStringList.Create;
    ReportList.Add('Preview: Find "' + SearchText + '" -> Replace with "' + ReplaceWithText + '"');
    ReportList.Add('=========================================================================');
    ReportList.Add('');
    
    // Create iterator
    Iterator := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eTextObject));
    
    If SelectedLayer = 0 Then
        Iterator.AddFilter_LayerSet(AllLayers)
    Else
        Iterator.AddFilter_LayerSet(MkSet(SelectedLayer));
    
    Iterator.AddFilter_Method(eProcessAll);
    
    // Search for matching text objects
    TextObj := Iterator.FirstPCBObject;
    While TextObj <> Nil Do
    Begin
        If TextMatches(TextObj.Text, SearchText, CaseSensitive, WholeWord) Then
        Begin
            Inc(MatchCount);
            
            // Calculate what the new text would be
            If WholeWord Then
                NewText := ReplaceWithText
            Else
                NewText := ReplaceTextInString(TextObj.Text, SearchText, ReplaceWithText, CaseSensitive);
            
            ReportList.Add('Match ' + IntToStr(MatchCount) + ':');
            ReportList.Add('  Layer: ' + Layer2String(TextObj.Layer));
            ReportList.Add('  Location: X=' + CoordToDisplayString(TextObj.XLocation) + 
                          ', Y=' + CoordToDisplayString(TextObj.YLocation));
            ReportList.Add('  Current: "' + TextObj.Text + '"');
            ReportList.Add('  After:   "' + NewText + '"');
            ReportList.Add('');
        End;
        
        TextObj := Iterator.NextPCBObject;
    End;
    
    Board.BoardIterator_Destroy(Iterator);
    
    ReportList.Add('=========================================================================');
    ReportList.Add('Total text objects that would be modified: ' + IntToStr(MatchCount));
    
    // Update status label
    FormFindReplaceText.LabelStatus.Caption := IntToStr(MatchCount) + ' text object(s) would be modified.';
    
    // Save and open report
    ReportFile := ChangeFileExt(Board.FileName, '') + '_ReplacePreview.txt';
    ReportList.SaveToFile(ReportFile);
    ReportList.Free;
    
    ReportDoc := Client.OpenDocument('Text', ReportFile);
    If ReportDoc <> Nil Then
        Client.ShowDocument(ReportDoc);
End;

{..............................................................................}
{ Close button click handler                                                   }
{..............................................................................}
Procedure TFormFindReplaceText.ButtonCloseClick(Sender: TObject);
Begin
    Close;
End;

{..............................................................................}
{ Main entry point - launches the Find and Replace dialog                      }
{..............................................................................}
Procedure RunGUI;
Begin
    // Get current PCB board
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then
    Begin
        ShowError('Please open a PCB document first.');
        Exit;
    End;
    
    // Populate the layer combo box
    PopulateLayerComboBox;
    
    // Show the dialog
    FormFindReplaceText.ShowModal;
    
    // Clean up
    If LayerIndexArray <> Nil Then
        LayerIndexArray.Free;
End;

{..............................................................................}
{ Quick Find and Replace - command line style without dialog                   }
{ Useful for scripted batch operations                                         }
{ LayerFilter = 0 means all layers                                             }
{..............................................................................}
Procedure QuickFindReplace(FindText, ReplaceWithText : String; LayerFilter : TLayer; CaseSensitive : Boolean);
Var
    Iterator      : IPCB_BoardIterator;
    TextObj       : IPCB_Text;
    TextObjList   : TInterfaceList;
    ReplaceCount  : Integer;
    NewText       : String;
    I             : Integer;
Begin
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;
    
    If FindText = '' Then Exit;
    
    ReplaceCount := 0;
    TextObjList := TInterfaceList.Create;
    
    // Create iterator
    Iterator := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eTextObject));
    
    If LayerFilter = 0 Then
        Iterator.AddFilter_LayerSet(AllLayers)
    Else
        Iterator.AddFilter_LayerSet(MkSet(LayerFilter));
    
    Iterator.AddFilter_Method(eProcessAll);
    
    // Collect all matching text objects
    TextObj := Iterator.FirstPCBObject;
    While TextObj <> Nil Do
    Begin
        If TextMatches(TextObj.Text, FindText, CaseSensitive, False) Then
            TextObjList.Add(TextObj);
        
        TextObj := Iterator.NextPCBObject;
    End;
    
    Board.BoardIterator_Destroy(Iterator);
    
    // Modify the collected objects
    If TextObjList.Count > 0 Then
    Begin
        Try
            PCBServer.PreProcess;
            
            For I := 0 To TextObjList.Count - 1 Do
            Begin
                TextObj := TextObjList.Items[I];
                
                PCBServer.SendMessageToRobots(TextObj.I_ObjectAddress, c_Broadcast, PCBM_BeginModify, c_NoEventData);
                
                NewText := ReplaceTextInString(TextObj.Text, FindText, ReplaceWithText, CaseSensitive);
                TextObj.Text := NewText;
                
                PCBServer.SendMessageToRobots(TextObj.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_NoEventData);
                
                Inc(ReplaceCount);
            End;
        Finally
            PCBServer.PostProcess;
            TextObjList.Free;
        End;
        
        Board.ViewManager_FullUpdate;
        Client.SendMessage('PCB:Zoom', 'Action=Redraw', 255, Client.CurrentView);
    End
    Else
        TextObjList.Free;
    
    ShowMessage('Replaced text in ' + IntToStr(ReplaceCount) + ' object(s).');
End;

{..............................................................................}
{ Example: Replace all instances of "REV A" with "REV B" on Top Overlay        }
{..............................................................................}
Procedure ExampleReplaceRevision;
Begin
    QuickFindReplace('REV A', 'REV B', eTopOverlay, False);
End;

{..............................................................................}
{ Example: Replace company name on all layers                                  }
{..............................................................................}
Procedure ExampleReplaceCompanyName;
Begin
    QuickFindReplace('Old Company', 'New Company', 0, False);
End;

{..............................................................................}
