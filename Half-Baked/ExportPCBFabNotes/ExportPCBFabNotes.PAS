{..............................................................................}
{ Summary: Exports PCB Fab Notes from text objects to a formatted text file    }
{          Converts X/Y coordinates to spaces and lines in text file           }
{ Version 1.0                                                                   }
{ Based on user requirements for spatial text arrangement                      }
{..............................................................................}

{..............................................................................}
Function StartsWithNumberAndPeriod(Text : String) : Boolean;
Var
    I : Integer;
    HasNumber : Boolean;
Begin
    Result := False;
    HasNumber := False;

    // Check if starts with number(s) followed by period
    For I := 1 to Length(Text) Do
    Begin
        If (Text[I] >= '0') And (Text[I] <= '9') Then
            HasNumber := True
        Else If (Text[I] = '.') And HasNumber Then
        Begin
            Result := True;
            Break;
        End
        Else If Text[I] <> ' ' Then  // Allow leading spaces
            Break;
    End;
End;
{..............................................................................}

{..............................................................................}
Function GetNumberFromNote(NoteText : String) : Integer;
Var
    I : Integer;
    NumStr : String;
Begin
    Result := -1;
    NumStr := '';

    // Extract number before the period
    For I := 1 to Length(NoteText) Do
    Begin
        If NoteText[I] = '.' Then
        Begin
            If NumStr <> '' Then
            Begin
                Result := StrToInt(NumStr);
            End;
            Break;
        End
        Else If (NoteText[I] >= '0') And (NoteText[I] <= '9') Then
            NumStr := NumStr + NoteText[I]
        Else
            Break;
    End;
End;
{..............................................................................}

{..............................................................................}
Function CreateSpaces(Count : Integer) : String;
Var
    I : Integer;
Begin
    Result := '';
    For I := 1 to Count Do
        Result := Result + ' ';
End;
{..............................................................................}

{..............................................................................}
Procedure Run;
Var
    Board           : IPCB_Board;
    Text            : IPCB_Text;
    Iterator        : IPCB_BoardIterator;

    // Lists to store note data
    NoteTexts       : TStringList;
    NoteXCoords     : TStringList;
    NoteYCoords     : TStringList;
    NoteSizes       : TStringList;

    // Variables for processing
    I, J            : Integer;
    OriginX, OriginY: Float;
    CharWidth       : Float;
    FirstNoteSize   : Float;
    Note1X          : Float;
    Note1Y          : Float;
    Note1Found      : Boolean;

    // For output
    OutputList      : TStringList;
    FileName        : String;
    CurrentLine     : String;
    SpaceCount      : Integer;
    LastY           : Float;
    LineGap         : Float;
    BlankLines      : Integer;

    // For sorting
    TempText        : String;
    TempX           : String;
    TempY           : String;
    TempSize        : String;
    CurrentY        : Float;
    CompareY        : Float;

    // Temp variables
    CurrentX        : Float;
    CurrentSize     : Float;
    NoteNumber      : Integer;
    MinX            : Float;
    MaxY            : Float;
Begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then
    Begin
        ShowMessage('No PCB board found!');
        Exit;
    End;

    // Initialize string lists
    NoteTexts := TStringList.Create;
    NoteXCoords := TStringList.Create;
    NoteYCoords := TStringList.Create;
    NoteSizes := TStringList.Create;

    Note1Found := False;
    Note1X := 0;
    Note1Y := 0;
    FirstNoteSize := 50; // Default in mils

    // Create the iterator to look for Text objects on Mechanical2 layer
    Iterator := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eTextObject));
    Iterator.AddFilter_LayerSet(MkSet(eMechanical2));
    Iterator.AddFilter_Method(eProcessAll);

    // Collect all fab notes
    Text := Iterator.FirstPCBObject;
    While (Text <> Nil) Do
    Begin
        If Not(Text.InComponent) And (Text.IsFreePrimitive) Then
        Begin
            // Store text data
            NoteTexts.Add(Text.Text);
            CurrentX := CoordToMils(Text.XLocation - Board.XOrigin);
            CurrentY := CoordToMils(Text.YLocation - Board.YOrigin);
            CurrentSize := CoordToMils(Text.Size);

            NoteXCoords.Add(FloatToStr(CurrentX));
            NoteYCoords.Add(FloatToStr(CurrentY));
            NoteSizes.Add(FloatToStr(CurrentSize));

            // Check if this is note "1."
            NoteNumber := GetNumberFromNote(Text.Text);
            If NoteNumber = 1 Then
            Begin
                Note1Found := True;
                Note1X := CurrentX;
                Note1Y := CurrentY;
                FirstNoteSize := CurrentSize;
            End;
        End;
        Text := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);

    If NoteTexts.Count = 0 Then
    Begin
        ShowMessage('No fab notes found on Mechanical2 layer!');
        NoteTexts.Free;
        NoteXCoords.Free;
        NoteYCoords.Free;
        NoteSizes.Free;
        Exit;
    End;

    // Set X origin from Note 1 if it exists, otherwise use leftmost numbered note
    If Note1Found Then
        OriginX := Note1X
    Else
    Begin
        // Fallback: find leftmost numbered note
        MinX := 99999;
        For I := 0 to NoteTexts.Count - 1 Do
        Begin
            If StartsWithNumberAndPeriod(NoteTexts.Strings[I]) Then
            Begin
                CurrentX := StrToFloat(NoteXCoords.Strings[I]);
                If CurrentX < MinX Then
                    MinX := CurrentX;
            End;
        End;
        // If no numbered notes found, use leftmost of all notes
        If MinX = 99999 Then
        Begin
            MinX := StrToFloat(NoteXCoords.Strings[0]);
            For I := 1 to NoteXCoords.Count - 1 Do
            Begin
                CurrentX := StrToFloat(NoteXCoords.Strings[I]);
                If CurrentX < MinX Then
                    MinX := CurrentX;
            End;
        End;
        OriginX := MinX;
    End;

    // Find Y origin from Note 1 if it exists, otherwise use highest Y (since we're sorting descending)
    If Note1Found Then
        OriginY := Note1Y
    Else
    Begin
        MaxY := StrToFloat(NoteYCoords.Strings[0]);
        For I := 1 to NoteYCoords.Count - 1 Do
        Begin
            CurrentY := StrToFloat(NoteYCoords.Strings[I]);
            If CurrentY > MaxY Then
                MaxY := CurrentY;
        End;
        OriginY := MaxY;
    End;

    // Calculate character width and line spacing
    CharWidth := FirstNoteSize;  // Use size directly for spacing calculation
    LineGap := FirstNoteSize * 1.5;    // Line spacing

    // Simple bubble sort by Y coordinate (descending order - highest Y first)
    For I := 0 to NoteTexts.Count - 2 Do
    Begin
        For J := 0 to NoteTexts.Count - 2 - I Do
        Begin
            CurrentY := StrToFloat(NoteYCoords.Strings[J]);
            CompareY := StrToFloat(NoteYCoords.Strings[J + 1]);

            If CurrentY < CompareY Then
            Begin
                // Swap all related data
                TempText := NoteTexts.Strings[J];
                TempX := NoteXCoords.Strings[J];
                TempY := NoteYCoords.Strings[J];
                TempSize := NoteSizes.Strings[J];

                NoteTexts.Strings[J] := NoteTexts.Strings[J + 1];
                NoteXCoords.Strings[J] := NoteXCoords.Strings[J + 1];
                NoteYCoords.Strings[J] := NoteYCoords.Strings[J + 1];
                NoteSizes.Strings[J] := NoteSizes.Strings[J + 1];

                NoteTexts.Strings[J + 1] := TempText;
                NoteXCoords.Strings[J + 1] := TempX;
                NoteYCoords.Strings[J + 1] := TempY;
                NoteSizes.Strings[J + 1] := TempSize;
            End;
        End;
    End;

    // Create output string list
    OutputList := TStringList.Create;

    OutputList.Add('PCB Fabrication Notes');
    OutputList.Add('======================');
    OutputList.Add('');

    LastY := OriginY;

    // Process each note
    For I := 0 to NoteTexts.Count - 1 Do
    Begin
        CurrentX := StrToFloat(NoteXCoords.Strings[I]);
        CurrentY := StrToFloat(NoteYCoords.Strings[I]);

        // Calculate blank lines based on Y gap (reversed since we're going high to low)
        BlankLines := Round((LastY - CurrentY) / LineGap) - 1;
        If BlankLines < 0 Then BlankLines := 0;

        // Add blank lines for vertical spacing
        For J := 1 to BlankLines Do
            OutputList.Add('');

        // Calculate horizontal spacing based on offset from note "1." position
        // Example: if text is 300 mils to the right and size is 50, we get 6 spaces
        SpaceCount := Round((CurrentX - OriginX) / CharWidth);
        If SpaceCount < 0 Then SpaceCount := 0;

        // Create line with appropriate spacing
        CurrentLine := CreateSpaces(SpaceCount) + NoteTexts.Strings[I];
        OutputList.Add(CurrentLine);

        LastY := CurrentY;
    End;

    // Add footer
    OutputList.Add('');
    OutputList.Add('======================');
    OutputList.Add('Total Notes: ' + IntToStr(NoteTexts.Count));
    OutputList.Add('Exported: ' + DateTimeToStr(Now));

    // Save to file
    FileName := ChangeFileExt(Board.FileName, '_FabNotes.txt');
    OutputList.SaveToFile(FileName);

    ShowMessage('Fab notes exported successfully to:' + #13#10 + FileName + #13#10 +
               'Total notes exported: ' + IntToStr(NoteTexts.Count));

    // Clean up
    OutputList.Free;
    NoteTexts.Free;
    NoteXCoords.Free;
    NoteYCoords.Free;
    NoteSizes.Free;
End;
{..............................................................................}
