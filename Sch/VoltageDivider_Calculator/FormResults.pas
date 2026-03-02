{..............................................................................}
{ Form unit for Voltage Divider results display                                }
{..............................................................................}

Unit FormResultsUnit;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids;

Type
  TFormResults = class(TForm)
    LabelFilter           : TLabel;
    LabelCount            : TLabel;
    LabelSortHint         : TLabel;
    ComboBoxFilterColumn  : TComboBox;
    EditFilter            : TEdit;
    ButtonClearFilter     : TButton;
    StringGridResults     : TStringGrid;
    ButtonExport          : TButton;
    ButtonBack            : TButton;
    ButtonClose           : TButton;
    SaveDialog            : TSaveDialog;
    FilterTimer           : TTimer;
    procedure ButtonExportClick(Sender: TObject);
    procedure StringGridResultsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGridResultsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGridResultsDblClick(Sender: TObject);
    procedure EditFilterChange(Sender: TObject);
    procedure FilterTimerTimer(Sender: TObject);
    procedure ButtonClearFilterClick(Sender: TObject);
    procedure ComboBoxFilterColumnChange(Sender: TObject);
  End;

Var
  FormResults : TFormResults;

  // Sort state
  CurrentSortColumn  : Integer;
  SortAscending      : Boolean;
  LastColumnCount    : Integer;
  OriginalHeadersArr : Array[0..50] Of String;

  // Filter state
  AllDataRows        : Array[0..10000, 0..50] Of String;
  AllDataRowCount    : Integer;
  AllDataColCount    : Integer;

  // Resize detection
  IsResizing         : Boolean;
  MouseDownX         : Integer;

  // Clicked cell tracking
  LastClickedCol     : Integer;
  LastClickedRow     : Integer;

Implementation

{$R *.DFM}

{..............................................................................}
{ Compare function for sorting - tries numeric first, falls back to string    }
{..............................................................................}
Function CompareValues(S1, S2 : String; Ascending : Boolean) : Integer;
Var
    V1, V2 : Double;
Begin
    // Try numeric comparison first
    V1 := StrToFloatDef(S1, -1e30);
    V2 := StrToFloatDef(S2, -1e30);

    If (V1 > -1e29) And (V2 > -1e29) Then
    Begin
        If V1 < V2 Then Result := -1
        Else If V1 > V2 Then Result := 1
        Else Result := 0;
    End
    Else
        Result := CompareText(S1, S2);

    If Not Ascending Then
        Result := -Result;
End;

{..............................................................................}
{ Swap two rows in the grid                                                    }
{..............................................................................}
Procedure SwapGridRows(Grid : TStringGrid; Row1, Row2 : Integer);
Var
    Col   : Integer;
    Temp  : String;
Begin
    For Col := 0 To Grid.ColCount - 1 Do
    Begin
        Temp := Grid.Cells[Col, Row1];
        Grid.Cells[Col, Row1] := Grid.Cells[Col, Row2];
        Grid.Cells[Col, Row2] := Temp;
    End;
End;

{..............................................................................}
{ QuickSort partition                                                          }
{..............................................................................}
Function PartitionGrid(Grid : TStringGrid; SortCol, Low, High : Integer; Ascending : Boolean) : Integer;
Var
    Pivot : String;
    I, J  : Integer;
Begin
    Pivot := Grid.Cells[SortCol, High];
    I := Low - 1;

    For J := Low To High - 1 Do
    Begin
        If CompareValues(Grid.Cells[SortCol, J], Pivot, Ascending) <= 0 Then
        Begin
            Inc(I);
            SwapGridRows(Grid, I, J);
        End;
    End;

    SwapGridRows(Grid, I + 1, High);
    Result := I + 1;
End;

{..............................................................................}
{ QuickSort implementation                                                     }
{..............................................................................}
Procedure QuickSortGrid(Grid : TStringGrid; SortCol, Low, High : Integer; Ascending : Boolean);
Var
    PivotIndex : Integer;
Begin
    If Low < High Then
    Begin
        PivotIndex := PartitionGrid(Grid, SortCol, Low, High, Ascending);
        QuickSortGrid(Grid, SortCol, Low, PivotIndex - 1, Ascending);
        QuickSortGrid(Grid, SortCol, PivotIndex + 1, High, Ascending);
    End;
End;

{..............................................................................}
{ Sort the grid by clicking a column header                                    }
{..............................................................................}
Procedure SortGridByColumn(Grid : TStringGrid; ColIndex : Integer);
Var
    I           : Integer;
    BaseHeader  : String;
    NeedInit    : Boolean;
Begin
    NeedInit := False;
    If LastColumnCount <> Grid.ColCount Then NeedInit := True
    Else If LastColumnCount = 0 Then NeedInit := True;

    If NeedInit Then
    Begin
        LastColumnCount := Grid.ColCount;
        CurrentSortColumn := -1;
        SortAscending := True;
        For I := 0 To Grid.ColCount - 1 Do
        Begin
            If I <= 50 Then
                OriginalHeadersArr[I] := Grid.Cells[I, 0];
        End;
    End;

    If ColIndex = CurrentSortColumn Then
        SortAscending := Not SortAscending
    Else
    Begin
        CurrentSortColumn := ColIndex;
        SortAscending := True;
    End;

    If Grid.RowCount > 2 Then
        QuickSortGrid(Grid, ColIndex, 1, Grid.RowCount - 1, SortAscending);

    For I := 0 To Grid.ColCount - 1 Do
    Begin
        If I <= 50 Then
            BaseHeader := OriginalHeadersArr[I]
        Else
            BaseHeader := Grid.Cells[I, 0];

        If I = ColIndex Then
        Begin
            If SortAscending Then
                Grid.Cells[I, 0] := BaseHeader + ' [A-Z]'
            Else
                Grid.Cells[I, 0] := BaseHeader + ' [Z-A]';
        End
        Else
            Grid.Cells[I, 0] := BaseHeader;
    End;
End;

{..............................................................................}
{ Store all data rows for filtering                                            }
{..............................................................................}
Procedure StoreAllData(Grid : TStringGrid);
Var
    I, J : Integer;
Begin
    AllDataColCount := Grid.ColCount;
    AllDataRowCount := Grid.RowCount - 1;

    For I := 1 To Grid.RowCount - 1 Do
    Begin
        If (I - 1) <= 10000 Then
        Begin
            For J := 0 To Grid.ColCount - 1 Do
            Begin
                If J <= 50 Then
                    AllDataRows[I - 1, J] := Grid.Cells[J, I];
            End;
        End;
    End;
End;

{..............................................................................}
{ Apply text filter to the grid                                                }
{..............................................................................}
Procedure ApplyFilter(Grid : TStringGrid; FilterCol : Integer; FilterText : String);
Var
    I, J          : Integer;
    GridRow       : Integer;
    FilterUpper   : String;
    MatchFound    : Boolean;
Begin
    If (AllDataRowCount = 0) Or ((Grid.RowCount - 1) > AllDataRowCount) Then
    Begin
        SortGridByColumn(Grid, 0);
        StoreAllData(Grid);
    End;

    FilterUpper := UpperCase(FilterText);
    GridRow := 1;

    If FilterText = '' Then
    Begin
        Grid.RowCount := AllDataRowCount + 1;
        For I := 0 To AllDataRowCount - 1 Do
            For J := 0 To AllDataColCount - 1 Do
                If J <= 50 Then
                    Grid.Cells[J, I + 1] := AllDataRows[I, J];
    End
    Else
    Begin
        Grid.RowCount := AllDataRowCount + 1;

        For I := 0 To AllDataRowCount - 1 Do
        Begin
            MatchFound := False;

            If FilterCol < 0 Then
            Begin
                For J := 0 To AllDataColCount - 1 Do
                    If J <= 50 Then
                        If Pos(FilterUpper, UpperCase(AllDataRows[I, J])) > 0 Then
                        Begin
                            MatchFound := True;
                            Break;
                        End;
            End
            Else
            Begin
                If FilterCol <= 50 Then
                    If Pos(FilterUpper, UpperCase(AllDataRows[I, FilterCol])) > 0 Then
                        MatchFound := True;
            End;

            If MatchFound Then
            Begin
                For J := 0 To AllDataColCount - 1 Do
                    If J <= 50 Then
                        Grid.Cells[J, GridRow] := AllDataRows[I, J];
                Inc(GridRow);
            End;
        End;

        If GridRow > 1 Then
            Grid.RowCount := GridRow
        Else
            Grid.RowCount := 2;
    End;

    If FilterText = '' Then
        FormResults.LabelCount.Caption := 'Results: ' + IntToStr(AllDataRowCount)
    Else
        FormResults.LabelCount.Caption := 'Showing: ' + IntToStr(GridRow - 1) + ' of ' + IntToStr(AllDataRowCount);

    LastColumnCount := 0;
End;

{..............................................................................}
{ Check if near column border for resize detection                             }
{..............................................................................}
Function IsNearColumnBorder(Grid : TStringGrid; X : Integer) : Boolean;
Const
    ResizeZone = 5;
Var
    ColLeft, ColRight, I : Integer;
Begin
    Result := False;
    ColLeft := 0;
    For I := 0 To Grid.ColCount - 1 Do
    Begin
        ColRight := ColLeft + Grid.ColWidths[I];
        If (X >= ColRight - ResizeZone) And (X <= ColRight + ResizeZone) Then
        Begin
            Result := True;
            Exit;
        End;
        ColLeft := ColRight + 1;
    End;
End;

{..............................................................................}
Procedure TFormResults.StringGridResultsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
    Col, Row : Integer;
Begin
    If Button <> mbLeft Then Exit;
    MouseDownX := X;
    StringGridResults.MouseToCell(X, Y, Col, Row);
    LastClickedCol := Col;
    LastClickedRow := Row;
    If Row = 0 Then
        IsResizing := IsNearColumnBorder(StringGridResults, X)
    Else
        IsResizing := False;
End;

{..............................................................................}
Procedure TFormResults.StringGridResultsMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
    Col, Row : Integer;
Begin
    If Button <> mbLeft Then Exit;
    If IsResizing Or (Abs(X - MouseDownX) > 3) Then
    Begin
        IsResizing := False;
        Exit;
    End;
    StringGridResults.MouseToCell(X, Y, Col, Row);
    If (Row = 0) And (Col >= 0) And (Not IsNearColumnBorder(StringGridResults, X)) Then
        SortGridByColumn(StringGridResults, Col);
    IsResizing := False;
End;

{..............................................................................}
Procedure TFormResults.StringGridResultsDblClick(Sender: TObject);
Begin
    // Reserved for future navigation support
End;

{..............................................................................}
Procedure TFormResults.EditFilterChange(Sender: TObject);
Begin
    FilterTimer.Enabled := False;
    FilterTimer.Enabled := True;
End;

{..............................................................................}
Procedure TFormResults.FilterTimerTimer(Sender: TObject);
Var
    FilterCol : Integer;
Begin
    FilterTimer.Enabled := False;
    FilterCol := ComboBoxFilterColumn.ItemIndex - 1;
    ApplyFilter(StringGridResults, FilterCol, EditFilter.Text);
End;

{..............................................................................}
Procedure TFormResults.ComboBoxFilterColumnChange(Sender: TObject);
Begin
    If EditFilter.Text <> '' Then
        EditFilterChange(Sender);
End;

{..............................................................................}
Procedure TFormResults.ButtonClearFilterClick(Sender: TObject);
Begin
    EditFilter.Text := '';
    ComboBoxFilterColumn.ItemIndex := 0;
    ApplyFilter(StringGridResults, -1, '');
End;

{..............................................................................}
Procedure TFormResults.ButtonExportClick(Sender: TObject);
Var
    CSVFile   : TStringList;
    I, J      : Integer;
    Line      : String;
    CellValue : String;
Begin
    If SaveDialog.Execute Then
    Begin
        CSVFile := TStringList.Create;
        Try
            For I := 0 To StringGridResults.RowCount - 1 Do
            Begin
                Line := '';
                For J := 0 To StringGridResults.ColCount - 1 Do
                Begin
                    If J > 0 Then Line := Line + ',';
                    CellValue := StringGridResults.Cells[J, I];
                    // Strip sort indicators from headers
                    If I = 0 Then
                    Begin
                        If Pos(' [A-Z]', CellValue) > 0 Then
                            CellValue := Copy(CellValue, 1, Pos(' [A-Z]', CellValue) - 1);
                        If Pos(' [Z-A]', CellValue) > 0 Then
                            CellValue := Copy(CellValue, 1, Pos(' [Z-A]', CellValue) - 1);
                    End;
                    CellValue := StringReplace(CellValue, '"', '""', rfReplaceAll);
                    Line := Line + '"' + CellValue + '"';
                End;
                CSVFile.Add(Line);
            End;
            CSVFile.SaveToFile(SaveDialog.FileName);
            ShowMessage('Export complete: ' + SaveDialog.FileName);
        Finally
            CSVFile.Free;
        End;
    End;
End;

End.
