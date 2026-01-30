{..............................................................................}
{ Form unit for Results dialog                                                 }
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
    ButtonClose           : TButton;
    SaveDialog            : TSaveDialog;
    procedure ButtonExportClick(Sender: TObject);
    procedure StringGridResultsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGridResultsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditFilterChange(Sender: TObject);
    procedure ButtonClearFilterClick(Sender: TObject);
    procedure ComboBoxFilterColumnChange(Sender: TObject);
    procedure StringGridResultsDblClick(Sender: TObject);
  End;

Const
  mrNavigate = 100;  // Custom modal result for navigation request

Var
  FormResults : TFormResults;

  // Navigation request info (set on double-click)
  NavigateParamName  : String;
  NavigateParamValue : String;
  NavigateRow        : Integer;

  // Sort state variables
  CurrentSortColumn  : Integer;
  SortAscending      : Boolean;
  LastColumnCount    : Integer;
  OriginalHeadersArr : Array[0..50] Of String;

  // Filter state - store all data rows
  AllDataRows        : Array[0..5000, 0..50] Of String;
  AllDataRowCount    : Integer;
  AllDataColCount    : Integer;
  FilteredRowCount   : Integer;

  // Resize detection state
  IsResizing         : Boolean;
  MouseDownX         : Integer;

  // Track clicked cell for double-click
  LastClickedCol     : Integer;
  LastClickedRow     : Integer;

Implementation

{$R *.DFM}

{..............................................................................}
{ Compare two strings for sorting (case-insensitive)                           }
{..............................................................................}
Function CompareStrings(S1, S2 : String; Ascending : Boolean) : Integer;
Begin
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
{ QuickSort partition for grid rows                                            }
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
        If CompareStrings(Grid.Cells[SortCol, J], Pivot, Ascending) <= 0 Then
        Begin
            Inc(I);
            SwapGridRows(Grid, I, J);
        End;
    End;

    SwapGridRows(Grid, I + 1, High);
    Result := I + 1;
End;

{..............................................................................}
{ QuickSort implementation for grid rows                                       }
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
{ Sort the grid by the specified column                                        }
{..............................................................................}
Procedure SortGridByColumn(Grid : TStringGrid; ColIndex : Integer);
Var
    I           : Integer;
    BaseHeader  : String;
    NeedInit    : Boolean;
Begin
    // Check if we need to initialize headers (new data or first time)
    NeedInit := False;
    If LastColumnCount <> Grid.ColCount Then
        NeedInit := True
    Else If LastColumnCount = 0 Then
        NeedInit := True;

    // Initialize original headers if needed
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

    // Toggle sort direction if clicking same column, otherwise sort ascending
    If ColIndex = CurrentSortColumn Then
        SortAscending := Not SortAscending
    Else
    Begin
        CurrentSortColumn := ColIndex;
        SortAscending := True;
    End;

    // Perform the sort (skip header row 0, sort rows 1 to RowCount-1)
    If Grid.RowCount > 2 Then
        QuickSortGrid(Grid, ColIndex, 1, Grid.RowCount - 1, SortAscending);

    // Update column headers to show sort indicator
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
    AllDataRowCount := Grid.RowCount - 1;  // Exclude header row

    // Store all data rows (starting from row 1, skipping header)
    For I := 1 To Grid.RowCount - 1 Do
    Begin
        If I <= 5000 Then
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
{ Apply filter to the grid                                                     }
{..............................................................................}
Procedure ApplyFilter(Grid : TStringGrid; FilterCol : Integer; FilterText : String);
Var
    I, J          : Integer;
    GridRow       : Integer;
    CellValue     : String;
    FilterUpper   : String;
    MatchFound    : Boolean;
Begin
    // Auto-store data if not yet stored or grid has been repopulated
    // (detected when grid has more data rows than we have stored)
    If (AllDataRowCount = 0) Or ((Grid.RowCount - 1) > AllDataRowCount) Then
    Begin
        // Sort by first column BEFORE storing (so stored data is sorted)
        SortGridByColumn(Grid, 0);
        StoreAllData(Grid);
    End;

    FilterUpper := UpperCase(FilterText);
    GridRow := 1;  // Start after header

    // If no filter, show all rows
    If FilterText = '' Then
    Begin
        Grid.RowCount := AllDataRowCount + 1;
        For I := 0 To AllDataRowCount - 1 Do
        Begin
            For J := 0 To AllDataColCount - 1 Do
            Begin
                If J <= 50 Then
                    Grid.Cells[J, I + 1] := AllDataRows[I, J];
            End;
        End;
        FilteredRowCount := AllDataRowCount;
    End
    Else
    Begin
        // Apply filter
        For I := 0 To AllDataRowCount - 1 Do
        Begin
            MatchFound := False;

            If FilterCol < 0 Then
            Begin
                // Search all columns
                For J := 0 To AllDataColCount - 1 Do
                Begin
                    If J <= 50 Then
                    Begin
                        CellValue := UpperCase(AllDataRows[I, J]);
                        If Pos(FilterUpper, CellValue) > 0 Then
                        Begin
                            MatchFound := True;
                            Break;
                        End;
                    End;
                End;
            End
            Else
            Begin
                // Search specific column
                If FilterCol <= 50 Then
                Begin
                    CellValue := UpperCase(AllDataRows[I, FilterCol]);
                    If Pos(FilterUpper, CellValue) > 0 Then
                        MatchFound := True;
                End;
            End;

            If MatchFound Then
            Begin
                // Ensure grid has enough rows
                If GridRow >= Grid.RowCount Then
                    Grid.RowCount := GridRow + 1;

                // Copy matching row to grid
                For J := 0 To AllDataColCount - 1 Do
                Begin
                    If J <= 50 Then
                        Grid.Cells[J, GridRow] := AllDataRows[I, J];
                End;
                Inc(GridRow);
            End;
        End;

        // Set final row count (at least 2 to keep header visible)
        If GridRow > 1 Then
            Grid.RowCount := GridRow
        Else
            Grid.RowCount := 2;

        FilteredRowCount := GridRow - 1;
    End;

    // Update count label
    If FilterText = '' Then
        FormResults.LabelCount.Caption := 'Unique parts: ' + IntToStr(AllDataRowCount)
    Else
        FormResults.LabelCount.Caption := 'Showing: ' + IntToStr(FilteredRowCount) + ' of ' + IntToStr(AllDataRowCount);

    // Reset sort state when filter changes
    LastColumnCount := 0;
End;

{..............................................................................}
{ Check if X coordinate is near a column border (resize zone)                  }
{..............................................................................}
Function IsNearColumnBorder(Grid : TStringGrid; X : Integer) : Boolean;
Const
    ResizeZone = 5;  // Pixels from column edge to consider as resize zone
Var
    ColLeft   : Integer;
    ColRight  : Integer;
    I         : Integer;
Begin
    Result := False;
    ColLeft := 0;

    For I := 0 To Grid.ColCount - 1 Do
    Begin
        ColRight := ColLeft + Grid.ColWidths[I];

        // Check if X is near the right edge of this column
        If (X >= ColRight - ResizeZone) And (X <= ColRight + ResizeZone) Then
        Begin
            Result := True;
            Exit;
        End;

        ColLeft := ColRight + 1;  // +1 for grid line
    End;
End;

{..............................................................................}
{ Mouse down event handler for grid - detect resize operations                 }
{..............................................................................}
Procedure TFormResults.StringGridResultsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
    Col, Row : Integer;
Begin
    // Only check for left mouse button
    If Button <> mbLeft Then Exit;

    // Store mouse position for movement detection
    MouseDownX := X;

    // Get the cell at the click position
    StringGridResults.MouseToCell(X, Y, Col, Row);

    // Store clicked cell for double-click handler
    LastClickedCol := Col;
    LastClickedRow := Row;

    // Check if click was on header row and near a column border
    If Row = 0 Then
        IsResizing := IsNearColumnBorder(StringGridResults, X)
    Else
        IsResizing := False;
End;

{..............................................................................}
{ Mouse up event handler for grid - detect header clicks for sorting           }
{..............................................................................}
Procedure TFormResults.StringGridResultsMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
    Col, Row : Integer;
Begin
    // Only respond to left mouse button clicks
    If Button <> mbLeft Then Exit;

    // Skip sorting if this was a resize operation or mouse moved significantly
    If IsResizing Or (Abs(X - MouseDownX) > 3) Then
    Begin
        IsResizing := False;
        Exit;
    End;

    // Get the cell at the click position
    StringGridResults.MouseToCell(X, Y, Col, Row);

    // Check if click was on header row (row 0) and not near a column border
    If (Row = 0) And (Col >= 0) And (Not IsNearColumnBorder(StringGridResults, X)) Then
        SortGridByColumn(StringGridResults, Col);

    IsResizing := False;
End;

{..............................................................................}
{ Filter text changed event handler                                            }
{..............................................................................}
Procedure TFormResults.EditFilterChange(Sender: TObject);
Var
    FilterCol : Integer;
Begin
    // Get selected column index (-1 for "All Columns")
    FilterCol := ComboBoxFilterColumn.ItemIndex - 1;
    ApplyFilter(StringGridResults, FilterCol, EditFilter.Text);
End;

{..............................................................................}
{ Filter column changed event handler                                          }
{..............................................................................}
Procedure TFormResults.ComboBoxFilterColumnChange(Sender: TObject);
Begin
    // Re-apply filter with new column
    If EditFilter.Text <> '' Then
        EditFilterChange(Sender);
End;

{..............................................................................}
{ Clear filter button click handler                                            }
{..............................................................................}
Procedure TFormResults.ButtonClearFilterClick(Sender: TObject);
Begin
    EditFilter.Text := '';
    ComboBoxFilterColumn.ItemIndex := 0;
    ApplyFilter(StringGridResults, -1, '');
End;

{..............................................................................}
{ Double-click event handler for grid - navigate to component                  }
{..............................................................................}
Procedure TFormResults.StringGridResultsDblClick(Sender: TObject);
Var
    HeaderName  : String;
Begin
    // Use cell position stored from MouseDown event
    // Ignore clicks on header row or invalid positions
    If (LastClickedRow <= 0) Or (LastClickedCol < 0) Then Exit;

    // Get the parameter name from header (remove sort indicators)
    HeaderName := StringGridResults.Cells[LastClickedCol, 0];
    If Pos(' [A-Z]', HeaderName) > 0 Then
        HeaderName := Copy(HeaderName, 1, Pos(' [A-Z]', HeaderName) - 1);
    If Pos(' [Z-A]', HeaderName) > 0 Then
        HeaderName := Copy(HeaderName, 1, Pos(' [Z-A]', HeaderName) - 1);

    // Store navigation info
    NavigateParamName := HeaderName;
    NavigateParamValue := StringGridResults.Cells[LastClickedCol, LastClickedRow];
    NavigateRow := LastClickedRow;

    // Close form with navigation result
    ModalResult := mrNavigate;
End;

{..............................................................................}
{ Export button click handler                                                  }
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
                    If J > 0 Then
                        Line := Line + ',';
                    CellValue := StringGridResults.Cells[J, I];
                    // Remove sort indicators from header for export
                    If I = 0 Then
                    Begin
                        If Pos(' [A-Z]', CellValue) > 0 Then
                            CellValue := Copy(CellValue, 1, Pos(' [A-Z]', CellValue) - 1);
                        If Pos(' [Z-A]', CellValue) > 0 Then
                            CellValue := Copy(CellValue, 1, Pos(' [Z-A]', CellValue) - 1);
                    End;
                    CellValue := StringReplace(CellValue, '"', '""', [rfReplaceAll]);
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
