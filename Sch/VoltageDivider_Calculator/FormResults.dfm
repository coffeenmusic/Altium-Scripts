object FormResults: TFormResults
  Left = 150
  Top = 80
  BorderStyle = bsSizeable
  Caption = 'Voltage Divider Results'
  ClientHeight = 500
  ClientWidth = 860
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LabelFilter: TLabel
    Left = 8
    Top = 12
    Width = 50
    Height = 13
    Caption = 'Filter by:'
  end
  object LabelCount: TLabel
    Left = 8
    Top = 470
    Width = 60
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Results: 0'
  end
  object LabelSortHint: TLabel
    Left = 150
    Top = 470
    Width = 300
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '(Click column header to sort)'
    Font.Color = clGray
    Font.Style = []
    ParentFont = False
  end
  object ComboBoxFilterColumn: TComboBox
    Left = 65
    Top = 8
    Width = 130
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = ComboBoxFilterColumnChange
  end
  object EditFilter: TEdit
    Left = 205
    Top = 8
    Width = 180
    Height = 21
    TabOrder = 1
    OnChange = EditFilterChange
  end
  object ButtonClearFilter: TButton
    Left = 395
    Top = 8
    Width = 60
    Height = 21
    Caption = 'Clear'
    TabOrder = 2
    OnClick = ButtonClearFilterClick
  end
  object StringGridResults: TStringGrid
    Left = 8
    Top = 38
    Width = 844
    Height = 420
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 11
    DefaultColWidth = 75
    DefaultRowHeight = 20
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    TabOrder = 3
    OnDblClick = StringGridResultsDblClick
    OnMouseDown = StringGridResultsMouseDown
    OnMouseUp = StringGridResultsMouseUp
  end
  object ButtonExport: TButton
    Left = 568
    Top = 466
    Width = 90
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Export to CSV'
    TabOrder = 4
    OnClick = ButtonExportClick
  end
  object ButtonClose: TButton
    Left = 770
    Top = 466
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 5
  end
  object ButtonBack: TButton
    Left = 670
    Top = 466
    Width = 90
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Back'
    ModalResult = 3
    TabOrder = 6
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'csv'
    Filter = 'CSV Files (*.csv)|*.csv|All Files (*.*)|*.*'
    Title = 'Export Results to CSV'
    Left = 720
    Top = 462
  end
  object FilterTimer: TTimer
    Enabled = False
    Interval = 300
    OnTimer = FilterTimerTimer
    Left = 760
    Top = 462
  end
end
