object FormResults: TFormResults
  Left = 150
  Top = 80
  BorderStyle = bsSizeable
  Caption = 'Unique Parts List'
  ClientHeight = 530
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
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
    Top = 495
    Width = 100
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Unique parts: 0'
  end
  object LabelSortHint: TLabel
    Left = 120
    Top = 495
    Width = 350
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '(Click header to sort, double-click cell to navigate to component)'
    Font.Color = clGray
    Font.Style = []
    ParentFont = False
  end
  object ComboBoxFilterColumn: TComboBox
    Left = 65
    Top = 8
    Width = 150
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = ComboBoxFilterColumnChange
  end
  object EditFilter: TEdit
    Left = 225
    Top = 8
    Width = 200
    Height = 21
    TabOrder = 1
    OnChange = EditFilterChange
  end
  object ButtonClearFilter: TButton
    Left = 435
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
    Width = 784
    Height = 440
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 2
    DefaultColWidth = 150
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
    Left = 512
    Top = 490
    Width = 90
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Export to CSV'
    TabOrder = 4
    OnClick = ButtonExportClick
  end
  object ButtonRefresh: TButton
    Left = 608
    Top = 490
    Width = 90
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Refresh Data'
    TabOrder = 5
    OnClick = ButtonRefreshClick
  end
  object ButtonClose: TButton
    Left = 704
    Top = 490
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 6
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'csv'
    Filter = 'CSV Files (*.csv)|*.csv|All Files (*.*)|*.*'
    Title = 'Export to CSV'
    Left = 560
    Top = 486
  end
  object FilterTimer: TTimer
    Enabled = False
    Interval = 300
    OnTimer = FilterTimerTimer
    Left = 600
    Top = 486
  end
end
