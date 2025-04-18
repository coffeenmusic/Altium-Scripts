object Form_LayoutDuplicator: TForm_LayoutDuplicator
  Left = 0
  Top = 0
  Caption = 'Form_LayoutDuplicator'
  ClientHeight = 584
  ClientWidth = 919
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 220
    Top = 454
    Width = 15
    Height = 13
    Caption = 'OR'
  end
  object lbSource: TListBox
    Left = 16
    Top = 16
    Width = 128
    Height = 400
    DragMode = dmAutomatic
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = lbSourceDblClick
    OnEndDrag = lbSourceEndDrag
    OnStartDrag = lbSourceStartDrag
  end
  object lbDestination: TListBox
    Left = 464
    Top = 16
    Width = 128
    Height = 400
    DragMode = dmAutomatic
    ItemHeight = 13
    TabOrder = 1
    OnDblClick = lbDestinationDblClick
    OnEndDrag = lbDestinationEndDrag
    OnStartDrag = lbDestinationStartDrag
  end
  object btnRun: TButton
    Left = 831
    Top = 547
    Width = 75
    Height = 25
    Caption = 'Run'
    TabOrder = 2
    Visible = False
    OnClick = btnRunClick
  end
  object btnSource: TButton
    Left = 16
    Top = 427
    Width = 440
    Height = 25
    Caption = 'Use Preselected Source Components'
    TabOrder = 3
    OnClick = btnSourceClick
  end
  object btnDestination: TButton
    Left = 464
    Top = 427
    Width = 440
    Height = 25
    Caption = 'Select Destination Components'
    TabOrder = 4
    Visible = False
    OnClick = btnDestinationClick
  end
  object btnSourceSelect: TButton
    Left = 16
    Top = 467
    Width = 440
    Height = 25
    Caption = 'Select Source Components'
    TabOrder = 5
    OnClick = btnSourceSelectClick
  end
  object ProgressBar1: TProgressBar
    Left = 16
    Top = 520
    Width = 888
    Height = 17
    TabOrder = 6
  end
  object lbSourceDesc: TListBox
    Left = 151
    Top = 16
    Width = 305
    Height = 400
    Enabled = False
    ItemHeight = 13
    TabOrder = 7
  end
  object lbDestinationDesc: TListBox
    Left = 599
    Top = 16
    Width = 305
    Height = 400
    Enabled = False
    ItemHeight = 13
    TabOrder = 8
  end
end
