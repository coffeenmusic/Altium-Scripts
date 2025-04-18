object FormAddBumps: TFormAddBumps
  Left = 0
  Top = 0
  Caption = 'Auto Add Differential Pair Bumps to Selected Tracks'
  ClientHeight = 501
  ClientWidth = 670
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnRun: TButton
    Left = 16
    Top = 402
    Width = 164
    Height = 25
    Caption = 'Add Bumps To Selected'
    TabOrder = 0
    OnClick = btnRunClick
  end
  object cbSmartPlace: TCheckBox
    Left = 14
    Top = 14
    Width = 162
    Height = 17
    Caption = 'Smart Place (Takes Longer)'
    TabOrder = 1
  end
  object MemoReport: TMemo
    Left = 10
    Top = 42
    Width = 646
    Height = 342
    Enabled = False
    Lines.Strings = (
      'MemoReport')
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object btnGetReport: TButton
    Left = 505
    Top = 402
    Width = 151
    Height = 25
    Caption = 'Get Final Lengths'
    TabOrder = 3
    Visible = False
    OnClick = btnGetReportClick
  end
  object ProgressBar1: TProgressBar
    Left = 12
    Top = 442
    Width = 644
    Height = 22
    TabOrder = 4
  end
end
