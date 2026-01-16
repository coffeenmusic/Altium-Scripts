object FormFindReplaceText: TFormFindReplaceText
  Left = 200
  Top = 150
  BorderStyle = bsDialog
  Caption = 'Find and Replace Text Objects'
  ClientHeight = 280
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LabelFind: TLabel
    Left = 16
    Top = 20
    Width = 58
    Height = 13
    Caption = 'Find Text:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelReplace: TLabel
    Left = 16
    Top = 68
    Width = 82
    Height = 13
    Caption = 'Replace With:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelLayer: TLabel
    Left = 16
    Top = 116
    Width = 68
    Height = 13
    Caption = 'Layer Filter:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel1: TBevel
    Left = 8
    Top = 160
    Width = 384
    Height = 2
  end
  object LabelStatus: TLabel
    Left = 16
    Top = 252
    Width = 3
    Height = 13
  end
  object EditFind: TEdit
    Left = 16
    Top = 36
    Width = 368
    Height = 21
    TabOrder = 0
  end
  object EditReplace: TEdit
    Left = 16
    Top = 84
    Width = 368
    Height = 21
    TabOrder = 1
  end
  object ComboBoxLayer: TComboBox
    Left = 16
    Top = 132
    Width = 200
    Height = 21
    Style = csDropDownList
    TabOrder = 2
  end
  object CheckBoxCaseSensitive: TCheckBox
    Left = 232
    Top = 134
    Width = 120
    Height = 17
    Caption = 'Case Sensitive'
    TabOrder = 3
  end
  object CheckBoxWholeWord: TCheckBox
    Left = 16
    Top = 172
    Width = 120
    Height = 17
    Caption = 'Whole Word Only'
    TabOrder = 4
  end
  object CheckBoxPartialMatch: TCheckBox
    Left = 150
    Top = 172
    Width = 120
    Height = 17
    Caption = 'Partial Match'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object ButtonFindAll: TButton
    Left = 16
    Top = 208
    Width = 90
    Height = 30
    Caption = 'Find All'
    TabOrder = 6
    OnClick = ButtonFindAllClick
  end
  object ButtonReplaceAll: TButton
    Left = 120
    Top = 208
    Width = 90
    Height = 30
    Caption = 'Replace All'
    TabOrder = 7
    OnClick = ButtonReplaceAllClick
  end
  object ButtonPreview: TButton
    Left = 224
    Top = 208
    Width = 90
    Height = 30
    Caption = 'Preview'
    TabOrder = 8
    OnClick = ButtonPreviewClick
  end
  object ButtonClose: TButton
    Left = 328
    Top = 208
    Width = 56
    Height = 30
    Caption = 'Close'
    TabOrder = 9
    OnClick = ButtonCloseClick
  end
end
