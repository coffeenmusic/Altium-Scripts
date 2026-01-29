object FormParamSelect: TFormParamSelect
  Left = 200
  Top = 100
  BorderStyle = bsDialog
  Caption = 'Select Parameters'
  ClientHeight = 400
  ClientWidth = 500
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
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 200
    Height = 13
    Caption = 'Select Parameter for Unique ID:'
  end
  object Label2: TLabel
    Left = 260
    Top = 8
    Width = 200
    Height = 13
    Caption = 'Select Additional Columns (optional):'
  end
  object Label3: TLabel
    Left = 16
    Top = 340
    Width = 100
    Height = 13
    Caption = 'Parameters found: 0'
  end
  object ListBoxUniqueID: TListBox
    Left = 16
    Top = 28
    Width = 220
    Height = 300
    ItemHeight = 13
    TabOrder = 0
  end
  object CheckListBoxColumns: TCheckListBox
    Left = 260
    Top = 28
    Width = 220
    Height = 300
    ItemHeight = 13
    TabOrder = 1
  end
  object ButtonOK: TButton
    Left = 320
    Top = 360
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
  end
  object ButtonCancel: TButton
    Left = 405
    Top = 360
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
