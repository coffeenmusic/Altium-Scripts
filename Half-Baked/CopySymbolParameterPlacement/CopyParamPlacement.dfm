object CopyParamPlacementForm: TCopyParamPlacementForm
  Left = 117
  Top = 90
  BorderStyle = bsDialog
  Caption = 'Copy Component Parameters From One Component To Others'
  ClientHeight = 102
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LabelImport: TLabel
    Left = 8
    Top = 8
    Width = 172
    Height = 13
    Caption = 'Reference Designator to Copy From:'
  end
  object txtBoxRefDes: TEdit
    Left = 8
    Top = 24
    Width = 337
    Height = 21
    TabOrder = 0
  end
  object ButtonRun: TButton
    Left = 8
    Top = 72
    Width = 408
    Height = 25
    Caption = 'Run'
    TabOrder = 1
    OnClick = ButtonRunClick
  end
end
