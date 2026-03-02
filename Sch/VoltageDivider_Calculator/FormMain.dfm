object FormMain: TFormMain
  Left = 200
  Top = 100
  BorderStyle = bsDialog
  Caption = 'Voltage Divider Calculator'
  ClientHeight = 580
  ClientWidth = 480
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
  object GroupBoxVoltage: TGroupBox
    Left = 12
    Top = 8
    Width = 220
    Height = 85
    Caption = ' Voltages '
    TabOrder = 0
    object LabelVin: TLabel
      Left = 12
      Top = 24
      Width = 80
      Height = 13
      Caption = 'Vtop (V):'
    end
    object LabelVout: TLabel
      Left = 12
      Top = 52
      Width = 90
      Height = 13
      Caption = 'Vdivider (V):'
    end
    object EditVin: TEdit
      Left = 120
      Top = 21
      Width = 88
      Height = 21
      TabOrder = 0
      Text = '12'
    end
    object EditVout: TEdit
      Left = 120
      Top = 49
      Width = 88
      Height = 21
      TabOrder = 1
      Text = '3.3'
    end
  end
  object GroupBoxR1: TGroupBox
    Left = 12
    Top = 100
    Width = 220
    Height = 115
    Caption = ' R1 (Top Resistor) '
    TabOrder = 1
    object LabelR1Min: TLabel
      Left = 12
      Top = 24
      Width = 55
      Height = 13
      Caption = 'Min (ohms):'
    end
    object LabelR1Max: TLabel
      Left = 12
      Top = 52
      Width = 57
      Height = 13
      Caption = 'Max (ohms):'
    end
    object EditR1Min: TEdit
      Left = 120
      Top = 21
      Width = 88
      Height = 21
      TabOrder = 0
      Text = '10'
    end
    object EditR1Max: TEdit
      Left = 120
      Top = 49
      Width = 88
      Height = 21
      TabOrder = 1
      Text = '1000000'
    end
    object CheckBoxTwoR1: TCheckBox
      Left = 12
      Top = 80
      Width = 180
      Height = 17
      Caption = 'Use two resistors in series'
      TabOrder = 2
    end
  end
  object GroupBoxR2: TGroupBox
    Left = 12
    Top = 222
    Width = 220
    Height = 115
    Caption = ' R2 (Bottom Resistor) '
    TabOrder = 2
    object LabelR2Min: TLabel
      Left = 12
      Top = 24
      Width = 55
      Height = 13
      Caption = 'Min (ohms):'
    end
    object LabelR2Max: TLabel
      Left = 12
      Top = 52
      Width = 57
      Height = 13
      Caption = 'Max (ohms):'
    end
    object EditR2Min: TEdit
      Left = 120
      Top = 21
      Width = 88
      Height = 21
      TabOrder = 0
      Text = '10'
    end
    object EditR2Max: TEdit
      Left = 120
      Top = 49
      Width = 88
      Height = 21
      TabOrder = 1
      Text = '1000000'
    end
    object CheckBoxTwoR2: TCheckBox
      Left = 12
      Top = 80
      Width = 180
      Height = 17
      Caption = 'Use two resistors in series'
      TabOrder = 2
    end
  end
  object GroupBoxCurrent: TGroupBox
    Left = 12
    Top = 344
    Width = 220
    Height = 55
    Caption = ' Max Divider Current '
    TabOrder = 3
    object LabelCurrent: TLabel
      Left = 12
      Top = 24
      Width = 62
      Height = 13
      Caption = 'Current (mA):'
    end
    object EditMaxCurrent: TEdit
      Left = 120
      Top = 21
      Width = 88
      Height = 21
      TabOrder = 0
      Text = '5'
    end
  end
  object GroupBoxStdTolerance: TGroupBox
    Left = 12
    Top = 406
    Width = 220
    Height = 55
    Caption = ' Standard Value Tolerance '
    TabOrder = 4
    object LabelTolerance: TLabel
      Left = 12
      Top = 24
      Width = 72
      Height = 13
      Caption = 'Tolerance (%):'
    end
    object ComboBoxTolerance: TComboBox
      Left = 120
      Top = 21
      Width = 88
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = '1 (E96)'
      Items.Strings = (
        '1 (E96)'
        '2 (E48)'
        '5 (E24)'
        '10 (E12)')
    end
  end
  object GroupBoxParams: TGroupBox
    Left = 248
    Top = 8
    Width = 220
    Height = 135
    Caption = ' Parameter Mapping '
    TabOrder = 5
    object LabelDescParam: TLabel
      Left = 12
      Top = 22
      Width = 60
      Height = 13
      Caption = 'Resistor ID:'
    end
    object LabelDescPrefix: TLabel
      Left = 12
      Top = 48
      Width = 75
      Height = 13
      Caption = 'ID starts with:'
    end
    object LabelValueParam: TLabel
      Left = 12
      Top = 74
      Width = 70
      Height = 13
      Caption = 'Value param:'
    end
    object LabelPkgParam: TLabel
      Left = 12
      Top = 100
      Width = 80
      Height = 13
      Caption = 'Package param:'
    end
    object ComboBoxDescParam: TComboBox
      Left = 100
      Top = 19
      Width = 108
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
    object EditDescPrefix: TEdit
      Left = 100
      Top = 45
      Width = 108
      Height = 21
      TabOrder = 1
      Text = 'RES'
    end
    object ComboBoxValueParam: TComboBox
      Left = 100
      Top = 71
      Width = 108
      Height = 21
      Style = csDropDownList
      TabOrder = 2
    end
    object ComboBoxPkgParam: TComboBox
      Left = 100
      Top = 97
      Width = 108
      Height = 21
      Style = csDropDownList
      TabOrder = 3
    end
  end
  object GroupBoxPackages: TGroupBox
    Left = 248
    Top = 150
    Width = 220
    Height = 135
    Caption = ' Include Packages '
    TabOrder = 6
    object CheckBox0201: TCheckBox
      Left = 12
      Top = 20
      Width = 80
      Height = 17
      Caption = '0201'
      TabOrder = 0
    end
    object CheckBox0402: TCheckBox
      Left = 12
      Top = 40
      Width = 80
      Height = 17
      Caption = '0402'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object CheckBox0603: TCheckBox
      Left = 12
      Top = 60
      Width = 80
      Height = 17
      Caption = '0603'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object CheckBox0805: TCheckBox
      Left = 12
      Top = 80
      Width = 80
      Height = 17
      Caption = '0805'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object CheckBox1206: TCheckBox
      Left = 12
      Top = 100
      Width = 80
      Height = 17
      Caption = '1206'
      TabOrder = 4
    end
    object CheckBox1210: TCheckBox
      Left = 115
      Top = 20
      Width = 80
      Height = 17
      Caption = '1210'
      TabOrder = 5
    end
    object CheckBox2010: TCheckBox
      Left = 115
      Top = 40
      Width = 80
      Height = 17
      Caption = '2010'
      TabOrder = 6
    end
    object CheckBox2512: TCheckBox
      Left = 115
      Top = 60
      Width = 80
      Height = 17
      Caption = '2512'
      TabOrder = 7
    end
    object CheckBoxOther: TCheckBox
      Left = 115
      Top = 80
      Width = 90
      Height = 17
      Caption = 'Other / TH'
      TabOrder = 8
    end
  end
  object GroupBoxSource: TGroupBox
    Left = 248
    Top = 292
    Width = 220
    Height = 70
    Caption = ' Resistor Source '
    TabOrder = 7
    object RadioButtonSchematic: TRadioButton
      Left = 12
      Top = 20
      Width = 180
      Height = 17
      Caption = 'From schematic project'
      Checked = True
      TabOrder = 0
    end
    object RadioButtonStandard: TRadioButton
      Left = 12
      Top = 42
      Width = 180
      Height = 17
      Caption = 'Standard values (1% E96)'
      TabOrder = 1
    end
  end
  object GroupBoxIgnore: TGroupBox
    Left = 248
    Top = 369
    Width = 220
    Height = 75
    Caption = ' Ignore Resistor Values '
    TabOrder = 8
    object LabelIgnore: TLabel
      Left = 12
      Top = 18
      Width = 190
      Height = 13
      Caption = 'Comma-separated ohm values:'
    end
    object EditIgnoreList: TEdit
      Left = 12
      Top = 36
      Width = 196
      Height = 21
      TabOrder = 0
      Text = '0'
    end
  end
  object GroupBoxMaxResults: TGroupBox
    Left = 248
    Top = 451
    Width = 220
    Height = 55
    Caption = ' Max Results '
    TabOrder = 9
    object LabelMaxResults: TLabel
      Left = 12
      Top = 24
      Width = 77
      Height = 13
      Caption = 'Show top N:'
    end
    object EditMaxResults: TEdit
      Left = 120
      Top = 21
      Width = 88
      Height = 21
      TabOrder = 0
      Text = '100'
    end
  end
  object ButtonCalculate: TButton
    Left = 248
    Top = 528
    Width = 100
    Height = 30
    Caption = 'Calculate'
    Default = True
    ModalResult = 1
    TabOrder = 10
  end
  object ButtonCancel: TButton
    Left = 368
    Top = 528
    Width = 100
    Height = 30
    Caption = 'Cancel'
    Cancel = True
    ModalResult = 2
    TabOrder = 11
  end
end
