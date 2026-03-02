{..............................................................................}
{ Form unit for Voltage Divider main input dialog                              }
{..............................................................................}

Unit FormMainUnit;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

Type
  TFormMain = class(TForm)
    GroupBoxVoltage      : TGroupBox;
    LabelVin             : TLabel;
    LabelVout            : TLabel;
    EditVin              : TEdit;
    EditVout             : TEdit;
    GroupBoxR1           : TGroupBox;
    LabelR1Min           : TLabel;
    LabelR1Max           : TLabel;
    EditR1Min            : TEdit;
    EditR1Max            : TEdit;
    CheckBoxTwoR1        : TCheckBox;
    GroupBoxR2           : TGroupBox;
    LabelR2Min           : TLabel;
    LabelR2Max           : TLabel;
    EditR2Min            : TEdit;
    EditR2Max            : TEdit;
    CheckBoxTwoR2        : TCheckBox;
    GroupBoxCurrent      : TGroupBox;
    LabelCurrent         : TLabel;
    EditMaxCurrent       : TEdit;
    GroupBoxStdTolerance : TGroupBox;
    LabelTolerance       : TLabel;
    ComboBoxTolerance    : TComboBox;
    GroupBoxParams       : TGroupBox;
    LabelDescParam       : TLabel;
    LabelDescPrefix      : TLabel;
    LabelValueParam      : TLabel;
    LabelPkgParam        : TLabel;
    ComboBoxDescParam    : TComboBox;
    EditDescPrefix       : TEdit;
    ComboBoxValueParam   : TComboBox;
    ComboBoxPkgParam     : TComboBox;
    GroupBoxPackages     : TGroupBox;
    CheckBox0201         : TCheckBox;
    CheckBox0402         : TCheckBox;
    CheckBox0603         : TCheckBox;
    CheckBox0805         : TCheckBox;
    CheckBox1206         : TCheckBox;
    CheckBox1210         : TCheckBox;
    CheckBox2010         : TCheckBox;
    CheckBox2512         : TCheckBox;
    CheckBoxOther        : TCheckBox;
    GroupBoxSource       : TGroupBox;
    RadioButtonSchematic : TRadioButton;
    RadioButtonStandard  : TRadioButton;
    GroupBoxIgnore       : TGroupBox;
    LabelIgnore          : TLabel;
    EditIgnoreList       : TEdit;
    GroupBoxMaxResults   : TGroupBox;
    LabelMaxResults      : TLabel;
    EditMaxResults       : TEdit;
    ButtonCalculate      : TButton;
    ButtonCancel         : TButton;
  End;

Var
  FormMain : TFormMain;

Implementation

{$R *.DFM}

End.
