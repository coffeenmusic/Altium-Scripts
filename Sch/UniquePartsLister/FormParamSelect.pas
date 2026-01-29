{..............................................................................}
{ Form unit for Parameter Selection dialog                                     }
{..............................................................................}

Unit FormParamSelectUnit;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst;

Type
  TFormParamSelect = class(TForm)
    Label1              : TLabel;
    Label2              : TLabel;
    Label3              : TLabel;
    ListBoxUniqueID     : TListBox;
    CheckListBoxColumns : TCheckListBox;
    ButtonOK            : TButton;
    ButtonCancel        : TButton;
  End;

Var
  FormParamSelect : TFormParamSelect;

Implementation

{$R *.DFM}

End.
