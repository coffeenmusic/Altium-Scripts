
Interface
Type
  TCheckPinsFormForm = class(TForm)
    ButtonBrowse        : TButton;
    ButtonRun           : TButton;
    OpenDialog          : TOpenDialog;
    Edit                : TEdit;
    procedure ButtonBrowseClick(Sender: TObject);
    procedure ButtonRunClick(Sender: TObject);
  End;

Var
    CheckPinsForm : TCheckPinsForm;
    SchDoc         : ISch_Document;
{..............................................................................}

{..............................................................................}
Procedure TCheckPinsForm.ButtonBrowseClick(Sender: TObject);
Begin
    If OpenDialog.Execute Then Edit.Text := OpenDialog.FileName;
End;
{..............................................................................}
Function GetAllPinsFromSymbol(Dummy): TStringList;
Var
     CurrentLib       : ISch_Lib;
     ALibCompReader : ILibCompInfoReader;
     CompInfo       : IComponentInfo;
     cmp: ISch_Component;
     cmpId: Integer;
     Iterator: ISch_Iterator;
     pin: ISch_Pin;
     symPinName, symPinDesc: String;
     PinList: TStringList;
Begin
     result := False;
     // Check if schematic server exists or not.
     If SchServer = Nil Then Exit;

     // Obtain the current schematic document interface.
     CurrentLib := SchServer.GetCurrentSchDocument;
     If CurrentLib = Nil Then Exit;

     // CHeck if CurrentLib is a Library document or not
     If CurrentLib.ObjectID <> eSchLib Then
     Begin
         ShowError('Please open schematic library.');
         Exit;
     End;
     cmp := CurrentLib.CurrentSchComponent;

     PinList := TStringList.Create;

     Iterator := cmp.SchIterator_Create;
     pin := Iterator.FirstSchObject;
     while pin <> nil do
     begin
         if pin.ObjectId = ePin then
         begin

             symPinName := Trim(pin.Designator);
             symPinDesc := Trim(pin.Name);

             PinList.Add(symPinName+';'+symPinDesc);
         end;
         pin := Iterator.NextSchObject;
     end;
     cmp.SchIterator_Destroy(Iterator);

     result := PinList;
End;

Function GetPinDescriptionForPinName(PinName: TPCBString, PinList: TStringList): String;
Var
     symPinName, symPinDesc: String;
     delimList: TStringList;
     i: Integer;
Begin
     result := 'NIL';

     delimList := TStringList.Create;
     delimList.Delimiter := ';';
     delimList.StrictDelimiter := True;
     for i:=0 to PinList.Count-1 do
     begin
         delimList.DelimitedText := PinList.Get(i);

         if delimList.Get(0) = PinName then
         begin
             result := delimList.Get(1);
             exit;
         end;
     end;
End;

function RemoveFileNameFromPath(Path: String): String;
var
    i, lastIdx: Integer;
    char: String;
begin
     for i:=0 to Length(Path) do
     begin
         char := Path[i];
         if char = '\' then
         begin
             lastIdx := i;
         end;
     end;

     result := Copy(Path, 0, lastIdx);
end;

Function Run(Dummy);
Const
    COL_CNT = 2;
    DESIGNATOR_COL = 0;
    LENGTH_COL = 1;
Var
    ValuesCount      : Integer       ;
    i, k             : Integer       ;
    row_idx, char_cnt : Integer;
    StrList, Row, NotFound, AllPins, PathDel: TStringList   ;
    PinName, PinDescription, SymDescription: String        ;
    importPath: String;
    exact_cnt, close_cnt, total_pins, match_idx: Integer;
Begin
    // check if file exists or not
    If Not(FileExists(Edit.Text)) or (Edit.Text = '') Then
    Begin
        ShowWarning('The Pin Data CSV format file doesnt exist!');
        Exit;
    End
    Else
    Begin
        importPath := Edit.Text;
    End;

    AllPins := GetAllPinsFromSymbol(0);

    StrList := TStringList.Create;
    NotFound := TStringList.Create;
    NotFound.Add('MatchType;PinDesignator;PinDescription;SymbolDescription');

    Row := TStringList.Create;
    Try
        StrList.LoadFromFile(importPath); // CSV with pin/package lengths

        // Iterate CSV Rows
        exact_cnt := 0;
        close_cnt := 0;
        For row_idx := 0 To StrList.Count-1 Do
        Begin
            Row.DelimitedText := StrList.Get(row_idx);
            Row.StrictDelimiter := True;


            if Row.Count < 2 then
            begin
                ShowMessage('Expected 2 columns (PinName, PinDescription), got '+IntToStr(Row.Count)+'. Exiting..');
                Exit;
            end;

            PinName := Trim(Row.Get(0));
            PinDescription := Trim(Row.Get(1));

            match_idx := AllPins.IndexOf(PinName+';'+PinDescription);

            //Exact Match
            if match_idx >= 0 then
            begin
                Inc(exact_cnt);

                AllPins.Delete(match_idx);
            end
            else
            begin
                SymDescription := GetPinDescriptionForPinName(PinName, AllPins);

                // Pin Name Match, Description Doesn't Match
                if SymDescription <> 'NIL' then
                begin
                    Inc(close_cnt);
                    NotFound.Add('NoDescriptionMatch;' + PinName + ';' + PinDescription+';' + SymDescription);
                    AllPins.Delete(AllPins.IndexOf(PinName+';'+SymDescription));
                end
                // No Match
                else
                begin
                    NotFound.Add('NoMatch;' + PinName + ';' + PinDescription+';');
                end;

            end;
        End;
    Finally
        total_pins := StrList.Count;
        StrList.Free;
    End;

    ShowMessage(IntToStr(exact_cnt) + ' pins match exactly. ' + IntToStr(close_cnt) + ' pins match the pin name but not the pin description. Total pins: ' + IntToStr(total_pins)+'. Check pin_list.csv dir for exported report file.');

    importPath := RemoveFileNameFromPath(importPath);
    NotFound.SaveToFile(importPath+'UnmatchedPins.txt');

    Close;
End;

{..............................................................................}
Procedure TCheckPinsForm.ButtonRunClick(Sender: TObject);
Begin
    Run(0);
End;
{..............................................................................}

{..............................................................................}
Procedure RunGUI;
Begin
    if CheckPinsForm = nil then exit;
    CheckPinsForm.ShowModal;
End;
{..............................................................................}

{..............................................................................}
