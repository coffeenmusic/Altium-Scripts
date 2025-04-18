function IteratePinsInComponent(Component : ISch_Component): TStringList;
Var
    pin          : ISch_Pin;
    SchParameter  : ISch_Parameter;
    Iterator   : ISch_Iterator;
    i, PinType      : Integer;
    PinFullString : String;
    PinDesignator : String;
    PinName, Desc, CmpName    : String;
    PinLocation   : TLocation;
    PowerDesignators: TStringList;
Begin
    PowerDesignators := TStringList.Create;
    PowerDesignators.Sorted := True;
    PowerDesignators.Duplicates := dupIgnore;

    CmpName := Component.Designator.Text;
    Iterator := Component.SchIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(ePin));
    Try
        pin := Iterator.FirstSchObject;
        While pin <> Nil Do
        Begin
            PinDesignator := pin.Designator;
            //PinName := pin.Name;
            PinType := pin.Electrical;
            Desc := pin.Description;
            if PinType = eCC_PinPower then
            begin
                PowerDesignators.Add(CmpName+';'+PinDesignator+';');
            end;
            pin := Iterator.NextSchObject;
        End;
    Finally
        Component.SchIterator_Destroy(Iterator);
    End;
    result := PowerDesignators;
End;

function IterateComponentsOnSheet(SchDoc : ISch_Document);
Var
    cmp          : ISch_Component;
    Iterator   : ISch_Iterator;
    CmpPwr, SheetPwr: TStringList;
    i: Integer;
Begin
    SheetPwr := TStringList.Create;

    Iterator := SchDoc.SchIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));
    Try
        cmp := Iterator.FirstSchObject;
        While cmp <> Nil Do
        Begin
            CmpPwr := IteratePinsInComponent(cmp);
            for i:=0 to CmpPwr.Count-1 do
            begin
                SheetPwr.Add(CmpPwr.Get(i));
            end;
            cmp := Iterator.NextSchObject;
        End;
    Finally
        SchDoc.SchIterator_Destroy(Iterator);
    End;
    result := SheetPwr;
End;

function GetCmpPwrNets(Cmp: IPCB_Component, PwrPins: TStringList): TStringList;
var
    PwrNets: TStringList;
    Iterator: IPCB_GroupIterator;
    pad: IPCB_Pad;
    cmpName, padName, pinDesc, padDesignator: String;
begin
    PwrNets := TStringList.Create;
    PwrNets.Sorted := True;
    PwrNets.Duplicates := dupIgnore;

    cmpName := Cmp.Name.Text;

    Iterator := Cmp.GroupIterator_Create;
    Iterator.SetState_FilterAll;
    Iterator.AddFilter_ObjectSet(MkSet(ePadObject));
    pad := Iterator.FirstPCBObject;
    while pad <> nil do
    begin
        padName := pad.Name;
        padDesignator := pad.PinDescriptor;
        // If CmpName;PadName; string in PwrPins TStringList
        if (pad.Net <> nil) and (PwrPins.IndexOf(cmpName+';'+padName+';') <> -1) then
        begin
            PwrNets.Add(pad.Net.Name);
        end;
        pad := Iterator.NextPCBObject;
    end;
    result := PwrNets;
end;

function GetPwrPinNets(Board: IPCB_Board, PwrPins: TStringList): TStringList;
var
    i, j: Integer;
    PwrNets, SplitDelimited: TStringList;
    CmpName, PrevName, PinName, PinDesignator: String;
    Cmp: IPCB_Component;
    CmpPwrNets: TStringList;
begin

    PwrNets := TStringList.Create;
    PwrNets.Sorted := True;
    PwrNets.Duplicates := dupIgnore;

    SplitDelimited := TStringList.Create;
    SplitDelimited.Delimiter := ';';
    SplitDelimited.StrictDelimiter := True;

    PrevName := '';
    for i:=0 to PwrPins.Count-1 do
    begin
        SplitDelimited.DelimitedText := PwrPins.Get(i);
        CmpName := SplitDelimited.Get(0);
        //PinDesignator := SplitDelimited.Get(1);
        //PinName := SplitDelimited.Get(2);

        if CmpName = PrevName then continue;
        Cmp := Board.GetPcbComponentByRefDes(CmpName);
        if Cmp = nil then continue;
        CmpPwrNets := GetCmpPwrNets(Cmp, PwrPins);
        for j:=0 to CmpPwrNets.Count-1 do
        begin
            PwrNets.Add(CmpPwrNets.Get(j));
        end;

        PrevName := CmpName;
    end;
    result := PwrNets;
end;

function NetClassExists(Board: IPCB_Board, ClassName : String):Boolean;
Var
    Iterator      : IPCB_BoardIterator;
    NetClass: IPCB_ObjectClass;
Begin
    result := False;

    Iterator    := Board.BoardIterator_Create;
    Iterator.SetState_FilterAll;
    Iterator.AddFilter_ObjectSet(MkSet(eClassObject));

    NetClass := Iterator.FirstPCBObject;
    While NetClass <> Nil Do
    Begin
        If NetClass.MemberKind = eClassMemberKind_Net Then
        Begin
            If NetClass.Name = ClassName then
            Begin
                result := True;
                exit;
            End;
        End;
        NetClass := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);
End;

function AddNetListToNetClass(Board: IPCB_Board, NetList: TStringList, ClassName : String): Integer;
Var
    i, NetsAdded: Integer;
    Iterator      : IPCB_BoardIterator;
    NetName: String;
    NetClass: IPCB_ObjectClass;
Begin
    Iterator    := Board.BoardIterator_Create;
    Iterator.SetState_FilterAll;
    Iterator.AddFilter_ObjectSet(MkSet(eClassObject));

    NetsAdded := 0;
    NetClass := Iterator.FirstPCBObject;
    While NetClass <> Nil Do
    Begin
        If NetClass.MemberKind = eClassMemberKind_Net Then
        Begin
            If NetClass.Name = ClassName then
            Begin
                for i := 0 to NetList.Count-1 do
                begin
                    NetName := NetList.Get(i);
                    if NetName <> '' then
                    begin
                        NetClass.AddMemberByName(NetName);
                        Inc(NetsAdded);
                    end;
                end;
                Break;
            End;
        End;
        NetClass := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);

    result := NetsAdded;
End;

Procedure Run;
Const
    CLASS_NAME = 'PowerPinNets'
Var
    i, j, NetsAdded           : Integer;
    Project     : IProject;
    Doc         : IDocument;
    Board       : IPCB_Board;
    NetClass      :  IPCB_ObjectClass;
    CurrentSch  : ISch_Document;
    SchematicId : Integer;
    SchDocument : IServerDocument;
    DocKind, PCB_Path : String;
    SheetPwr, AllPwr, PwrNets: TStringList;
Begin
    Project := GetWorkspace.DM_FocusedProject;
    If Project = Nil Then Exit;

    AllPwr := TStringList.Create;
    PCB_Path := '';

    // Iterate Schematic Sheets
    For i := 0 to Project.DM_LogicalDocumentCount - 1 Do
    Begin
        Doc := Project.DM_LogicalDocuments(i);
        DocKind := Doc.DM_DocumentKind;
        If Doc.DM_DocumentKind = 'SCH' Then
        Begin
             SchDocument := Client.OpenDocument('SCH',Doc.DM_FullPath); // Open Document
             Client.ShowDocumentDontFocus(SchDocument); // Make Visible
             CurrentSch := SchServer.GetSchDocumentByPath(Doc.DM_FullPath);
             If CurrentSch <> Nil Then
             Begin
                 SheetPwr := IterateComponentsOnSheet(CurrentSch);
                 for j:= 0 to SheetPwr.Count - 1 do
                 begin
                     AllPwr.Add(SheetPwr.Get(j));
                 end;
                 SheetPwr.Clear;
             End;
        End
        Else If DocKind = 'PCB' Then
        Begin
            PCB_Path := Doc.DM_FullPath;
            Board := PCBServer.GetPCBBoardByPath(PCB_Path);
            If Board = Nil Then
            begin
                Client.OpenDocument('PCB', PCB_Path); // Open Document
            end;
        End;
    End;

    if PCB_Path = '' then
    begin
        ShowMessage('Failed to get PCB path. Please open entire Altium project and not just schematic documents.');
        exit;
    end;

    Board := PCBServer.GetPCBBoardByPath(PCB_Path);
    If Board = Nil Then
    begin
        ShowMessage('PCB not found. Please open PCB before running script.');
    end;

    PwrNets := GetPwrPinNets(Board, AllPwr);

    // Add NetClass if it doesn't exist already
    if not NetClassExists(Board, CLASS_NAME) then
    begin
        PCBServer.PreProcess;

        NetClass := PCBServer.PCBClassFactoryByClassMember(eClassMemberKind_Net);
        NetClass.SuperClass := False;
        NetClass.Name := CLASS_NAME;
        Board.AddPCBObject(NetClass);

        PCBServer.PostProcess;
    end;

    NetsAdded := AddNetListToNetClass(Board, PwrNets, CLASS_NAME);

    ShowMessage(IntToStr(NetsAdded) + ' nets added to ' + CLASS_NAME + ' net class.');

    AllPwr.Free;
End;
{..............................................................................}

{..............................................................................}
