Var
    Board : IPCB_Board;
    MatchS, MatchD, FromToListS, FromToListD, PinNumListS, PinNumListD: TStringList;
    MinSrcX, MinSrcY, MaxSrcX, MaxSrcY : Integer;
    MinDstX, MinDstY, MaxDstX, MaxDstY : Integer;
    SheetSrc, SheetDst : Integer;
    SourceCmps, DestCmps       : TStringList;
    DragStart : Integer;


function GetListOfSelectedDesignators(Board: IPCB_Board, CmpList: TStringList) : TStringList;
var
    Iterator      : IPCB_BoardIterator;
    Cmp : IPCB_Component;
    CmpName : TPCB_String;
begin
    CmpList.Duplicates := dupIgnore;

    Iterator        := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eComponentObject));
    Iterator.AddFilter_LayerSet(MkSet(eTopLayer, eBottomLayer));
    Iterator.AddFilter_Method(eProcessAll);

    Cmp := Iterator.FirstPCBObject;
    While Cmp <> Nil Do
    Begin
        If Cmp.Selected = True Then
        Begin
            CmpList.Add(Cmp.Name.Text);
        End;

        Cmp := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);

    result := CmpList;
end;

function ComponentOnSheet(SchematicSheet: ISch_Sheet, RefDes: TPCB_String, SelectCmp: Boolean): Boolean;
Var
    iterator    :   ISch_Iterator;
    Cmp : ISch_Component;
    Location : TCoord;
    XLoc : Integer;
Begin
    result := False;

    // ensure we're in a schematic doc
    If SchematicSheet = Nil then Exit;

    iterator := SchematicSheet.SchIterator_Create;
    iterator.SetState_FilterAll;
    iterator.SetState_IterationDepth(eIterateAllLevels);
    iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    Cmp := iterator.FirstSCHObject;

    while (Cmp <> Nil) do
    begin
        If Cmp.Designator.Text = RefDes Then
        Begin
            If SelectCmp Then
            Begin
                Cmp.Selection := True;
            End;

            result := True;
            Exit;
        End;
        Cmp := iterator.NextSchObject;
    end;

    SchematicSheet.SchIterator_Destroy(iterator);
end;

// Finds the sheet number for the given refrerence designator
function GetSheetForComponent(RefDes: TPCB_String, OpenDoc: Boolean, SelectCmp: Boolean): Integer;
Var
    I           : Integer;
    Project     : IProject;
    Doc         : IDocument;
    CurrentSch  : ISch_Document;
    SchematicId : Integer;
    SchDocument : IServerDocument;
    DocKind : String;
Begin
    result := -1;

    Project := GetWorkspace.DM_FocusedProject;
    If Project = Nil Then Exit;

    // Iterate Schematic Sheets
    For I := 0 to Project.DM_LogicalDocumentCount - 1 Do
    Begin
        Doc := Project.DM_LogicalDocuments(I);
        DocKind := Doc.DM_DocumentKind;
        If Doc.DM_DocumentKind = 'SCH' Then
        Begin
             SchDocument := Client.OpenDocument('SCH',Doc.DM_FullPath); // Open Document
             //Client.ShowDocumentDontFocus(SchDocument); // Make Visible
             CurrentSch := SchServer.GetSchDocumentByPath(Doc.DM_FullPath);
             If CurrentSch <> Nil Then
             Begin
                 SchematicId := CurrentSch.SchDocID;
                 If ComponentOnSheet(CurrentSch, RefDes, SelectCmp) Then
                 Begin
                     If OpenDoc Then
                     Begin
                         Client.ShowDocumentDontFocus(SchDocument); // Make Visible
                     End;

                     result := CurrentSch.SchDocID;
                     Exit;
                 End;
             End;
        End;
    End;
end;

function CopyList(SourceList: TStringList) : TStringList;
var
    i : Integer;
    DestList : TStringList;
begin
    DestList := TStringList.Create;
    For i := 0 to SourceList.Count - 1 do
    begin
        DestList.Add(SourceList.Get(i));
    end;
    result := DestList;
end;

function StrInList(Text: String, StrList: TStringList) : Boolean;
var
    i : Integer;
begin
    result := False;
    For i := 0 to StrList.Count - 1 do
    begin
        If Text = StrList.Get(i) Then
        Begin
            result := True;
            Exit;
        End;
    end;
end;

function IndexOfStrInList(Text: String, StrList: TStringList) : Integer;
var
    i : Integer;
begin
    result := -1;
    For i := 0 to StrList.Count - 1 do
    begin
        If Text = StrList.Get(i) Then
        Begin
            result := i;
            Exit;
        End;
    end;
end;

// Gets the Component Object for each Reference Designator in the List
function RefDesListToCmpObjectList(SheetId:Integer, DesList: TStringList) : TStringList;
var
    SchematicDoc : ISch_Document;
    iterator    :   ISch_Iterator;
    Cmp : ISch_Component;
    CmpList : TList;
    Unmatched : TStringList;
    i : Integer;
begin
    CmpList := TList.Create;

    Unmatched := TStringList.Create;
    Unmatched := CopyList(DesList);

    i := 0;
    While Unmatched.Count > 0 Do
    Begin
        If i = 0 Then
        Begin
            SchematicDoc := SchServer.GetSchDocumentBySchDocID(SheetId);
        End
        Else
        Begin
            SchematicDoc := SchServer.GetSchDocumentBySchDocID(GetSheetForComponent(Unmatched.Get(0), False, False));
        End;

        // ensure we're in a schematic doc
        If SchematicDoc = Nil then Exit;

        iterator := SchematicDoc.SchIterator_Create;
        iterator.SetState_FilterAll;
        iterator.SetState_IterationDepth(eIterateAllLevels);
        iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

        Cmp := iterator.FirstSCHObject;

        while (Cmp <> Nil) do
        begin
            If StrInList(Cmp.Designator.Text, DesList) Then // Component RefDes in Cmp List
            Begin
                CmpList.Add(Cmp);
                Unmatched.Delete(Unmatched.IndexOf(Cmp.Designator.Text));
            End;
            Cmp := iterator.NextSchObject;
        end;

        SchematicDoc.SchIterator_Destroy(iterator);
        Inc(i);
    End;

    result := CmpList;
end;

// Gets the Component Object for each Reference Designator in the List
function RefDesToSchCmpObject(SheetId:Integer, RefDes: String) : ISch_Component;
var
    SchematicDoc : ISch_Document;
    iterator    :   ISch_Iterator;
    Cmp : ISch_Component;
begin
    result := Nil;
    SchematicDoc := SchServer.GetSchDocumentBySchDocID(SheetId);

    // ensure we're in a schematic doc
    If SchematicDoc = Nil then Exit;

    iterator := SchematicDoc.SchIterator_Create;
    iterator.SetState_FilterAll;
    iterator.SetState_IterationDepth(eIterateAllLevels);
    iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    Cmp := iterator.FirstSCHObject;

    while (Cmp <> Nil) do
    begin
        If Cmp.Designator.Text = RefDes Then // Component RefDes in Cmp List
        Begin
            result := Cmp;
            Exit;
        End;
        Cmp := iterator.NextSchObject;
    end;

    SchematicDoc.SchIterator_Destroy(iterator);
end;


function GetParameter(Cmp: ISch_Component, SearchParam: String) : String;
var
    iterator    :   ISch_Iterator;
    Parameter : ISch_Parameter;
    ParamName : String;
    ParamValue : String;
    CmpDes : String;
begin
    result := '';

    If Cmp = Nil Then Exit;

    Try
        CmpDes := Cmp.Designator.Text;
        iterator := Cmp.SchIterator_Create;
        iterator.SetState_FilterAll;
        iterator.SetState_IterationDepth(eIterateAllLevels);
        iterator.AddFilter_ObjectSet(MkSet(eParameter));

        Parameter := iterator.FirstSchObject;
        While Parameter <> Nil Do
        Begin
            ParamName := Parameter.Name;
            ParamValue := Parameter.Text;

            If LowerCase(ParamName) = LowerCase(SearchParam) Then
            Begin
                result := ParamValue;
                Exit;
            End;
            Parameter := iterator.NextSchObject;
        End;
    Except

    End;
end;

function ArgMinDelta(CmpSList: TList, CmpDList: TList, minSrcX: Integer, minSrcY: Integer, maxSrcX: Integer, maxSrcY: Integer, minDstX: Integer, minDstY: Integer, maxDstX: Integer, maxDstY: Integer, var minIdxS: Integer, var minIdxD: Integer): Integer;
var
    i, j : Integer;
    delta, minDelta, deltaX, deltaY : Double;
    DesS, DesD, ConfParam : String;
    nSrcX, nSrcY : Double;
    nDstX, nDstY : Double;
    CmpS, CmpD : ISch_Component;
    scaleSx, scaleSy, scaleDx, scaleDy : Integer;
begin
    scaleSx := maxSrcX - minSrcX;
    scaleSy := maxSrcY - minSrcY;
    scaleDx := maxDstX - minDstX;
    scaleDy := maxDstY - minDstY;

    minDelta := 9999999.9;
    For i := 0 to CmpSList.Count - 1 do
    begin
        For j := 0 to CmpDList.Count - 1 do
        Begin
            CmpS := CmpSList[i];
            CmpD := CmpDList[j];

            DesS := CmpS.Designator.Text;
            DesD := CmpD.Designator.Text;

            // Normalize Coordinates
            nSrcX := (CmpS.Location.X - minSrcX) / scaleSx;
            nSrcY := (CmpS.Location.Y - minSrcY) / scaleSy;
            nDstX := (CmpD.Location.X - minDstX) / scaleDx;
            nDstY := (CmpD.Location.Y - minDstY) / scaleDy;

            deltaX := abs(nSrcX - nDstX);
            deltaY := abs(nSrcY - nDstY);

            delta := deltaX + deltaY;

            If delta < minDelta Then
            Begin
                minDelta := delta;
                minIdxS := i;
                minIdxD := j;

                If delta = 0 Then Exit; // Not going to get any closer match than this
            End;
        End;
    end;
end;

function IsClosestComponent(SourceCmp: String, DestCmp: String, Destination: TStringList): Boolean;
var
    CmpS, CmpD : TList;
    idxS, idxD : Integer;
    Source : TStringList;
begin
    result := False;

    Source := TStringList.Create;
    Source.Add(SourceCmp);

    CmpS := RefDesListToCmpObjectList(SheetSrc, Source);
    CmpD := RefDesListToCmpObjectList(SheetDst, Destination);

    Source.Free;

    ArgMinDelta(CmpS, CmpD, MinSrcX, MinSrcY, MaxSrcX, MaxSrcY, MinDstX, MinDstY, MaxDstX, MaxDstY, idxS, idxD);

    If DestCmp = Destination.Get(idxD) Then
    Begin
        result := True;
    End;
end;

function MatchDesignatorsBySchematicPosition(MatchOnce: Boolean, var Source: TStringList, var Destination: TStringList);
var
    CmpS, CmpD : TList;
    idxS, idxD : Integer;
    CurrentMatchS, CurrentMatchD : String;
begin
    CmpS := RefDesListToCmpObjectList(SheetSrc, Source);
    CmpD := RefDesListToCmpObjectList(SheetDst, Destination);

    While (Source.Count > 0) and (Destination.Count > 0) Do
    Begin
        idxS := 0; idxD := 0;
        ArgMinDelta(CmpS, CmpD, MinSrcX, MinSrcY, MaxSrcX, MaxSrcY, MinDstX, MinDstY, MaxDstX, MaxDstY, idxS, idxD);
        CurrentMatchS := Source.Get(idxS);
        CurrentMatchD := Destination.Get(idxD);
        MatchS.Add(Source.Get(idxS));
        MatchD.Add(Destination.Get(idxD));

        Source.Delete(idxS);
        Destination.Delete(idxD);

        CmpS.Delete(idxS);
        CmpD.Delete(idxD);

        If MatchOnce Then Exit; // Don't find every match, we are good with one
    End;
end;

function GetRefDesPrefix(RefDes: String) : String;
var
    i : Integer;
    NumList : TStringList;
    c : String;
    newStr : String;
begin
    If RefDes = '' Then Exit;

    NumList := TStringList.Create;
    NumList.Delimiter := ',';
    NumList.DelimitedText := '0,1,2,3,4,5,6,7,8,9';

    newStr := '';
    For i:=0 to 3 Do
    Begin
        c := copy(RefDes, i, 1);
        If Not(StrInList(c, NumList)) Then
        Begin
            newStr := newStr + c;
        End
        Else // Break on first number
        Begin
            Break;
        End;
        Inc(i);
    End;
    NumList.Free;

    result := newStr;
end;

function IsFromToComponent(Cmp1: IPCB_Component, Cmp2: IPCB_Component): Boolean;
var
    GrpIter1, GrpIter2 : IPCB_GroupIterator;
    Cmp1_Pad, Cmp2_Pad : IPCB_Pad;
begin
    result := False;

    GrpIter1 := Cmp1.GroupIterator_Create;
    GrpIter1.SetState_FilterAll;
    GrpIter1.AddFilter_ObjectSet(MkSet(ePadObject));
    Cmp1_Pad := GrpIter1.FirstPCBObject;
    While Cmp1_Pad <> Nil Do
    Begin
        If (Cmp1_Pad.Net <> Nil) and (Cmp1_Pad.Net.Name = 'GND') Then
        Begin
            Cmp1_Pad := GrpIter1.NextPCBObject;
            Continue;
        End;

        GrpIter2 := Cmp2.GroupIterator_Create;
        GrpIter2.SetState_FilterAll;
        GrpIter2.AddFilter_ObjectSet(MkSet(ePadObject));
        Cmp2_Pad := GrpIter2.FirstPCBObject;
        While Cmp2_Pad <> Nil Do
        Begin
            If Cmp1_Pad.Net = Cmp2_Pad.Net Then
            Begin
                result := True;
                Exit;
            End;

            Cmp2_Pad := GrpIter2.NextPCBObject;
        End;
        Cmp2.GroupIterator_Destroy(GrpIter2);


        Cmp1_Pad := GrpIter1.NextPCBObject;
    End;
    Cmp1.GroupIterator_Destroy(GrpIter1);
end;

function CompareCmpListConnections(Board: IPCB_Board, CmpList: TStringList): TStringList;
var
    Cmp1, Cmp2 : TList;
    i, j : Integer;
    FromTo : TStringList;
    FromToList : TStringList;
    Cmp1Name, Cmp2Name : String;
begin
    FromToList := TStringList.Create;
    FromToList.Delimiter := ';';

    For i:= 0 to CmpList.Count - 1 Do
    Begin
        Cmp1 := Board.GetPcbComponentByRefDes(CmpList.Get(i));
        Cmp1Name := Cmp1.Name.Text;
        FromTo := TStringList.Create;
        For j := 0 to CmpList.Count - 1 Do
        Begin
            Cmp2 := Board.GetPcbComponentByRefDes(CmpList.Get(j));
            Cmp2Name := Cmp2.Name.Text;
            If Cmp1.Name.Text = Cmp2.Name.Text Then Continue; // Skip comparing same component

            If IsFromToComponent(Cmp1, Cmp2) Then
            Begin
                FromTo.Add(GetRefDesPrefix(Cmp2.Name.Text));
            End;
        End;

        FromToList.Add(FromTo.DelimitedText);
        FromTo.Free;
    End;
    result := FromToList;
end;

function GetPinCount(Cmp: IPCB_Component) : Integer;
var
    Iterator    : IPCB_GroupIterator;
    Pad : IPCB_Pad;
    PinCount : Integer;
begin
    Iterator := Cmp.GroupIterator_Create;
    Iterator.SetState_FilterAll;
    Iterator.AddFilter_ObjectSet(MkSet(ePadObject));

    PinCount := 0;

    Pad := Iterator.FirstPCBObject;
    While Pad <> Nil Do
    Begin
        Inc(PinCount);
        Pad := Iterator.NextPCBObject;
    End;
    Cmp.GroupIterator_Destroy(Iterator);

    result := PinCount;
end;

function ListMatchingPinNums(Cmp1: IPCB_Component, Cmp2: IPCB_Component): String;
var
    GrpIter1, GrpIter2 : IPCB_GroupIterator;
    Cmp1_Pad, Cmp2_Pad : IPCB_Pad;
    PinList : TStringList;
begin
    PinList := TStringList.Create;
    PinList.Delimiter := ';';

    GrpIter1 := Cmp1.GroupIterator_Create;
    GrpIter1.SetState_FilterAll;
    GrpIter1.AddFilter_ObjectSet(MkSet(ePadObject));
    Cmp1_Pad := GrpIter1.FirstPCBObject;
    While Cmp1_Pad <> Nil Do
    Begin
        GrpIter2 := Cmp2.GroupIterator_Create;
        GrpIter2.SetState_FilterAll;
        GrpIter2.AddFilter_ObjectSet(MkSet(ePadObject));
        Cmp2_Pad := GrpIter2.FirstPCBObject;
        While Cmp2_Pad <> Nil Do
        Begin
            If Cmp1_Pad.Net = Cmp2_Pad.Net Then
            Begin
                PinList.Add(Cmp2_Pad.Name);
            End;

            Cmp2_Pad := GrpIter2.NextPCBObject;
        End;
        Cmp2.GroupIterator_Destroy(GrpIter2);


        Cmp1_Pad := GrpIter1.NextPCBObject;
    End;
    Cmp1.GroupIterator_Destroy(GrpIter1);

    result := PinList.DelimitedText;
end;

function GetPinNumberList(Board: IPCB_Board, CmpList: TStringList): TStringList;
var
    Cmp1, Cmp2 : TList;
    i, j : Integer;
    CmpPinNumList : TStringList;
    ReturnList : TStringList;
begin
    ReturnList := TStringList.Create;

    For i:= 0 to CmpList.Count - 1 Do
    Begin
        Cmp1 := Board.GetPcbComponentByRefDes(CmpList.Get(i));

        CmpPinNumList := TStringList.Create;
        For j := 0 to CmpList.Count - 1 Do
        Begin
            Cmp2 := Board.GetPcbComponentByRefDes(CmpList.Get(j));

            If Cmp1.Name.Text = Cmp2.Name.Text Then Continue; // Skip comparing same component

            If IsFromToComponent(Cmp1, Cmp2) Then
            Begin
                // Ignore 2 pin components because they can be symmetrical
                If GetPinCount(Cmp2) > 2 Then
                Begin
                    CmpPinNumList.Add(ListMatchingPinNums(Cmp1, Cmp2));
                End;
            End;
        End;
        ReturnList.Add(CmpPinNumList.DelimitedText);
        CmpPinNumList.Free;
    End;
    result := ReturnList;
end;

function FromToPrefixMatch(FromToS: String, FromToD: String, ExactMatch: Boolean): Boolean;
var
    CmpFromToS, CmpFromToD : TStringList;
    RefDesPrefix : String;
    i, j, DeleteIdx : Integer;
begin
    result := True;

    CmpFromToS := TStringList.Create; CmpFromToD := TStringList.Create;
    CmpFromToS.Delimiter := ';'; CmpFromToD.Delimiter := ';';
    CmpFromToS.DelimitedText := FromToS;
    CmpFromToD.DelimitedText := FromToD;

    For i:= 0 to CmpFromToD.Count - 1 Do
    Begin
        RefDesPrefix := CmpFromToD.Get(i);
        If Not(StrInList(RefDesPrefix, CmpFromToS)) Then
        Begin
            result := False;
            Exit;
        End
        Else
        Begin
            If ExactMatch Then
            Begin
                DeleteIdx := CmpFromToS.IndexOf(RefDesPrefix);
                If DeleteIdx >= 0 Then
                Begin
                    CmpFromToS.Delete(DeleteIdx);
                End;
            End;
        End;
    End;
end;

function PinNumbersMatch(PinS: String, PinD: String): Boolean;
var
    CmpPinS, CmpPinD : TStringList;
    PinNumberStr : String;
    i, j, DeleteIdx : Integer;
begin
    result := True;

    If (PinS = '') or (PinD = '') Then
    Begin
        Exit;
    End;

    CmpPinS := TStringList.Create; CmpPinD := TStringList.Create;
    CmpPinS.Delimiter := ';'; CmpPinD.Delimiter := ';';
    CmpPinS.StrictDelimiter := True; CmpPinD.StrictDelimiter := True;
    CmpPinS.DelimitedText := PinS;
    CmpPinD.DelimitedText := PinD;

    For i:= 0 to CmpPinD.Count - 1 Do
    Begin
        PinNumberStr := CmpPinD.Get(i);
        If Not(StrInList(PinNumberStr, CmpPinS)) Then
        Begin
            result := False;
            Exit;
        End;
    End;
end;

function CmpParameterMatches(ParamName: String, RefDesS: String, RefDesD: String) : Boolean;
var
    CmpS, CmpD: ISch_Component;
    idxS, idxD : Integer;
    ParamS, ParamD : String;
begin
    result := True;

    If (RefDesS = '') or (RefDesD = '') Then Exit;

    CmpS := RefDesToSchCmpObject(SheetSrc, RefDesS);
    CmpD := RefDesToSchCmpObject(SheetDst, RefDesD);

    ParamS := GetParameter(CmpS, ParamName);
    ParamD := GetParameter(CmpD, ParamName);

    If ParamS <> ParamD Then result := False;
end;

// Remove components if they are a singular RefDes prefix that doesn't exist in the other group
function RemoveExtraComponents(CmpListS: TStringList, CmpListD: TStringList): TStringList;
var
    i : Integer;
    PrefixListS, SheetNumberList : TStringList;
    DeleteList : TStringList;
    NewList : TStringList;
    Sheet : String;
    RefDes : String;
begin
    PrefixListS := TStringList.Create;
    PrefixListS.Sorted := True;
    PrefixListS.Duplicates := dupIgnore;

    DeleteList := TStringList.Create;
    NewList := TStringList.Create;

    For i:=0 to CmpListS.Count - 1 Do
    Begin
        PrefixListS.Add(GetRefDesPrefix(CmpListS.Get(i)));
    End;

    For i:=0 to CmpListD.Count - 1 Do
    Begin
        RefDes := CmpListD.Get(i);

        If Not StrInList(GetRefDesPrefix(RefDes), PrefixListS) Then
        Begin
            DeleteList.Add(RefDes);
        End
    End;


    For i:=0 to CmpListD.Count - 1 Do
    Begin
        If Not StrInList(CmpListD.Get(i), DeleteList) Then
        Begin
            NewList.Add(CmpListD.Get(i));
        End;
    End;

    DeleteList.Free;
    PrefixListS.Free;

    result := NewList;
end;

function GetMinMaxCoord(SheetId:Integer, CmpList: TStringList, var MinX: Integer, var MinY: Integer, var MaxX: Integer, var MaxY: Integer);
var
    SchematicDoc : ISch_Document;
    iterator    :   ISch_Iterator;
    Cmp : ISch_Component;
begin
    SchematicDoc := SchServer.GetSchDocumentBySchDocID(SheetId);

    // ensure we're in a schematic doc
    If SchematicDoc = Nil then Exit;

    iterator := SchematicDoc.SchIterator_Create;
    iterator.SetState_FilterAll;
    iterator.SetState_IterationDepth(eIterateAllLevels);
    iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    Cmp := iterator.FirstSCHObject;

    MaxX := 0;
    MaxY := 0;
    MinX := 2147483646;
    MinY := 2147483646;
    while (Cmp <> Nil) do
    begin
        If StrInList(Cmp.Designator.Text, CmpList) Then // Component RefDes in Cmp List
        Begin
            If Cmp.Location.X < MinX Then MinX := Cmp.Location.X;
            If Cmp.Location.Y < MinY Then MinY := Cmp.Location.Y;
            If Cmp.Location.X > MaxX Then MaxX := Cmp.Location.X;
            If Cmp.Location.Y > MaxY Then MaxY := Cmp.Location.Y;
        End;
        Cmp := iterator.NextSchObject;
    end;
    SchematicDoc.SchIterator_Destroy(iterator);
end;

function FilterComponentList(Board: IPCB_Board, CmpS: IPCB_Component, SourceList:TStringList, DestList: TStringList, FilterLevel: Integer) : TStringList;
var
    CmpD : IPCB_Component;
    i, j, FiltCnt : Integer;
    Filtered : TStringList;
    FPConf, FPDesc : TPCB_String;
    CmpNameS, CmpNameD : TPCB_String;
    PartNum : TPCB_String;
    DelIndex : Integer;
    DesPrefixS, DesPrefixD, CmpFromToS, CmpFromToD, CmpPinS, CmpPinD : String;
begin
    Filtered := TStringList.Create;
    Filtered := CopyList(DestList);

    i := 0;
    For i := 0 to DestList.Count - 1 do
    begin
        CmpD := Board.GetPcbComponentByRefDes(DestList.Get(i));
        FPConf := CmpS.SourceDescription;
        FPDesc := CmpD.SourceDescription; // SourceDescription
        CmpNameS := CmpS.Name.Text;
        CmpNameD := CmpD.Name.Text;

        For FiltCnt := 0 to FilterLevel Do
        Begin
            DelIndex := Filtered.IndexOf(CmpNameD);
            If DelIndex = -1 Then Continue;

            If FiltCnt = 0 Then
            Begin
                // filter out components where 'RefDes prefixes' don't match
                DesPrefixS := GetRefDesPrefix(CmpNameS);
                DesPrefixD := GetRefDesPrefix(CmpNameD);
                If DesPrefixS <> DesPrefixD Then
                Begin
                    Filtered.Delete(DelIndex);
                    Break;
                End;
            End
            Else If FiltCnt = 1 Then
            Begin
                // filter out components where 'First 4 Digits of Part Number' don't match
                If copy(CmpS.SourceCompDesignItemID, 0, 4) <> copy(CmpD.SourceCompDesignItemID, 0, 4) Then
                Begin
                    Filtered.Delete(DelIndex);
                    Break;
                End;
            End
            Else If FiltCnt = 2 Then
            Begin
                // filter out 'Component Values' that don't match
                If Not(CmpParameterMatches('Value', CmpS.Name.Text, CmpD.Name.Text)) Then
                Begin
                    Filtered.Delete(DelIndex);
                    Break;
                End;
            End
            Else If FiltCnt = 3 Then
            Begin
                // filter out components where 'Footprints' don't match
                If CmpS.SourceFootprintLibrary <> CmpD.SourceFootprintLibrary Then
                Begin
                    Filtered.Delete(DelIndex);
                    Break;
                End;
            End
            Else If FiltCnt = 4 Then
            Begin
                // filter out components where 'Descriptions' don't match
                If CmpS.SourceDescription <> CmpD.SourceDescription Then
                Begin
                    Filtered.Delete(DelIndex);
                    Break;
                End;
            End
            Else If FiltCnt = 5 Then
            Begin
                CmpPinS := PinNumListS.Get(SourceList.IndexOf(CmpNameS));
                CmpPinD := PinNumListD.Get(i);

                If Not PinNumbersMatch(CmpPinS, CmpPinD) Then
                Begin
                    Filtered.Delete(DelIndex);
                    Break;
                End;
            End
            Else If FiltCnt = 6 Then
            Begin
                CmpFromToS := FromToListS.Get(SourceList.IndexOf(CmpNameS));
                CmpFromToD := FromToListD.Get(i);

                // filter out components where 'FromTo prefixes don't exist in other set'
                If Not FromToPrefixMatch(CmpFromToS, CmpFromToD, False) Then
                Begin
                    Filtered.Delete(DelIndex);
                    Break;
                End;
            End
            Else If FiltCnt = 7 Then
            Begin
                CmpFromToS := FromToListS.Get(SourceList.IndexOf(CmpNameS));
                CmpFromToD := FromToListD.Get(i);

                // filter out components where 'FromTo prefixes don't have a match for every single component'
                If Not FromToPrefixMatch(CmpFromToS, CmpFromToD, True) Then
                Begin
                    Filtered.Delete(DelIndex);
                    Break;
                End;
            End
            Else If FiltCnt = 8 Then
            Begin
                // Final Filter: filter out all components that aren't the closest match by schematic position
                If Not IsClosestComponent(CmpNameS, CmpNameD, Filtered) Then
                Begin
                    Filtered.Delete(DelIndex);
                    Break;
                End;
            End;

            If Filtered.Count = 0 Then
            Begin
                result := Filtered;
                Exit;
            End;
        End;
    end;

    result := Filtered;
end;

// Pass in a source & destination list of reference designator strings
function MatchDesignators(Source: TStringList, Destination: TStringList) : Integer;
var
    CmpS, CmpD : IPCB_Component;
    i, MatchCount : Integer;
    Filtered : TStringList;
    MatchName, NewMatchList : TPCB_String;
    FilterLvl : Integer;
    DeleteIdx : Integer;
    NewMatch : Boolean;
    SourceName : String;
begin
    MatchCount := 0; NewMatch := True;

    ProgressBar1.Position := 0;
    ProgressBar1.Update;
    ProgressBar1.Max := Destination.Count;

    // Iterate until all matches found
    // 1. Find all matches from filtering component list
    //    a) Increase filter level on each iteration
    //       a-1) Filter 1
    //       a-2) Filter 2
    //       a-n) ...
    //    b) If only 1 item left in filter list, it's a match
    // 2. Find other match from schematic component position
    // 3. Repeat 1 & 2 until all found
    While (Source.Count > 0) and (Destination.Count > 0) Do
    Begin

    While NewMatch Do
    Begin
        NewMatch := False;
        NewMatchList := TStringList.Create;

        For i := 0 to Source.Count - 1 do
        begin
            CmpS := Board.GetPcbComponentByRefDes(Source.Get(i));
            SourceName := CmpS.Name.Text;

            For FilterLvl := 0 to 7 Do
            Begin
                Filtered := FilterComponentList(Board, CmpS, Source, Destination, FilterLvl);

                // If only 1 component left, it's a match
                If Filtered.Count = 1 Then
                Begin

                    MatchName := Filtered.Get(0);
                    MatchS.Add(CmpS.Name.Text); NewMatchList.Add(CmpS.Name.Text);
                    MatchD.Add(MatchName);

                    DeleteIdx := Destination.IndexOf(MatchName);
                    Destination.Delete(DeleteIdx);
                    FromToListD.Delete(DeleteIdx);
                    PinNumListD.Delete(DeleteIdx);

                    NewMatch := True;
                    Inc(MatchCount);

                    ProgressBar1.Position := ProgressBar1.Position + 1;
                    ProgressBar1.Update;

                    Break;
                End;

                If Filtered.Count = 0 Then
                Begin
                    Break;
                End;
            End;
        end;

        // Remove matched designators from source list
        If Source.Count > 0 Then
        Begin
            For i := 0 to NewMatchList.Count - 1 do
            begin
                DeleteIdx := Source.IndexOf(NewMatchList.Get(i));
                If DeleteIdx >= 0 Then
                Begin
                    Source.Delete(DeleteIdx);
                    FromToListS.Delete(DeleteIdx);
                    PinNumListS.Delete(DeleteIdx);
                End;
            end;
        End;
        NewMatchList.Free;
    End;

    // Any unmatched designators will now be matched by similarity in schematic symbol's placement
    MatchDesignatorsBySchematicPosition(True, Source, Destination);

    End;

    result := MatchCount;
end;

{..............................................................................}
function CopyPlacement(): Boolean;
Var
    CmpSrc, CmpDst            : IPCB_Component;
    NameSrc, NameDst : TPCB_String;
    i : Integer;
    Descriptor, Detail, SrcDescription : TPCB_String;
    X1,Y1,X2,Y2    : TCoord;
Begin

    PCBServer.PreProcess;

    if MatchS <> nil then
    begin
        For i := 0 to MatchS.Count - 1 do
        begin
            NameSrc := MatchS.Get(i);
            CmpSrc := Board.GetPcbComponentByRefDes(NameSrc);

            NameDst := MatchD.Get(i);
            CmpDst := Board.GetPcbComponentByRefDes(NameDst);

            PCBServer.SendMessageToRobots(CmpDst.I_ObjectAddress, c_Broadcast, PCBM_BeginModify , c_NoEventData);

            // Move Destination Components to Match Source Components
            CmpDst.Rotation := CmpSrc.Rotation;
            CmpDst.Layer_V6 := CmpSrc.Layer_V6;
            CmpDst.x := CmpSrc.x;
            CmpDst.y := CmpSrc.y;
            CmpDst.Selected := True;

            PCBServer.SendMessageToRobots(CmpDst.I_ObjectAddress, c_Broadcast, PCBM_EndModify , c_NoEventData);

            //Descriptor := CmpDst.Descriptor;
            //Detail := CmpDst.Detail; // Footprint
            SrcDescription := CmpDst.SourceDescription;
        end;
    end;

    PCBServer.PostProcess;

    SourceCmps.Free;
    DestCmps.Free;
End;
{..............................................................................}

{..............................................................................}
Procedure RunGUI;
Begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    Form_LayoutDuplicator.ShowModal;
End;

function PreprocessSource(): Boolean;
Var
    i : Integer;
Begin
    MatchS := TStringList.Create;

    ProgressBar1.Position := 1;
    ProgressBar1.Update;
    ProgressBar1.Max := 6;

    // Expects Source Components to be Pre-Selected
    SourceCmps := TStringList.Create;
    GetListOfSelectedDesignators(Board, SourceCmps);
    For i:= 0 to SourceCmps.Count - 1 Do
    Begin
        lbSource.Items.Add(SourceCmps.Get(i));
    End;

    ProgressBar1.Position := 2;
    ProgressBar1.Update;

    If SourceCmps.Count = 0 Then
    Begin
        ShowMessage('Select source components before running.');
        Exit;
    End;
    SheetSrc := GetSheetForComponent(SourceCmps.Get(0), False, False); // Get the schematic sheet the source components are on

    ProgressBar1.Position := 3;
    ProgressBar1.Update;

    GetMinMaxCoord(SheetSrc, SourceCmps, MinSrcX, MinSrcY, MaxSrcX, MaxSrcY);

    ProgressBar1.Position := 4;
    ProgressBar1.Update;

    FromToListS := CompareCmpListConnections(Board, SourceCmps);

    ProgressBar1.Position := 5;
    ProgressBar1.Update;

    PinNumListS := GetPinNumberList(Board, SourceCmps);

    ProgressBar1.Position := 6;
    ProgressBar1.Update;

    Client.SendMessage('PCB:DeSelect', 'Scope=All' , 255, Client.CurrentView);

    // Finished, Reset
    ProgressBar1.Position := 0;
    ProgressBar1.Update;

    btnDestination.Visible := True;
End;

procedure TForm_LayoutDuplicator.btnSourceClick(Sender: TObject);
begin
    PreprocessSource();
end;

procedure TForm_LayoutDuplicator.btnSourceSelectClick(Sender: TObject);
begin
    Client.SendMessage('PCB:Select', 'Scope=InsideArea | ObjectKind=Component' , 255, Client.CurrentView); // User selects source components
    PreprocessSource();
end;

procedure TForm_LayoutDuplicator.btnDestinationClick(Sender: TObject);
Var
    i, MatchCount : Integer;
    Cmp : IPCB_Component;
begin
    MatchD := TStringList.Create;

    Client.SendMessage('PCB:Select', 'Scope=InsideArea | ObjectKind=Component' , 255, Client.CurrentView); // User selects destination components
    DestCmps := TStringList.Create;
    GetListOfSelectedDesignators(Board, DestCmps);
    If DestCmps.Count = 0 Then
    Begin
        ShowMessage('No destination components chosen. Exiting.');
        Exit;
    End;
    SheetDst := GetSheetForComponent(DestCmps.Get(0), False, False);
    GetMinMaxCoord(SheetDst, DestCmps, MinDstX, MinDstY, MaxDstX, MaxDstY);
    FromToListD := CompareCmpListConnections(Board, DestCmps);
    PinNumListD := GetPinNumberList(Board, DestCmps);
    Client.SendMessage('PCB:DeSelect', 'Scope=All' , 255, Client.CurrentView);


    // Look for and try to remove extra components
    If SourceCmps.Count <> DestCmps.Count Then
    Begin;
        DestCmps := RemoveExtraComponents(SourceCmps, DestCmps); // Remove Extra Destination Components
        SourceCmps := RemoveExtraComponents(DestCmps, SourceCmps); // Remove Extra Source Components

        If SourceCmps.Count <> DestCmps.Count Then
        Begin
            If Not ConfirmNoYes('Component counts dont match between source and destination layouts. Are you sure you would like to continue?') Then Exit;
        End;
    End;

    // Match component Designators between Source and Destination components
    MatchCount := MatchDesignators(SourceCmps, DestCmps);

    lbSource.Items.Clear;
    For i:= 0 to MatchS.Count - 1 Do
    Begin
        lbSource.Items.Add(MatchS.Get(i));

        Cmp := Board.GetPcbComponentByRefDes(MatchS.Get(i));
        lbSourceDesc.Items.Add(Cmp.SourceDescription);
    End;

    For i:= 0 to MatchD.Count - 1 Do
    Begin
        lbDestination.Items.Add(MatchD.Get(i));

        Cmp := Board.GetPcbComponentByRefDes(MatchD.Get(i));
        lbDestinationDesc.Items.Add(Cmp.SourceDescription);
    End;

    ProgressBar1.Position := 0;
    ProgressBar1.Update;

    btnRun.Visible := True;
end;

procedure TForm_LayoutDuplicator.lbSourceEndDrag(Sender, Target: TObject; X, Y: Integer);
var
    DragStop : Integer;
begin
    DragStop := lbSource.GetItemIndex;
    lbSource.Items.Move(DragStart, DragStop);
    lbSourceDesc.Items.Move(DragStart, DragStop);
    MatchD.Move(DragStart, DragStop);
end;

procedure TForm_LayoutDuplicator.lbSourceStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
    DragStart := lbSource.GetItemIndex;
end;

procedure TForm_LayoutDuplicator.lbDestinationStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
    DragStart := lbDestination.GetItemIndex;
end;

procedure TForm_LayoutDuplicator.lbDestinationEndDrag(Sender, Target: TObject; X, Y: Integer);
var
    DragStop : Integer;
begin
    DragStop := lbDestination.GetItemIndex;
    lbDestination.Items.Move(DragStart, DragStop);
    lbDestinationDesc.Items.Move(DragStart, DragStop);
    MatchD.Move(DragStart, DragStop);
end;

procedure TForm_LayoutDuplicator.lbSourceDblClick(Sender: TObject);
const
    NEWLINECODE = #13#10;
var
    RefDes : String;
    Sheet : Integer;
    Cmp : IPCB_Component;
begin
    RefDes := lbSource.Items[lbSource.GetItemIndex];

    Sheet := GetSheetForComponent(RefDes, True, True);
end;

procedure TForm_LayoutDuplicator.lbDestinationDblClick(Sender: TObject);
const
    NEWLINECODE = #13#10;
var
    RefDes : String;
    Sheet : Integer;
    Cmp : IPCB_Component;
begin
    RefDes := lbDestination.Items[lbDestination.GetItemIndex];

    Sheet := GetSheetForComponent(RefDes, True, True);
end;


procedure TForm_LayoutDuplicator.btnRunClick(Sender: TObject);
begin
    CopyPlacement();
end;

