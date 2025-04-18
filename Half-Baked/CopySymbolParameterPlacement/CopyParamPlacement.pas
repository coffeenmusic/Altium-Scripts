
Interface
Type
  CopyParamPlacementForm = class(TForm)
    ButtonBrowse        : TButton;
    ButtonRun           : TButton;
    txtBoxRefDes                : TEdit;
    procedure ButtonBrowseClick(Sender: TObject);
    End;

Var
    SrcRot : Integer;

{..............................................................................}
Function OrientationToStr(ARotate : TRotationBy90) : String;
Begin
    Result := '';

    Case ARotate Of
        eRotate0   : Result := '0';
        eRotate90  : Result := '90';
        eRotate180 : Result := '180';
        eRotate270 : Result := '270';
    End;
End;

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
        //iterator.SetState_IterationDepth(eIterateAllLevels);
        iterator.SetState_IterationDepth(eIterateFirstLevel);
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

function GetParamOffset(ParamName: String, SrcData: TStringList, var Dx: Integer, var Dy: Integer, var just: TTextJustification, var rot: string):Boolean;
var
    i: Integer;
    ParamInfo: TStringList;
    DstParamName: String;
begin
    result := False;

    for i:=0 to SrcData.Count - 1 do
    begin
         ParamInfo := TStringList.Create;
         ParamInfo.Delimiter := ';';
         ParamInfo.DelimitedText := SrcData.Get(i);

         DstParamName := ParamInfo.Get(0);
         Dx := ParamInfo.Get(1);
         Dy := ParamInfo.Get(2);
         just := ParamInfo.Get(3);
         rot := ParamInfo.Get(4);
         If ParamName = DstParamName then
         begin
             result := True;
             exit;
         end;

         ParamInfo.Free;
    end;
end;

function SetSelectedPositions(Project: IProject, SrcData: TStringList);
Var
    I           : Integer;
    Doc         : IDocument;
    CurrentSch  : ISch_Document;
    Cmp: ISch_Component;
    CmpIterator, PIterator   : ISch_Iterator;
    CmpDes: ISch_Designator;
    CmpName, ParamName, ParamText: string;
    Hidden, Selected, ParamMatch: boolean;
    Parameter: ISch_Parameter;
    CmpX, CmpY, Px, Py, DesX, DesY, Dx, Dy: Integer;
    just : TTextJustification;
    rot: string;
Begin
    For I := 0 to Project.DM_LogicalDocumentCount - 1 Do
    Begin
        Doc := Project.DM_LogicalDocuments(I);
        If Doc.DM_DocumentKind = 'SCH' Then
        Begin
            Client.OpenDocument('SCH',Doc.DM_FullPath); // Open Document
            CurrentSch := SchServer.GetSchDocumentByPath(Doc.DM_FullPath);

            SchServer.ProcessControl.PreProcess(CurrentSch, '');

            // Look for components only
            CmpIterator := CurrentSch.SchIterator_Create;
            CmpIterator.AddFilter_ObjectSet(MkSet(eSchComponent));

            Try
                Cmp := CmpIterator.FirstSchObject;
                While Cmp <> Nil Do
                Begin
                    //ReportList.Add(AComponent.Designator.Name + ' ' + AComponent.Designator.Text);
                    CmpDes := Cmp.Designator;
                    CmpName := Cmp.Designator.Name;

                    Selected := Cmp.Selection;
                    if (CmpDes <> nil) and (Selected) then
                    begin
                        Try
                            // Match component rotation to source rotation
                            //Cmp.SetState_OrientationWithoutRotating(SrcRot);
                            Cmp.Orientation := SrcRot;

                            CmpX := Cmp.Location.X;
                            CmpY := Cmp.Location.Y;

                            DesX := CmpDes.Location.X;
                            DesY := CmpDes.Location.Y;

                            // Handle Cmp Designator Position First
                            if GetParamOffset('DESIGNATOR', SrcData, Dx, Dy, just, rot) then
                            begin
                                CmpDes.Orientation := StrToInt(rot);
                                CmpDes.Justification := StrToInt(just);
                                CmpDes.MoveToXY(CmpX + StrToInt(Dx), CmpY + StrToInt(Dy));
                            end;

                            // ITERATE PARAMETERS -----------------------------
                            PIterator := Cmp.SchIterator_Create;
                            PIterator.AddFilter_ObjectSet(MkSet(eParameter));

                            Parameter := PIterator.FirstSchObject;
                            While Parameter <> Nil Do
                            Begin
                                ParamName := Parameter.Name;
                                ParamText := Parameter.Text;
                                Parameter.IsHidden := True; // Init to Hidden

                                if GetParamOffset(ParamName, SrcData, Dx, Dy, just, rot) then
                                begin
                                    Parameter.IsHidden := False;
                                    Parameter.Orientation := StrToInt(rot);
                                    Parameter.Justification := StrToInt(just);
                                    Parameter.MoveToXY(CmpX + StrToInt(Dx), CmpY + StrToInt(Dy));
                                end;

                                Parameter := PIterator.NextSchObject;
                            End;
                        Finally
                            Cmp.SchIterator_Destroy(PIterator);
                        End;
                    end;

                    Cmp := CmpIterator.NextSchObject;
                End;
            Finally
                CurrentSch.SchIterator_Destroy(CmpIterator);
            End;

            SchServer.ProcessControl.PostProcess(CurrentSch, '');
            CurrentSch.GraphicallyInvalidate;
        End;
    End;
End;

function GetSrcPositions(Project: IProject, SrcDes: String): TStringList;
Var
    I           : Integer;
    Doc         : IDocument;
    CurrentSch  : ISch_Document;
    SrcData        : TStringList;
    Cmp: ISch_Component;
    CmpIterator, PIterator   : ISch_Iterator;
    CmpDes: ISch_Designator;
    CmpName, ParamName, ParamText: string;
    Hidden: boolean;
    Parameter: ISch_Parameter;
    CmpX, CmpY, Px, Py, Dx, Dy: Integer;
Begin
    SrcData := TStringList.Create;

    For I := 0 to Project.DM_LogicalDocumentCount - 1 Do
    Begin
        Doc := Project.DM_LogicalDocuments(I);
        If Doc.DM_DocumentKind = 'SCH' Then
        Begin
            Client.OpenDocument('SCH',Doc.DM_FullPath); // Open Document
            CurrentSch := SchServer.GetSchDocumentByPath(Doc.DM_FullPath);

            // Look for components only
            CmpIterator := CurrentSch.SchIterator_Create;
            CmpIterator.AddFilter_ObjectSet(MkSet(eSchComponent));

            Try
                Cmp := CmpIterator.FirstSchObject;
                While Cmp <> Nil Do
                Begin
                    //ReportList.Add(AComponent.Designator.Name + ' ' + AComponent.Designator.Text);
                    CmpDes := Cmp.Designator;
                    if (CmpDes <> nil) and (Cmp.Designator.Text = SrcDes) then
                    begin
                        Try
                            SrcRot := Cmp.Orientation;

                            CmpX := Cmp.Location.X;
                            CmpY := Cmp.Location.Y;

                            Dx := CmpDes.Location.X;
                            Dy := CmpDes.Location.Y;

                            SrcData.Add('DESIGNATOR'+';'+IntToStr(Dx-CmpX)+';'+IntToStr(Dy-CmpY)+';'+IntToStr(CmpDes.Justification)+';'+IntToStr(CmpDes.Orientation));

                            PIterator := Cmp.SchIterator_Create;
                            PIterator.AddFilter_ObjectSet(MkSet(eParameter));

                            Parameter := PIterator.FirstSchObject;
                            While Parameter <> Nil Do
                            Begin
                                if Parameter.IsHidden = False then
                                begin
                                    ParamName := Parameter.Name;
                                    ParamText := Parameter.Text;
                                    Px := Parameter.Location.X;
                                    Py := Parameter.Location.Y;

                                    SrcData.Add(ParamName+';'+IntToStr(Px-CmpX)+';'+IntToStr(Py-CmpY)+';'+IntToStr(Parameter.Justification)+';'+IntToStr(CmpDes.Orientation));
                                end;

                                Parameter := PIterator.NextSchObject;
                            End;
                        Finally
                            Cmp.SchIterator_Destroy(PIterator);
                        End;

                        result := SrcData;
                        exit;
                    end;

                    Cmp := CmpIterator.NextSchObject;
                End;
            Finally
                CurrentSch.SchIterator_Destroy(CmpIterator);
            End;
        End;
    End;

    SrcData.Free;
End;

function NoGUI();
Const
    D = ',';
Var
    I           : Integer;
    Project     : IProject;
    Doc         : IDocument;
    CurrentSch  : ISch_Document;
    ReportDocument : IServerDocument;
    FileName       : TPCBString;
    Document       : IServerDocument;
    SrcData        : TStringList;
    Cmp            : IPCB_Component;
    xorigin, yorigin : Integer;
    Layer, x,y,L,R,T,B, RefDes, Rot, Nets, CmpData: String;
    Rect: TCoordRect;
Begin
    Project := GetWorkspace.DM_FocusedProject;
    If Project = Nil Then Exit;

    SrcData := GetSrcPositions(Project, 'R180');

    SetSelectedPositions(Project, SrcData);
End;

{..............................................................................}
function Run(CmpSrc: String);
Const
    D = ',';
Var
    I           : Integer;
    Project     : IProject;
    Doc         : IDocument;
    CurrentSch  : ISch_Document;
    ReportDocument : IServerDocument;
    FileName       : TPCBString;
    Document       : IServerDocument;
    SrcData        : TStringList;
    Cmp            : IPCB_Component;
    xorigin, yorigin : Integer;
    Layer, x,y,L,R,T,B, RefDes, Rot, Nets, CmpData: String;
    Rect: TCoordRect;
Begin
    Project := GetWorkspace.DM_FocusedProject;
    If Project = Nil Then Exit;

    SrcData := GetSrcPositions(Project, CmpSrc);

    SetSelectedPositions(Project, SrcData);
End;
{..............................................................................}

{..............................................................................}


{..............................................................................}
Procedure TCopyParamPlacementForm.ButtonRunClick(Sender: TObject);
Begin
    Run(Trim(txtBoxRefDes.Text));
End;
{..............................................................................}

{..............................................................................}
Procedure RunGUI;
Begin
    if CopyParamPlacementForm = nil then exit;
    CopyParamPlacementForm.ShowModal;
End;
{..............................................................................}

{..............................................................................}
