{..............................................................................}
{ Summary: Copy parameter placement from source component to destination       }
{          components. Works with unannotated designators (R?, C?, etc.)      }
{                                                                              }
{ Usage:   1. Select destination component(s)                                 }
{          2. Run script                                                      }
{          3. Click on source component when prompted                         }
{..............................................................................}

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

{..............................................................................}
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
         Dx := StrToInt(ParamInfo.Get(1));
         Dy := StrToInt(ParamInfo.Get(2));
         just := StrToInt(ParamInfo.Get(3));
         rot := ParamInfo.Get(4);
         If ParamName = DstParamName then
         begin
             result := True;
             ParamInfo.Free;
             exit;
         end;

         ParamInfo.Free;
    end;
end;

{..............................................................................}
function SetPositionsForComponent(Cmp: ISch_Component, SrcData: TStringList): Boolean;
Var
    PIterator   : ISch_Iterator;
    CmpDes: ISch_Designator;
    ParamName, ParamText: string;
    Parameter: ISch_Parameter;
    CmpX, CmpY, Dx, Dy: Integer;
    just : TTextJustification;
    rot: string;
Begin
    Result := False;

    If Cmp = Nil Then Exit;

    Try
        CmpDes := Cmp.Designator;

        If CmpDes = Nil Then Exit;

        // Match component rotation to source rotation
        Cmp.Orientation := SrcRot;

        CmpX := Cmp.Location.X;
        CmpY := Cmp.Location.Y;

        // Handle Cmp Designator Position First
        if GetParamOffset('DESIGNATOR', SrcData, Dx, Dy, just, rot) then
        begin
            CmpDes.Orientation := StrToInt(rot);
            CmpDes.Justification := just;
            CmpDes.MoveToXY(CmpX + Dx, CmpY + Dy);
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
                Parameter.Justification := just;
                Parameter.MoveToXY(CmpX + Dx, CmpY + Dy);
            end;

            Parameter := PIterator.NextSchObject;
        End;

        Cmp.SchIterator_Destroy(PIterator);
        Result := True;
    Except
        ShowMessage('Error setting parameter positions');
    End;
End;

{..............................................................................}
function GetSrcPositionsFromComponent(Cmp: ISch_Component): TStringList;
Var
    SrcData        : TStringList;
    PIterator      : ISch_Iterator;
    CmpDes: ISch_Designator;
    ParamName, ParamText: string;
    Parameter: ISch_Parameter;
    CmpX, CmpY, Px, Py, Dx, Dy: Integer;
Begin
    SrcData := TStringList.Create;
    Result := SrcData;

    If Cmp = Nil Then Exit;

    Try
        CmpDes := Cmp.Designator;
        If CmpDes = Nil Then Exit;

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

                SrcData.Add(ParamName+';'+IntToStr(Px-CmpX)+';'+IntToStr(Py-CmpY)+';'+IntToStr(Parameter.Justification)+';'+IntToStr(Parameter.Orientation));
            end;

            Parameter := PIterator.NextSchObject;
        End;

        Cmp.SchIterator_Destroy(PIterator);
    Except
        ShowMessage('Error reading source component parameters');
    End;
End;

{..............................................................................}
function GetComponentAtLocation(CurrentSch: ISch_Document; X: Integer; Y: Integer): ISch_Component;
Var
    SpatialIterator : ISch_Iterator;
    GraphicalObj    : ISch_GraphicalObject;
    SearchRadius    : Integer;
Begin
    Result := Nil;

    If CurrentSch = Nil Then Exit;

    // Define search radius (in internal units - adjust as needed)
    // Typical schematic grid is 10 mils = 100000 internal units
    SearchRadius := 50000; // About 5 mils

    // Create a spatial iterator around the clicked point
    SpatialIterator := CurrentSch.SchIterator_Create;
    If SpatialIterator = Nil Then Exit;

    Try
        SpatialIterator.AddFilter_ObjectSet(MkSet(eSchComponent));
        SpatialIterator.AddFilter_Area(X - SearchRadius, Y - SearchRadius,
                                       X + SearchRadius, Y + SearchRadius);

        GraphicalObj := SpatialIterator.FirstSchObject;
        While GraphicalObj <> Nil Do
        Begin
            If GraphicalObj.ObjectId = eSchComponent Then
            Begin
                Result := GraphicalObj;
                Exit; // Return first component found
            End;
            GraphicalObj := SpatialIterator.NextSchObject;
        End;

    Finally
        CurrentSch.SchIterator_Destroy(SpatialIterator);
    End;
End;

{..............................................................................}
Procedure Run;
Var
    CurrentSch          : ISch_Document;
    DestinationList     : TList;
    Iterator            : ISch_Iterator;
    DestinationCmp      : ISch_Component;
    SourceCmp           : ISch_Component;
    AComponent          : ISch_Component;
    SrcData             : TStringList;
    DestDesignator      : String;
    SrcDesignator       : String;
    ClickLocation       : TLocation;
    i                   : Integer;
    AppliedCount        : Integer;
Begin
    If SchServer = Nil Then Exit;

    CurrentSch := SchServer.GetCurrentSchDocument;
    If CurrentSch = Nil Then Exit;

    DestinationList := TList.Create;
    Try
        // Step 1: Get currently selected destination component(s)
        Iterator := CurrentSch.SchIterator_Create;
        Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

        Try
            AComponent := Iterator.FirstSchObject;
            While AComponent <> Nil Do
            Begin
                If AComponent.Selection Then
                Begin
                    DestinationList.Add(AComponent);
                End;

                AComponent := Iterator.NextSchObject;
            End;
        Finally
            CurrentSch.SchIterator_Destroy(Iterator);
        End;

        If DestinationList.Count = 0 Then
        Begin
            ShowMessage('Please select destination component(s) first, then run the script');
            Exit;
        End;

        // Build designator list
        DestDesignator := '';
        For i := 0 to DestinationList.Count - 1 Do
        Begin
            DestinationCmp := DestinationList.Items[i];
            If DestinationCmp.Designator <> Nil Then
            Begin
                If i > 0 Then DestDesignator := DestDesignator + ', ';
                DestDesignator := DestDesignator + DestinationCmp.Designator.Text;
            End;
        End;

        // Step 2: Prompt user to click on SOURCE component
        ClickLocation := TLocation;
        ClickLocation.X := 0;
        ClickLocation.Y := 0;

        If Not CurrentSch.ChooseLocationInteractively(ClickLocation,
                                                       'Click on SOURCE component to copy parameter placement from') Then
        Begin
            Exit;
        End;

        // Find component at clicked location
        SourceCmp := GetComponentAtLocation(CurrentSch, ClickLocation.X, ClickLocation.Y);

        If SourceCmp = Nil Then
        Begin
            ShowMessage('No component found at clicked location. Please try again.');
            Exit;
        End;

        // Step 3: Get parameter positions from source component
        If SourceCmp.Designator <> Nil Then
            SrcDesignator := SourceCmp.Designator.Text
        Else
            SrcDesignator := 'Unknown';

        SrcData := GetSrcPositionsFromComponent(SourceCmp);

        If SrcData.Count = 0 Then
        Begin
            ShowMessage('No parameter data found on source component: ' + SrcDesignator);
            SrcData.Free;
            Exit;
        End;

        // Step 4: Apply to all destination components
        SchServer.ProcessControl.PreProcess(CurrentSch, '');

        AppliedCount := 0;
        For i := 0 to DestinationList.Count - 1 Do
        Begin
            DestinationCmp := DestinationList.Items[i];
            If SetPositionsForComponent(DestinationCmp, SrcData) Then
                AppliedCount := AppliedCount + 1;
        End;

        SchServer.ProcessControl.PostProcess(CurrentSch, '');
        CurrentSch.GraphicallyInvalidate;

        ShowMessage('Parameter placement copied successfully!' + #13#10 +
                   'Source: ' + SrcDesignator + #13#10 +
                   'Applied to ' + IntToStr(AppliedCount) + ' component(s): ' + DestDesignator);

        // Cleanup
        SrcData.Free;

    Finally
        DestinationList.Free;
    End;
End;
{..............................................................................}
