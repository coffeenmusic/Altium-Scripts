var
    Board : IPCB_Board;

Function GetAssemblyBoundingRectangle(Cmp: IPCB_Component): TStringList;
Var
    GrpIter       : IPCB_GroupIterator;
    Track : IPCB_Track;
    xMin, xMax, yMin, yMax : Float;
    i : integer;
    Rect : TStringList;
Begin
    GrpIter := Cmp.GroupIterator_Create;
    GrpIter.AddFilter_LayerSet(MkSet(eMechanical2, eMechanical3));
    GrpIter.AddFilter_ObjectSet(MkSet(eTrackObject));

    i := 0;
    Track := GrpIter.FirstPCBObject;
    While (Track <> Nil) Do
    Begin

        if i = 0 Then
        Begin
            xMin := min(Track.x1, Track.x2);
            xMax := max(Track.x1, Track.x2);
            yMin := min(Track.y1, Track.y2);
            yMax := max(Track.y1, Track.y2);
        End
        Else
        Begin
            xMin := min(xMin, min(Track.x1, Track.x2));
            xMax := max(xMax, max(Track.x1, Track.x2));
            yMin := min(yMin, min(Track.y1, Track.y2));
            yMax := max(yMax, max(Track.y1, Track.y2));
        End;

        Track := GrpIter.NextPCBObject;
        Inc(i);
    End;
    Cmp.GroupIterator_Destroy(GrpIter);

    //Client.SendMessage('PCB:Deselect', 'Scope=All' , 255, Client.CurrentView); // Zoom to selected

    Rect := TStringList.Create;
    Rect.Add(xMin);
    Rect.Add(xMax);
    Rect.Add(yMin);
    Rect.Add(yMax);
    result := Rect;
End;

Function Resize_Font(Txt: IPCB_Text, l: Float, r: Float, t: Float, b: Float, AspectRatio: Float);
Var
    NewSize, NewWidth, dim : Integer;
    w, h : Float;
Begin
    w := r - l;
    h := t - b;

    If AspectRatio > 0.9 Then
    Begin
        dim := min(h, w/Length(Txt.Text));
    End
    Else
    Begin
        dim := min(w, h/Length(Txt.Text));
    End;

    NewSize := Trunc(CoordToMils(dim/1.2));
    NewWidth := Trunc(NewSize/8);
    Txt.Size := MilsToCoord(NewSize);
    Txt.Width := MilsToCoord(NewWidth);
End;

Function Resize_Box(Txt: IPCB_Text, l: Float, r: Float, t: Float, b: Float);
Var
    x1, x2, y1, y2, x_off, y_off : Integer;
    TmpDel : String;
    rot : Float;
Begin
    Txt.X2Location := Txt.XLocation + abs(r - l);
    Txt.Y2Location := Txt.YLocation + abs(t - b);
End;

{..............................................................................}
Procedure Run;
Var
    //Board         : IPCB_Board;
    Cmp           : IPCB_Component;
    Iterator      : IPCB_BoardIterator;
    GrpIter       : IPCB_GroupIterator;
    Count         : Integer;
    Txt : IPCB_Text;
    TxtStr, TxtRot, CmpRot, CmpDes : String;
    Rect, TxtR: TStringList;
    IsDes, IsHoriz, IsVisible : Boolean;
    AspectRatio, cx, cy, l, r, t, b, xT, yT, rot : Float;
    NormalRotations : TStringList;
Begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    // Create the iterator that will look for Component Body objects only
    Iterator        := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eComponentObject));
    Iterator.AddFilter_IPCB_LayerSet(LayerSet.AllLayers);
    Iterator.AddFilter_Method(eProcessAll); //eProcessAll

    NormalRotations := TStringList.Create;
    NormalRotations.Add('0');
    NormalRotations.Add('90');
    NormalRotations.Add('180');
    NormalRotations.Add('270');
    NormalRotations.Add('360');

    Count := 0;
    Cmp := Iterator.FirstPCBObject;
    While (Cmp <> Nil) Do
    Begin
        CmpDes := Cmp.Name.Text;
        IsVisible := Not(Cmp.Name.IsHidden);



        GrpIter := Cmp.GroupIterator_Create;
        GrpIter.AddFilter_LayerSet(MkSet(eMechanical2, eMechanical3));
        GrpIter.AddFilter_ObjectSet(MkSet(eTextObject));
        Txt := GrpIter.FirstPCBObject;
        While (Txt <> nil) And (IsVisible) Do
        Begin

            If  Txt.Text = CmpDes Then
            Begin
                //Rect := Cmp.BoundingRectangleNoNameComment;
                Rect := GetAssemblyBoundingRectangle(Cmp);
                l := Rect.Get(0);
                r := Rect.Get(1);
                b := Rect.Get(2);
                t := Rect.Get(3);
                cx := l + (r - l)/2; // Center X
                cy := b + (t - b)/2; // Center Y
                AspectRatio := (r - l)/(t - b);
                rot := Txt.Rotation;

                PCBServer.PreProcess;
                PCBServer.SendMessageToRobots(Txt.I_ObjectAddress, c_Broadcast, PCBM_BeginModify, c_NoEventData);

                Resize_Font(Txt, l, r, t, b, AspectRatio);



                // Get Text Center coords that are agnostic to the text justification setting
                xT := min(Txt.X1Location, Txt.X2Location) + Abs(Txt.X1Location - Txt.X2Location)/2;
                yT := min(Txt.Y1Location, Txt.Y2Location) + Abs(Txt.Y1Location - Txt.Y2Location)/2;

                Txt.MoveByXY(cx - xT, cy - yT); // Center Text In Assembly Border

                // Set rotation to 0 degrees, but rotate about center
                Txt.RotateAroundXY(cx, cy, -Txt.Rotation);

                //Resize_Box(Txt, l, r, t, b);

                If AspectRatio <= 0.9 Then Txt.RotateAroundXY(cx, cy, 90); // Rotate to 90 deg

                If NormalRotations.IndexOf(IntToStr(Cmp.Rotation)) = -1 Then
                Begin
                    If (Cmp.Rotation = 45) or (Cmp.Rotation = 225) Then
                    Begin
                        Txt.RotateAroundXY(cx, cy, 45);
                    End
                    Else If (Cmp.Rotation = 315) or (Cmp.Rotation = 135) Then
                    Begin
                        Txt.RotateAroundXY(cx, cy, 315); 
                    End
                    Else
                    Begin
                        Txt.RotateAroundXY(cx, cy, Cmp.Rotation);
                    End;
                End;

                PCBServer.SendMessageToRobots(Txt.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_NoEventData);
                PCBServer.PostProcess;
            End;

            Txt := GrpIter.NextPCBObject;
        End;
        Cmp.GroupIterator_Destroy(GrpIter);

        Inc(Count);
        Cmp := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);

    //Refresh the screen
    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

    //Rect.Destroy;

    ShowMessage('Complete');
End;
{..............................................................................}

{..............................................................................}
