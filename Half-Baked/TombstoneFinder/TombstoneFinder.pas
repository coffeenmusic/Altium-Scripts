

// Check if two layers are the on the same side of the board. Handle different layer names.
function Is_Same_Side(Obj1: IPCB_ObjectClass, Obj2: IPCB_ObjectClass): Boolean;
var
   Layer1, Layer2 : Integer;
begin
    Layer1 := Obj1.Layer; Layer2 := Obj2.Layer;
    if Obj1.ObjectId = eComponentBodyObject then Layer1 := Obj1.Component.Layer;
    if Obj2.ObjectId = eComponentBodyObject then Layer2 := Obj2.Component.Layer;

    // Top Layer
    if (Layer1 = eTopLayer) or (Layer1 = eTopOverlay) then
    begin
        if (Layer2 <> eBottomLayer) and (Layer2 <> eBottomOverlay) then
        begin
              result := True; Exit; // return True
        end;
    end
    // Bottom Layer
    else if (Layer1 = eBottomLayer) or (Layer1 = eBottomOverlay) then
    begin
         if (Layer2 <> eTopLayer) and (Layer2 <> eTopOverlay) then
         begin
              result := True; Exit; // return True
         end;
    end
    // Multi Layer
    else if (Layer1 = eMultiLayer) or (Layer2 = eMultiLayer) then
    begin
         result := True; Exit;
    end;

    result := False;
end;

// May want different Bounding Rectangles depending on the object
function Get_Obj_Rect(Obj: IPCB_ObjectClass): TCoordRect;
var
    Rect    : TCoordRect;
    ObjID : Integer;
begin
    ObjID := Obj.ObjectId;
    if ObjID = eBoardObject then
    begin
        Rect := Obj.BoardOutline.BoundingRectangle;
    end
    else if ObjID = eComponentObject then
    begin
        //Rect := Obj.BoundingRectangleNoNameComment;
        Rect := Obj.BoundingRectangleNoNameCommentForSignals;
    end
    else
    begin
        Rect := Obj.BoundingRectangle;
    end;

    result := Rect;
end;

function Is_Outside_Board(Board: IPCB_Board, Obj: IPCB_ObjectClass): Boolean;
var
    BoardRect, Rect    : TCoordRect;
begin
    Rect := Get_Obj_Rect(Obj);
    BoardRect := Get_Obj_Rect(Board);

    if (Rect.Left < BoardRect.Left) or
       (Rect.Right > BoardRect.Right) or
       (Rect.Bottom < BoardRect.Bottom) or
       (Rect.Top > BoardRect.Top)
    then
    begin
         result := True;
         Exit; // return
    end;

    result := False;
end;

function TrackInPad(Board: IPCB_Board, Pad: IPCB_Pad, Track: IPCB_Track): Boolean;
var
    Rect    : TCoordRect;
    L, R, T, B  : Integer;
    x, x2, y, y2, dx, dy : Integer;
    InPad1, InPad2 : Boolean;
begin
    // Continue if Layers Dont Match
    if not Is_Same_Side(Pad, Track) then
    begin
         result := False;
         Exit; // Continue
    end;

    Rect := Get_Obj_Rect(Pad);

    // Get Bounding Area For Both Objects
    L := Rect.Left;
    R := Rect.Right;
    T := Rect.Top;
    B := Rect.Bottom;

    x := Track.x1;
    y := Track.y1;

    x2 := Track.x2;
    y2 := Track.y2;

    InPad1 := (x < R) and (x > L) and (y < T) and (y > B);
    InPad2 := (x2 < R) and (x2 > L) and (y2 < T) and (y2 > B);

    if (InPad1 and not(InPad2)) or (InPad2 and not(InPad1)) then
    begin
        result := True;
        Exit;
    end;
    // Handle case where lines aren't straight through
    result := False;
end;

function TrackOverPadWidth(Board: IPCB_Board, Pad: IPCB_Pad): Integer;
var
    Iterator      : IPCB_SpatialIterator;
    Track          : IPCB_Track;
    Rect : TCoordRect;
    RectL,RectR,RectB,RectT : TCoord;
    Name : TPCBString;
    Width : Integer;
    Coord : IPCB_Coordinate;
begin
    Rect := Get_Obj_Rect(Pad);
    RectL := Rect.Left;
    RectR := Rect.Right;
    RectT := Rect.Top;
    RectB := Rect.Bottom;

    Iterator := Board.SpatialIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eTrackObject)); // TODO: Handle Arcs eArcObject
    Iterator.AddFilter_LayerSet(MkSet(eTopLayer, eBottomLayer));
    Iterator.AddFilter_Area(RectL, RectB, RectR, RectT);

    Width := 0;
    Track := Iterator.FirstPCBObject;
    While Track <> NIL Do
    Begin
        If (Track.Layer_V6 = Pad.Layer_V6) and TrackInPad(Board, Pad, Track) Then
        Begin
             Width := Width + CoordToMils(Track.Width);
        End;

        Track := Iterator.NextPCBObject;
    End;
    Board.SpatialIterator_Destroy(Iterator);

    result := Width;
end;

function PolyOverPadWidth(Board: IPCB_Board, Pad: IPCB_Pad): Integer;
var
    Iterator      : IPCB_SpatialIterator;
    Poly          : IPCB_Polygon;
    PolyTrack     : IPCB_Object;
    Rect : TCoordRect;
    RectL,RectR,RectB,RectT : TCoord;
    Width : Integer;
    Coord : IPCB_Coordinate;
    ReliefCnt : Integer;
    ReliefWidth : Integer;
    TrackSize : TCoord;
    PolyTrackIter : IPCB_GroupIterator;
begin
    Rect := Get_Obj_Rect(Pad);
    RectL := Rect.Left;
    RectR := Rect.Right;
    RectT := Rect.Top;
    RectB := Rect.Bottom;

    Iterator := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(ePolyObject));
    Iterator.AddFilter_LayerSet(MkSet(eTopLayer, eBottomLayer));
    Iterator.AddFilter_Method(eProcessAll);

    Width := 0;
    Poly := Iterator.FirstPCBObject;
    While Poly <> NIL Do
    Begin
        If Poly.Layer = Pad.Layer Then
        Begin
        //    Poly.Selected := True;
        //    ReliefCnt := Poly.ReliefEntries;
        //    ReliefWidth := Poly.ReliefConductorWidth;
        //    TrackSize := Poly.TrackSize;
            //Width := Width + CoordToMils(Poly.Width);
        //    Width := ReliefWidth;
        //    Client.SendMessage('PCB:Zoom', 'Action=Selected' , 255, Client.CurrentView);
        //    Poly.Selected := False;
        //End;
        //Poly.Selected := True;
        //Client.SendMessage('PCB:Zoom', 'Action=Selected' , 255, Client.CurrentView);

        //ShowMessage(BoolToStr(Poly.PrimitiveInsidePoly(Pad)));
        //Poly.Selected := False;

        PolyTrackIter := Poly.GroupIterator_Create;
        PolyTrackIter.AddFilter_ObjectSet(MkSet(ePadObject, eTrackObject, eComponentObject));

        PolyTrack := PolyTrackIter.FirstPCBObject;
        While PolyTrack <> NIL Do
        Begin
            PolyTrack.Selected := True;

            PolyTrack := PolyTrackIter.NextPCBObject;
        End;
        Poly.GroupIterator_Destroy(PolyTrackIter);
        End;

        Poly := Iterator.NextPCBObject;
    End;
    Board.SpatialIterator_Destroy(Iterator);

    result := Width;
end;

function Get_Pad_Count(Cmp: IPCB_Component): Integer;
var
    PadCnt : Integer;
    PadIterator     : IPCB_GroupIterator;
    Pad            : IPCB_Pad;
begin
    PadIterator := Cmp.GroupIterator_Create;
    PadIterator.AddFilter_ObjectSet(MkSet(ePadObject));

    PadCnt := 0;
    Pad := PadIterator.FirstPCBObject;
    While (Pad <> Nil) Do
    Begin
        If Pad.Layer = eTopLayer Then
        Begin
            Inc(PadCnt);
        End;
        Pad := PadIterator.NextPCBObject;
    End;
    Cmp.GroupIterator_Destroy(PadIterator);

    result := PadCnt;
end;

function Get_Thermal_Ratio(Board: IPCB_Board, Cmp: IPCB_Component): Integer;
var
    Cnt1, Cnt2 : Integer;
    PadIterator     : IPCB_GroupIterator;
    TrackIterator : IPCB_GroupIterator;
    Pad            : IPCB_Pad;
    Track : IPCB_Track;
    Ratio :   Double;
    W1, W2, Width : Integer;
    StringDes : TPCBString;
    PadCnt : Integer;
begin
    PadIterator := Cmp.GroupIterator_Create;
    PadIterator.AddFilter_ObjectSet(MkSet(ePadObject));

    W1 := 0;
    W2 := 0;
    Width := 0;
    Ratio := 0;
    PadCnt := 1;
    Pad := PadIterator.FirstPCBObject;
    While (Pad <> Nil) Do
    Begin
        If Pad.Layer = Cmp.Layer Then
        Begin
            StringDes := Pad.Name;
            Width := TrackOverPadWidth(Board, Pad);
            //PolyOverPadWidth(Board, Pad);

            If PadCnt = 1 Then
            Begin
                W1 := Width;
            End
            Else If PadCnt = 2 Then
            Begin
                W2 := Width;
            End;
            Inc(PadCnt);
        End;
        Pad := PadIterator.NextPCBObject;
    End;
    Cmp.GroupIterator_Destroy(PadIterator);

    // If infinite ratio
    If (W1 = 0) or (W2 = 0) Then
    Begin
        result := 1000000; // Large arbitrary number
        Exit;
    End;

    Ratio := W1/W2;
    If (W2/W1) > Ratio Then
    Begin
        Ratio := W2/W1;
    End;
    result := Ratio;
end;

{..............................................................................}
Procedure RunFindTombstones;
Const
    AREA_0603 = 10000; // mils (Approximation & Includes Courtyard)
    MAX_RATIO = 2.5;
Var
    Board          : IPCB_Board;
    Iterator       : IPCB_BoardIterator;
    Filter_Size    : Integer;
    Cmp            : IPCB_Component;
    Pad            : IPCB_Pad;
    PadIterator     : IPCB_GroupIterator;
    PadCnt          : Integer;
    CmpRect         : TCoordRect;
    CmpArea            : Integer;
    Des                : TPCBString;
    Ratio : Double;
    KeepRunning : Boolean;
    Polygon       : IPCB_Polygon;
Begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    // Create the iterator that will look for Component Body objects only
    Iterator := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eComponentObject));
    Iterator.AddFilter_IPCB_LayerSet(LayerSet.AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    Cmp := Iterator.FirstPCBObject;
    While (Cmp <> Nil) Do
    Begin
        If Not(Is_Outside_Board(Board, Cmp)) Then
        Begin
            CmpRect := Get_Obj_Rect(Cmp);
            CmpArea := CoordToMils(CmpRect.Right - CmpRect.Left)*CoordToMils(CmpRect.Top - CmpRect.Bottom);

            // If component smaller than 0603
            If CmpArea < AREA_0603 Then
            Begin
                    PadCnt := Get_Pad_Count(Cmp);
                    If PadCnt = 2 Then
                    Begin
                        Ratio := Get_Thermal_Ratio(Board, Cmp);
                        If (Ratio >= MAX_RATIO) and (Ratio < 100) Then
                        Begin
                           Des := Cmp.Name.Text;
                           Cmp.Selected := True;
                           Client.SendMessage('PCB:Zoom', 'Action=Selected' , 255, Client.CurrentView);
                           KeepRunning := ConfirmNoYes('Thermal Ratio: ' + FloatToStr(Ratio) + '  Continue?');
                           If not(KeepRunning) Then
                               Break;
                           Cmp.Selected := False;
                        End;
                    End;
            End;
        End;

        Cmp := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);

    ShowMessage('Script Complete.');
End;
{..............................................................................}

{..............................................................................}
