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
    PolyIterator  : IPCB_BoardIterator;
    Poly          : IPCB_Polygon;
    PolyGroupIter : IPCB_GroupIterator;
    PolyPrimitive : IPCB_Object;
    Region        : IPCB_Region;
    Rect          : TCoordRect;
    RectL,RectR,RectB,RectT : TCoord;
    Width : Integer;
    PadCx, PadCy : TCoord;
    ThermalWidth : Integer;
    i, j : Integer;
    HoleContour : IPCB_Contour;
    MainContour : IPCB_Contour;
    MinX, MaxX, MinY, MaxY : TCoord;
    HoleCx, HoleCy : TCoord;
    NumSpokes : Integer;
    EstimatedSpokeWidth : Integer;
begin
    Rect := Get_Obj_Rect(Pad);
    RectL := Rect.Left;
    RectR := Rect.Right;
    RectT := Rect.Top;
    RectB := Rect.Bottom;

    // Get pad center for distance calculations
    PadCx := (RectL + RectR) div 2;
    PadCy := (RectT + RectB) div 2;

    // Use board iterator to find all polygons
    PolyIterator := Board.BoardIterator_Create;
    PolyIterator.AddFilter_ObjectSet(MkSet(ePolyObject));
    PolyIterator.AddFilter_LayerSet(MkSet(eTopLayer, eBottomLayer));
    PolyIterator.AddFilter_Method(eProcessAll);

    Width := 0;
    Poly := PolyIterator.FirstPCBObject;
    While Poly <> NIL Do
    Begin
        // Check if polygon is on same layer and net as pad
        // This is the primary check - if they're on same net, thermal relief would apply
        If (Poly.Layer = Pad.Layer) and (Poly.Net = Pad.Net) Then
        Begin
            ThermalWidth := 0;

            // For solid polygons, get the Region object and analyze it
            If Poly.PolyHatchStyle = ePolySolid Then
            Begin
                // Create group iterator to access polygon's Region primitives
                PolyGroupIter := Poly.GroupIterator_Create;
                PolyGroupIter.AddFilter_ObjectSet(MkSet(eRegionObject));

                PolyPrimitive := PolyGroupIter.FirstPCBObject;
                While PolyPrimitive <> NIL Do
                Begin
                    If PolyPrimitive.ObjectId = eRegionObject Then
                    Begin
                        Region := PolyPrimitive;

                        // Check each hole in the region to see if it's around our pad
                        // Thermal reliefs create holes around pads with narrow bridges
                        For i := 0 To Region.HoleCount - 1 Do
                        Begin
                            HoleContour := Region.Holes[i];

                            // Calculate bounding box of the hole manually
                            If HoleContour.Count > 0 Then
                            Begin
                                MinX := HoleContour.x[0];
                                MaxX := HoleContour.x[0];
                                MinY := HoleContour.y[0];
                                MaxY := HoleContour.y[0];

                                For j := 1 To HoleContour.Count - 1 Do
                                Begin
                                    If HoleContour.x[j] < MinX Then MinX := HoleContour.x[j];
                                    If HoleContour.x[j] > MaxX Then MaxX := HoleContour.x[j];
                                    If HoleContour.y[j] < MinY Then MinY := HoleContour.y[j];
                                    If HoleContour.y[j] > MaxY Then MaxY := HoleContour.y[j];
                                End;

                                // Calculate hole center
                                HoleCx := (MinX + MaxX) div 2;
                                HoleCy := (MinY + MaxY) div 2;

                                // Check if this hole is centered around our pad
                                // Allow some tolerance for the hole to be slightly offset
                                If (Abs(HoleCx - PadCx) < MilsToCoord(20)) and
                                   (Abs(HoleCy - PadCy) < MilsToCoord(20)) Then
                                Begin
                                    // This hole is likely a thermal relief around our pad
                                    // The thermal spokes are the copper that remains between the hole and pad

                                    // Use polygon's relief conductor width if available
                                    If Poly.ReliefConductorWidth > 0 Then
                                    Begin
                                        // Polygon has defined relief conductor width
                                        NumSpokes := Poly.ReliefEntries;
                                        If NumSpokes <= 0 Then NumSpokes := 4; // Default to 4 spokes
                                        ThermalWidth := CoordToMils(Poly.ReliefConductorWidth) * NumSpokes;
                                    End
                                    Else
                                    Begin
                                        // Estimate based on typical thermal patterns
                                        // Usually 4 spokes of about 10-20 mils each for small components
                                        ThermalWidth := 40; // Conservative estimate: 4 spokes × 10 mils
                                    End;

                                    Break; // Found the thermal relief hole for this pad
                                End;
                            End;
                        End;
                    End;

                    PolyPrimitive := PolyGroupIter.NextPCBObject;
                End;

                Poly.GroupIterator_Destroy(PolyGroupIter);
            End
            // For hatched polygons, the tracks ARE the copper pattern
            Else If (Poly.PolyHatchStyle <> ePolyNoHatch) Then
            Begin
                // For hatched polygons, check if there are track primitives
                PolyGroupIter := Poly.GroupIterator_Create;
                PolyGroupIter.AddFilter_ObjectSet(MkSet(eTrackObject));

                PolyPrimitive := PolyGroupIter.FirstPCBObject;
                While PolyPrimitive <> NIL Do
                Begin
                    If PolyPrimitive.ObjectId = eTrackObject Then
                    Begin
                        // Check if track connects to pad
                        If TrackInPad(Board, Pad, PolyPrimitive) Then
                        Begin
                            ThermalWidth := ThermalWidth + CoordToMils(PolyPrimitive.Width);
                        End;
                    End;

                    PolyPrimitive := PolyGroupIter.NextPCBObject;
                End;

                Poly.GroupIterator_Destroy(PolyGroupIter);
            End;

            // Add thermal width from this polygon to total
            Width := Width + ThermalWidth;
        End;

        Poly := PolyIterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(PolyIterator);

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
        If Pad.Layer = Cmp.Layer Then
        Begin
            Inc(PadCnt);
        End;
        Pad := PadIterator.NextPCBObject;
    End;
    Cmp.GroupIterator_Destroy(PadIterator);

    result := PadCnt;
end;

function Get_Thermal_Ratio(Board: IPCB_Board, Cmp: IPCB_Component): Double;
var
    Cnt1, Cnt2 : Integer;
    PadIterator     : IPCB_GroupIterator;
    TrackIterator : IPCB_GroupIterator;
    Pad            : IPCB_Pad;
    Track : IPCB_Track;
    Ratio :   Double;
    W1, W2, Width : Integer;
    TrackWidth, PolyWidth : Integer;
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

            // Get width from both tracks and polygons
            TrackWidth := TrackOverPadWidth(Board, Pad);
            PolyWidth := PolyOverPadWidth(Board, Pad);
            Width := TrackWidth + PolyWidth;

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

    // If infinite ratio (one pad has no thermal connection)
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

    ShowMessage('Tombstone Finder: Checking for thermal imbalance in small components.' + #13#10 +
                'Now includes polygon thermal relief analysis.');

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
                           KeepRunning := ConfirmNoYes('Component: ' + Des + #13#10 +
                                                       'Thermal Ratio: ' + FloatToStr(Ratio) + #13#10 +
                                                       '(Includes tracks and polygon thermals)' + #13#10#13#10 +
                                                       'Continue searching?');
                           If not(KeepRunning) Then
                               Exit;
                           Cmp.Selected := False;
                        End;
                    End;
            End;
        End;

        Cmp := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);

    ShowMessage('Tombstone check complete.');
End;
{..............................................................................}

{..............................................................................}
