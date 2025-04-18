var
    Board    : IPCB_Board;
    ResultList : TStringList;
    SmartPlace, RunningGUI: Boolean;

function GetTrackRotation(trk: IPCB_Track, Center: Boolean) : Double;
var
    Rot: Double;
    dx, dy: Double;
begin
    dx := trk.x2 - trk.x1;
    dy := trk.y2 - trk.y1;

    Rot := RadToDeg(arctan(dy/dx));

    if (dx >= 0) and (dy >= 0) then
    begin
        Rot := Rot + 0;
    end
    else if (dx < 0) and (dy >= 0) then
    begin
        Rot := Rot + 180;
    end
    else if (dx < 0) and (dy < 0) then
    begin
        Rot := Rot + 180;
    end
    else
    begin
        Rot := Rot + 360;
    end;

    // Return angle between 180 & negative 180 instead of 0 & 360
    if (Center) and (Rot > 180) then
    begin
        Rot := Rot - 360;
    end;

    result := Rot;
end;

function GetSelectedTrackList(NetName: String) : TInterfaceList;
var
    TrackList : TInterfaceList;
    i : Integer;
begin
   if Board.SelectecObjectCount = 0 then
   begin
      result := NIL;
      exit;
   end;

   TrackList := TInterfaceList.Create;
   for i := 0 to Board.SelectecObjectCount - 1 do
   begin
      if Board.SelectecObject[i].ObjectId = eTrackObject then
      begin
          if (NetName = '') or (Board.SelectecObject[i].Net.Name = NetName) then
          begin
              TrackList.Add(Board.SelectecObject[i]);
          end;
      end;
   end;
   result := TrackList;
end;

function GetSelectedNetList(Board: IPCB_Board) : TStringList;
var
    Nets : TStringList;
    trk: IPCB_Track;
    i : Integer;
begin
   if Board.SelectecObjectCount = 0 then exit;

   Nets := TStringList.Create;
   Nets.Sorted := True;
   Nets.Duplicates := dupIgnore;

   for i := 0 to Board.SelectecObjectCount - 1 do
   begin
      if Board.SelectecObject[i].ObjectId = eTrackObject then
      begin
         trk := Board.SelectecObject[i];
         if trk.Net.InDifferentialPair then Nets.Add(trk.Net.Name);
      end;
   end;
   result := Nets;
end;

function CalculateBump(trace_width: Double, diff_gap: Double, var side_len: Double, var top_len: Double) : Boolean;
var
    diff_pitch, bump_inner, bend45extra: Double;
begin
   diff_pitch := trace_width + diff_gap;
   bend45extra := 2*diff_pitch/Tan(DegToRad(135/2));
   bump_inner := (bend45extra/2)/(2*(Sqrt(2)-1));

   side_len := bump_inner/Sin(DegToRad(45));
   top_len := 3*trace_width;
end;

function CopyTrack(ReferenceTrack: IPCB_Track) : IPCB_Track;
var
    Seg: IPCB_Track;
begin
   Seg := PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default);
   Seg := ReferenceTrack.Replicate;
   result := Seg;
end;

function CopyList(TrackList: TInterfaceList, Reverse: Boolean) : TInterfaceList;
var
    i: Integer;
    copyList: TInterfaceList;
begin
    copyList := TInterfaceList.Create;
    for i:=0 to TrackList.Count - 1 do
    begin
        if Reverse then
        begin
            copyList.Add(TrackList[TrackList.Count - 1 - i]);
        end
        else
        begin
            copyList.Add(TrackList[i]);
        end;
    end;
    result := copyList;
end;

function GetMinTrackDistance(Track1: IPCB_Track, Track2: IPCB_Track): Double;
var
    minDist, tmpDist: Double;
begin
    minDist := abs(Track1.x1 - Track2.x1) + abs(Track1.y1 - Track2.y1);

    tmpDist := abs(Track1.x1 - Track2.x2) + abs(Track1.y1 - Track2.y2);
    if tmpDist < minDist then
    begin
        minDist := tmpDist;
    end;

    tmpDist := abs(Track1.x2 - Track2.x1) + abs(Track1.y2 - Track2.y1);
    if tmpDist < minDist then
    begin
        minDist := tmpDist;
    end;

    tmpDist := abs(Track1.x2 - Track2.x2) + abs(Track1.y2 - Track2.y2);
    if tmpDist < minDist then
    begin
        minDist := tmpDist;
    end;
    result := minDist;
end;

function GetNextConnectedTrack(OriginalTrackList: TInterfaceList, Net: String, PrevTrk: IPCB_Track, CurTrk: IPCB_Track) : IPCB_Track;
var
    TrackList, Touching: TInterfaceList;
    i, dist, minLen: Integer;
    trk, minTrk : IPCB_Track;
    uniqueId, PrevuniqueId, CuruniqueId: String;
    Rect : TCoordRect;
    minDist: Double;
begin
   TrackList := CopyList(OriginalTrackList, False);
   Touching := TInterfaceList.Create;

   if TrackList.IndexOf(CurTrk) >= 0 then
   begin
      TrackList.Remove(CurTrk);
   end
   else if TrackList.IndexOf(PrevTrk) >= 0 then
   begin
       TrackList.Remove(PrevTrk);
   end;

   for i := 0 to TrackList.Count - 1 do
   begin
      trk := TrackList[i];

      if Net = trk.Net.Name then
      begin
          dist := CoordToMils(Board.PrimPrimDistance(CurTrk, trk));
          if Board.PrimPrimDistance(CurTrk, trk) = 0 then
          begin
              Touching.Add(trk);
          end;
      end;
   end;

   if Touching.Count = 0 then
   begin
      result := nil;
   end
   else if Touching.Count = 1 then
   begin
      result := Touching[0];
   end
   else
   begin
      for i:=0 to Touching.Count - 1 do
      begin
          trk := Touching[i];
          if (i = 0) or (GetMinTrackDistance(CurTrk, trk) < minDist) then
          begin
              minDist := GetMinTrackDistance(CurTrk, trk);
              minTrk := trk;
          end;
      end;
      result := minTrk;
   end;
end;


function GetMatchingPairTrack(Track: IPCB_Track, TrackList: TInterfaceList) : IPCB_Track;
var
    MatchingTrack, Track2: IPCB_Track;
    i, minDistance: Integer;
begin
    for i:= 0 to TrackList.Count - 1 do
    begin
        Track2 := TrackList[i];
        if (i = 0) or (Board.PrimPrimDistance(Track, Track2) < minDistance) then
        begin
            minDistance := Board.PrimPrimDistance(Track, Track2);
            MatchingTrack := Track2;
        end;
    end;
    result := MatchingTrack;
end;

function GetEndTrack(OriginalTrackList: TInterfaceList, NetName: String) : IPCB_Track;
var
    TrackList: TInterfaceList;
    Track, PrevTrack: IPCB_Track;
    i: Integer;
    Rect: TCoordRect;
begin
   TrackList := CopyList(OriginalTrackList, False);

   for i:= 0 to TrackList.Count - 1 do
   begin
       Track := TrackList[i];
       if Track.Net.Name = NetName then break;
   end;

   while True do
   begin
       PrevTrack := Track;
       Track := GetNextConnectedTrack(TrackList, NetName, PrevTrack, Track);
       if (Track <> nil) and (TrackList.Count > 0) then
       begin
          TrackList.Remove(Track);
       end
       else
       begin
           result := PrevTrack;
           exit;
       end;
   end;
end;

function SortTrackList(TrackList: TInterfaceList) : TInterfaceList;
var
    Track, PrevTrack: IPCB_Track;
    i: Integer;
    SortedTrackList: TInterfaceList;
    NetName: String;
begin
    NetName := TrackList[0].Net.Name;
    Track := GetEndTrack(TrackList, TrackList[0].Net.Name);
    TrackList.Remove(Track);

    SortedTrackList := TInterfaceList.Create;
    SortedTrackList.Add(Track);

    while TrackList.Count > 0 do
    begin
       PrevTrack := Track;
       Track := GetNextConnectedTrack(TrackList, NetName, PrevTrack, Track);

       if (Track <> nil) and (TrackList.Count > 0) then
       begin
          SortedTrackList.Add(Track);
          TrackList.Remove(Track);
       end
       else
       begin
          break;
       end;
    end;

    result := SortedTrackList;
end;

function GetSlope(Track: IPCB_Track, var Slope: Double): Boolean;
begin
    result := True;
    if Track.x2 - Track.x1 = 0 then
    begin
        result := False;
        exit;
    end;
    Slope := (Track.y2 - Track.y1)/(Track.x2 - Track.x1);
end;

function GetCoordFromLocation(Track: IPCB_Track, PrevTrack: IPCB_Track, Location:String, var x: Integer, var y: Integer);
var
    x_cent, y_cent, x_beg, y_beg, x_end, y_end: Integer;
begin
    if (Track.x1 = PrevTrack.x1) and (Track.y1 = PrevTrack.y1) then
    begin
        x_cent := Track.x1; y_cent := Track.y1;
        x_end := Track.x2; y_end := Track.y2;
        x_beg := PrevTrack.x2; y_beg := PrevTrack.y2;
    end
    else if (Track.x1 = PrevTrack.x2) and (Track.y1 = PrevTrack.y2) then
    begin
        x_cent := Track.x1; y_cent := Track.y1;
        x_end := Track.x2; y_end := Track.y2;
        x_beg := PrevTrack.x1; y_beg := PrevTrack.y1;
    end
    else if (Track.x2 = PrevTrack.x1) and (Track.y2 = PrevTrack.y1) then
    begin
        x_cent := Track.x2; y_cent := Track.y2;
        x_end := Track.x1; y_end := Track.y1;
        x_beg := PrevTrack.x2; y_beg := PrevTrack.y2;
    end
    else if (Track.x2 = PrevTrack.x2) and (Track.y2 = PrevTrack.y2) then
    begin
        x_cent := Track.x2; y_cent := Track.y2;
        x_end := Track.x1; y_end := Track.y1;
        x_beg := PrevTrack.x1; y_beg := PrevTrack.y1;
    end;



    if LowerCase(Location) = 'begin' then
    begin
        x := x_beg; y := y_beg;
    end
    else if LowerCase(Location) = 'end' then
    begin
        x := x_end; y := y_end;
    end
    else // Center
    begin
        x := x_cent; y := y_cent;
    end;
end;

function SameTrack(Track1: IPCB_Track, Track2: IPCB_Track): Boolean;
begin
    result := False;
    if (((Track1.x1 = Track2.x1) and (Track1.y1 = Track2.y1)) and
       ((Track1.x2 = Track2.x2) and (Track1.y2 = Track2.y2))) or
       (((Track1.x2 = Track2.x1) and (Track1.y2 = Track2.y1)) and
       ((Track1.x1 = Track2.x2) and (Track1.y1 = Track2.y2))) then
    begin
        result := True;
    end;
end;

function GetAngleBetweenTracks(Track: IPCB_Track, PrevTrack: IPCB_Track): Double;
var
    Angle, m1, m2, r1, r2: Double;
    s1, s2: Boolean;
    x1, x2, y1, y2, px2, py2: Integer;
begin
    if (Track.x1 = PrevTrack.x1) and (Track.y1 = PrevTrack.y1) then
    begin
        x1 := Track.x1; y1 := Track.y1;
        x2 := Track.x2; y2 := Track.y2;
        px2 := PrevTrack.x2; py2 := PrevTrack.y2;
    end
    else if (Track.x1 = PrevTrack.x2) and (Track.y1 = PrevTrack.y2) then
    begin
        x1 := Track.x1; y1 := Track.y1;
        x2 := Track.x2; y2 := Track.y2;
        px2 := PrevTrack.x1; py2 := PrevTrack.y1;
    end
    else if (Track.x2 = PrevTrack.x1) and (Track.y2 = PrevTrack.y1) then
    begin
        x1 := Track.x2; y1 := Track.y2;
        x2 := Track.x1; y2 := Track.y1;
        px2 := PrevTrack.x2; py2 := PrevTrack.y2;
    end
    else if (Track.x2 = PrevTrack.x2) and (Track.y2 = PrevTrack.y2) then
    begin
        x1 := Track.x2; y1 := Track.y2;
        x2 := Track.x1; y2 := Track.y1;
        px2 := PrevTrack.x1; py2 := PrevTrack.y1;
    end;

    s1 := GetSlope(Track, m1);
    s2 := GetSlope(PrevTrack, m2);
    if (s1) and (s2) then
    begin
         Angle := RadToDeg(arctan((m2 - m1)/(1+m1*m2)));
    end
    else
    begin
         r1 := GetTrackRotation(Track, False);
         r2 := GetTrackRotation(PrevTrack, False);

         if (abs(r1) = 90) or (r1 = 270) then
         begin
             if y1 > y2 then r1 := 270;
             if y1 < y2 then r1 := 90;
         end;
         if (abs(r1) = 180) or (r1 = 0) then
         begin
             if x1 > x2 then r1 := 180;
             if x1 < x2 then r1 := 0;
         end;

         if (abs(r2) = 90) or (r2 = 270) then
         begin
             if y1 > py2 then r2 := 90;
             if y1 < py2 then r2 := 270;
         end;
         if (abs(r2) = 180) or (r2 = 0) then
         begin
             if x1 > px2 then r2 := 0;
             if x1 < px2 then r2 := 180;
         end;

         Angle :=  r2 - r1;
         Angle := Angle mod 90;
    end;
    result := Angle;
end;

function GetTrackRotationDelta(TrackList: TInterfaceList): Double;
var
   Rotation, PrevRotation, RotationDelta, TotalRotation: Double;
   i: Integer;
   trk: IPCB_Track;
   Sign: Double;
begin
   TotalRotation := 0;
   for i:=1 to TrackList.Count-1 do
   begin
       Rotation := GetAngleBetweenTracks(TrackList[i], TrackList[i-1]) mod 90;
       TotalRotation := TotalRotation + Rotation;
   end;
   result := TotalRotation;
end;

function GetPairedTracksOnly(gap: Double, TrackList1: TInterfaceList, TrackList2: TInterfaceList, var Diff1: TInterfaceList, var Diff2: TInterfaceList);
var
    i, j: Integer;
    trk1, trk2: IPCB_Track;
    rot1, rot2, dist: Double;
begin
    Diff1 := TInterfaceList.Create; Diff2 := TInterfaceList.Create;

    for i:=0 to TrackList1.Count - 1 do
    begin
        trk1 := TrackList1[i];
        for j:=0 to TrackList2.Count-1 do
        begin
            trk2 := TrackList2[j];
            dist := CoordToMils(Board.PrimPrimDistance(trk1, trk2));
            if dist = gap then
            begin
                rot1 := GetTrackRotation(trk1, False);
                rot2 := GetTrackRotation(trk2, False);
                if rot1 = rot2 then
                begin
                    Diff1.Add(trk1); Diff2.Add(trk2);
                end;
            end;
        end;
    end;
end;

function GetDiffPairBend(TrackList1: TInterfaceList, TrackList2: TInterfaceList, gap: Double) : Double;
var
    Track: IPCB_Track;
    i, trkLen: Integer;
    Bend1, Bend2 : Double;
    RotList1, RotList2, Diff1, Diff2: TInterfaceList;
begin
   GetPairedTracksOnly(gap, TrackList1, TrackList2, Diff1, Diff2);

   Bend1 := 0; Bend2 := 0;
   Bend1 := GetTrackRotationDelta(Diff1);
   Bend2 := GetTrackRotationDelta(Diff2);
   if Bend1 = Bend2 then
   begin
       result := Bend1;
       exit;
   end
   else
   begin
       result := -1;
       ShowMessage('Unable to calculate differential pair bend. Try running Route --> Retrace Selected before running the script.');
       exit;
   end;
end;

function GetTrackLength(TrackList: TInterfaceList) : Double;
var
    trk: IPCB_Track;
    i, trkLen: Integer;
    segLen: Double;
begin
   trkLen := 0; segLen := 0;

   for i := 0 to TrackList.Count - 1 do
   begin
       trk := TrackList[i];
       trkLen := trk.GetState_Length();
       segLen := segLen + CoordToMils(trkLen);
   end;

   result := segLen;
end;

function GetClosestDiffPair(Track: IPCB_Track, TrackList: TInterfaceList): IPCB_Track;
var
    trk, ClosestTrack: IPCB_Track;
    i, minDist: Integer;
    ClosestTracks: TInterfaceList;
    rot, rotClosest, minRot: Double;
begin
    for i:=0 to TrackList.Count -1 do
    begin
        trk := TrackList[i];

        if (i=0) or (Board.PrimPrimDistance(Track, trk) < minDist) then
        begin
           minDist := Board.PrimPrimDistance(Track, trk);
        end;
    end;

    ClosestTracks := TInterfaceList.Create;
    for i:=0 to TrackList.Count -1 do
    begin
        trk := TrackList[i];
        if Board.PrimPrimDistance(Track, trk) = minDist then
        begin
            ClosestTracks.Add(trk);
        end;
    end;

    if ClosestTracks.Count = 1 then
    begin
        ClosestTrack := ClosestTracks[0];
    end
    else if ClosestTracks.Count > 1 then
    begin
        rot := GetTrackRotation(Track, False);
        for i:=0 to ClosestTracks.Count-1 do
        begin
            trk := ClosestTracks[i];
            rotClosest := GetTrackRotation(trk, False);
            if (i=0) or (abs(rot - rotClosest) < minRot) then
            begin
                minRot := abs(rot - rotClosest);
                ClosestTrack := ClosestTracks[i];
            end;
        end;
    end
    else
    begin
        result := nil;
        exit;
    end;

    result := ClosestTrack;
end;

function GetClosestDiffPairDistance(Track: IPCB_Track, TrackList: TInterfaceList): Integer;
var
    trk, ClosestTrack: IPCB_Track;
begin
    ClosestTrack := GetClosestDiffPair(Track, TrackList);
    result := Board.PrimPrimDistance(Track, ClosestTrack);
end;

function GetNextLargest(NumberList: TList): Integer;
var
    i, val, minVal: Integer;
begin
    for i:=0 to NumberList.Count - 1 do
    begin
        val := NumberList.Items[i];
        if (i=0) or (val < minVal) then
        begin
            minVal := NumberList.Items[i];
        end;
    end;
    result := minVal;
end;

function SortNumberList(NumberList: TList): TList;
var
    nextNumber: Double;
    sorted: TList;
begin
    sorted := TList.Create;
    while NumberList.Count > 0 do
    begin
        nextNumber := GetNextLargest(NumberList);
        sorted.Add(nextNumber);
        NumberList.Remove(nextNumber);
    end;
    result := sorted;
end;

function GetDiffPairGap(TrackList1: TInterfaceList, TrackList2: TInterfaceList) : Double;
var
    i: Integer;
    GapList: TList;
    val: Double;
begin
   GapList := TList.Create;

   for i := 0 to TrackList1.Count - 1 do
   begin
      GapList.Add(GetClosestDiffPairDistance(TrackList1[i], TrackList2));
   end;

   GapList := SortNumberList(GapList);

   // Get median value
   result := CoordToMils(GapList[int(GapList.Count/2)]);
end;

function TrackClearanceGood(Track: IPCB_Track, minClearance: Integer, MatchingNet: String): Boolean;
const
    FILTER_PAD = 5; // mils
var
    Iterator: IPCB_SpatialIterator;
    Rect : TCoordRect;
    pad : Integer;
    Obj: IPCB_ObjectClass;
begin
    result := True;

    pad := MilsToCoord(FILTER_PAD);
    Rect := Track.BoundingRectangle;

    Iterator := Board.SpatialIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eViaObject, ePadObject, eTrackObject));
    Iterator.AddFilter_LayerSet(MkSet(eMultiLayer, Track.Layer));
    Iterator.AddFilter_Area(Rect.Left - pad, Rect.Bottom - pad, Rect.Right + pad, Rect.Top + pad);

    Obj := Iterator.FirstPCBObject;
    while Obj <> nil do
    begin
        if (Obj.ObjectId = eTrackObject) and ((Obj.Net.Name = Track.Net.Name) or (Obj.Net.Name = MatchingNet)) then
        begin
            Obj := Iterator.NextPCBObject;
            continue;
        end;

        if Board.PrimPrimDistance(Track, Obj) < minClearance then
        begin
            result := False;
            exit;
        end;
        Obj := Iterator.NextPCBObject;
    end;
    Board.SpatialIterator_Destroy(Iterator);
end;

function AddTrackToPCB(Track: IPCB_Track);
begin
    PCBServer.SendMessageToRobots(Track.I_ObjectAddress, c_Broadcast, PCBM_BeginModify, c_NoEventData);

    Board.AddPCBObject(Track);

    PCBServer.SendMessageToRobots(Track.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_NoEventData);
end;

function AddBumpToTrack(MatchingTrackList: TInterfaceList, gap: Double, var Track: IPCB_Track): Boolean;
const
    MIN_OTHER_OBJ_CLEARANCE = 4;
var
    side_len, run_len, width, flatLen, closestDist, dist : Double;
    Bump_Segment, Prev_Bump_Segment: IPCB_Track;
    i, j, x, y, direction, addRot: Integer;
    TrksAdded: TInterfaceList;
begin
   result := True;
   direction := 1; addRot := 0;
   TrksAdded := TInterfaceList.Create;

   CalculateBump(CoordToMils(Track.Width), gap, side_len, run_len);
   flatLen := 2*run_len + 2*(side_len/sqrt(2));

   // First Segment (Flat track)
   Bump_Segment := CopyTrack(Track);
   Bump_Segment.SetState_Length(MilsToCoord(run_len));
   AddTrackToPCB(Bump_Segment);
   TrksAdded.Add(Bump_Segment);

   x := Bump_Segment.x2;
   y := Bump_Segment.y2;

   // Second Segment (45 track)
   Prev_Bump_Segment := CopyTrack(Bump_Segment);
   Bump_Segment := CopyTrack(Bump_Segment);
   Bump_Segment.MoveToXY(x, y);
   Bump_Segment.SetState_Length(MilsToCoord(side_len));
   Bump_Segment.RotateBy(direction*45.0);
   closestDist := CoordToMils(GetClosestDiffPairDistance(Bump_Segment, MatchingTrackList));
   if closestDist < gap then
   begin
       direction := direction * -1;
       Bump_Segment.RotateBy(direction*90);
       if Board.PrimPrimDistance(Bump_Segment, Prev_Bump_Segment) <> 0 then
       begin
           Bump_Segment.RotateBy(180);
           Bump_Segment.MoveToXY(x, y);
       end;
   end;
   AddTrackToPCB(Bump_Segment);
   TrksAdded.Add(Bump_Segment);

   GetCoordFromLocation(Bump_Segment, Prev_Bump_Segment, 'end', x, y);

   // Third Segment (Top flat track)
   Prev_Bump_Segment := CopyTrack(Bump_Segment);
   Bump_Segment := CopyTrack(Bump_Segment);
   Bump_Segment.MoveToXY(x, y);
   if SameTrack(Bump_Segment, Prev_Bump_Segment) then
   begin
      addRot := 180; // Bump segment didn't move outside previous bump, so rotate 180
   end;
   Bump_Segment.SetState_Length(MilsToCoord(run_len));
   Bump_Segment.RotateBy((direction*-45.0)+addRot);
   AddTrackToPCB(Bump_Segment);
   TrksAdded.Add(Bump_Segment);

   GetCoordFromLocation(Bump_Segment, Prev_Bump_Segment, 'end', x, y);

   // Last Segment (-45 track)
   Prev_Bump_Segment := CopyTrack(Bump_Segment);
   Bump_Segment := CopyTrack(Bump_Segment);
   Bump_Segment.MoveToXY(x, y);
   Bump_Segment.SetState_Length(MilsToCoord(side_len));
   Bump_Segment.RotateBy(direction*-45.0);
   AddTrackToPCB(Bump_Segment);
   TrksAdded.Add(Bump_Segment);

   GetCoordFromLocation(Bump_Segment, Prev_Bump_Segment, 'end', x, y);

   // Verify Bump Clearances, Skip first track
   for i:=1 to TrksAdded.Count - 1 do
   begin
       if not TrackClearanceGood(TrksAdded[i], MilsToCoord(MIN_OTHER_OBJ_CLEARANCE), MatchingTrackList[0].Net.Name) then
       begin
           result := False;
           for j:=0 to TrksAdded.Count - 1 do
           begin
               Board.RemovePCBObject(TrksAdded[j]);
           end;

           // Add flat track
           Bump_Segment := CopyTrack(Track);
           Bump_Segment.SetState_Length(MilsToCoord(flatLen));
           AddTrackToPCB(Bump_Segment);
       break;
       end;
   end;

   // Update original track so it doesn't overlap new bump
   PCBServer.SendMessageToRobots(Track.I_ObjectAddress, c_Broadcast, PCBM_BeginModify , c_NoEventData);
   Track.SetState_Length(Track.GetState_Length() - MilsToCoord(flatLen));
   Track.MoveToXY(x, y);
   PCBServer.SendMessageToRobots(Track.I_ObjectAddress, c_Broadcast, PCBM_EndModify , c_NoEventData);
end;

function CompareRouteDistUpToTrack(Track: IPCB_Track, ShortList: TInterfaceList, LongList: TInterfaceList): Double;
var
    i: Integer;
    ClosestTrack: IPCB_Track;
    NewTrackList: TInterfaceList;
    shortLen, longLen: Double;
begin
    NewTrackList := TInterfaceList.Create;

    ClosestTrack := GetClosestDiffPair(Track, LongList);
    if ClosestTrack = nil then
    begin
        result := -1;
        exit;
    end;

    // Short Track Length
    for i:=0 to ShortList.Count - 1 do
    begin
        NewTrackList.Add(ShortList[i]);
        if ShortList[i] = Track then break;
    end;
    shortLen := GetTrackLength(NewTrackList);
    NewTrackList.Clear;

    // Long Track Length
    for i:=0 to LongList.Count - 1 do
    begin
        NewTrackList.Add(LongList[i]);
        if LongList[i] = ClosestTrack then break;
    end;
    longLen := GetTrackLength(NewTrackList);

    result := abs(longLen - shortLen);
end;

function SortTracksByLengthOffset(ShortList: TInterfaceList, LongList: TInterfaceList):TInterfaceList;
var
    i, j:Integer;
    NextLargestTrack: IPCB_Track;
    DistanceDelta, largest: Double;
    NewTracks, TrackList: TInterfaceList;
begin
    NewTracks := TInterfaceList.Create;
    TrackList := CopyList(ShortList, False);

    while TrackList.Count > 0 do
    begin
    largest := 0;
    for i:=0 to TrackList.Count - 1 do
    begin
        DistanceDelta := CompareRouteDistUpToTrack(TrackList[i], ShortList, LongList);
        if (i=0) or (DistanceDelta > largest) then
        begin
            largest := DistanceDelta;
            NextLargestTrack := TrackList[i];
        end;
    end;
    NewTracks.Add(NextLargestTrack);
    TrackList.Remove(NextLargestTrack);
    end;

    result := NewTracks;
end;

function GetTracksAroundBends(TrackList: TInterfaceList): TInterfaceList;
var
    i: Integer;
    PrevTrack, CurTrack, NextTrack : IPCB_Track;
    angle: Double;
    NewTrackList: TInterfaceList;
    keepIdx: TStringList;
begin
    NewTrackList := TInterfaceList.Create;

    keepIdx := TStringList.Create;
    keepIdx.Sorted := True;
    keepIdx.Duplicates := dupIgnore;

    angle := 0;
    for i:=1 to TrackList.Count - 2 do
    begin
         PrevTrack := TrackList[i-1];
         CurTrack := TrackList[i];
         NextTrack := TrackList[i+1];

         angle := GetAngleBetweenTracks(CurTrack, PrevTrack);
         angle := angle + GetAngleBetweenTracks(NextTrack, CurTrack);

         if abs(angle) >= 45 then
         begin
             keepIdx.Add(IntToStr(i-1));
             keepIdx.Add(IntToStr(i));
             keepIdx.Add(IntToStr(i+1));
         end;
    end;

    for i:=0 to keepIdx.Count-1 do
    begin
        NewTrackList.Add(TrackList[StrToInt(keepIdx[i])]);
    end;
    result := NewTrackList;
end;

function GetPairNet(Track: IPCB_Track, TrackList: TInterfaceList): String;
var
    trk: IPCB_Track;
    i, j, dist, minDist: Integer;
    pairNet: String;
begin
    j:=0;
    for i := 0 to TrackList.Count - 1 do
    begin
        trk := TrackList[i];
        if trk.Net.Name <> Track.Net.Name then
        begin
            dist := Board.PrimPrimDistance(Track, trk);
            if (j=0) or (dist < minDist) then
            begin
                minDist := dist;
                pairNet := trk.Net.Name;
            end;
            Inc(j);
        end;
    end;

    result := pairNet;
end;

function FilterTrackList(TrackList: TInterfaceList, NetName: String, LayerName: String): TInterfaceList;
var
    FilteredList: TInterfaceList;
    i: Integer;
    trk: IPCB_Track;
begin
    FilteredList := TInterfaceList.Create;
    for i:=0 to TrackList.Count - 1 do
    begin
        trk := TrackList[i];
        if ((NetName = '') or (trk.Net.Name = NetName)) and ((LayerName = '') or (trk.Layer = String2Layer(LayerName))) then FilteredList.Add(trk);
    end;
    result := FilteredList;
end;

function GetLayersFromTrackList(TrackList: TInterfaceList, NetName: String): TStringList;
var
    LayerList: TStringList;
    i: Integer;
    trk: IPCB_Track;
begin
    LayerList := TStringList.Create;
    LayerList.Duplicates := dupIgnore; // Ignore duplicates
    LayerList.Sorted := True;

    for i:=0 to TrackList.Count - 1 do
    begin
        trk := TrackList[i];
        if ((trk.Net.Name = NetName) or (NetName = '')) then
        begin
            LayerList.Add(Layer2String(trk.Layer));
        end;
    end;
    result := LayerList;
end;

function GetLayerCountFromTrackList(TrackList: TInterfaceList): Integer;
var
    LayerList: TStringList;
begin
    LayerList := GetLayersFromTrackList(TrackList, '');
    result := LayerList.Count;
end;

// Gets all newly added tracks for tracklist on layer with the same net
function UpdateTrackList(NetName: String, LayerName: String): TInterfaceList;
const
    FILTER_PAD = 2;
var
    i, pad:Integer;
    trk, trk2: IPCB_Track;
    Iterator      : IPCB_BoardIterator;
    Rect : TCoordRect;
    NewTrackList: TInterfaceList;
begin
    NewTrackList := TInterfaceList.Create;

    Iterator        := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject));
    Iterator.AddFilter_LayerSet(MkSet(String2Layer(LayerName)));
    Iterator.AddFilter_Method(eProcessAll);

    trk := Iterator.FirstPCBObject;
    While (trk <> Nil) Do
    Begin
        if (trk.Layer = String2Layer(LayerName)) and (trk.Net <> nil) and (trk.Net.Name = NetName) then
        begin
           NewTrackList.Add(trk);
        end;
        trk := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);

    result := NewTrackList;
end;

function GetDiffPairList(NetList: TStringList): TInterfaceList;
var
    i:Integer;
    Iterator      : IPCB_BoardIterator;
    diff: IPCB_DifferentialPair;
    layerName, diffName, NetPos, NetNeg, Test1, Test2: String;
    IsSelected: Boolean;
    DiffList : TInterfaceList;
begin
    Iterator        := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eDifferentialPairObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    DiffList := TInterfaceList.Create;

    i:=0;
    diff := Iterator.FirstPCBObject;
    While (diff <> Nil) Do
    Begin

        if (diff.PositiveNet = Nil) Or ((diff.NegativeNet = Nil)) then
        begin
            diff := Iterator.NextPCBObject;
            continue;
        end;
        NetPos := diff.PositiveNet.Name;
        NetNeg := diff.NegativeNet.Name;

        if (NetList.IndexOf(NetPos) >= 0) or (NetList.IndexOf(NetNeg) >= 0) then
        begin
            DiffList.Add(diff);
        end;

        Inc(i);
        diff := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);
    result := DiffList;
end;

function GetDiffPair(NetName: String, DiffList: TInterfaceList, var MatchingNet: String): IPCB_DifferentialPair;
var
    i: Integer;
    diff: IPCB_DifferentialPair;
    NetPos, NetNeg: String;
begin
    for i:=0 to DiffList.Count - 1 do
    begin
        diff := DiffList[i];
        NetPos := diff.PositiveNet.Name;
        NetNeg := diff.NegativeNet.Name;
        if (NetName = NetPos) or (NetName = NetNeg) then
        begin
            // Get matching net
            MatchingNet := NetNeg;
            if NetName = NetNeg then MatchingNet := NetPos;

            result := diff;
            exit;
        end;
    end;
end;

function GetDiffPairRuleList(Board:IPCB_Board): TInterfaceList;
var
    Iterator      : IPCB_BoardIterator;
    rule: IPCB_Rule;
    DiffPairRules: TInterfaceList;
begin
    DiffPairRules := TInterfaceList.Create;

    Iterator        := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eRuleObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    rule := Iterator.FirstPCBObject;
    While (rule <> Nil) Do
    Begin
        if rule.RuleKind = eRule_DifferentialPairsRouting then
        begin
           DiffPairRules.Add(rule);
        end;
        rule := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);

    result := DiffPairRules;
end;

function GetDiffPairRuleForTrack(RuleList: TInterfaceList, Track: IPCB_Track): IPCB_Rule;
var
    i: Integer;
    rule: IPCB_Rule;
begin
    for i:=0 to RuleList.Count-1 do
    begin
        rule := RuleList[i];
        if rule.Scope1Includes(Track) then
        begin
            result := rule;
            exit;
        end;
    end
end;

// Selects all tracks on layer if only one layer was selected. Selects all on multilayer if multiple layers were selected
function SelectAllTracks();
var
   i, maxTrack, StartLayerCnt, LayerCnt : Integer;
   TempList : TInterfaceList;
begin
     maxTrack := 0;
     i := 0;

     TempList := GetSelectedTrackList('');
     StartLayerCnt := GetLayerCountFromTrackList(TempList);

     While True Do
     Begin
          TempList := GetSelectedTrackList('');
          If TempList <> NIL Then
              Begin
              LayerCnt := GetLayerCountFromTrackList(TempList);

              If LayerCnt = StartLayerCnt Then
              Begin
                  If TempList.Count = maxTrack Then Break;
                  If TempList.Count > maxTrack Then maxTrack := TempList.Count;
              End;
          End;

          Client.SendMessage('PCB:SelectNext', 'SelectTopologyObjects = TRUE', 255, Client.CurrentView); // Iterate track selection

          i := i + 1;

          If i > 9 Then
          Begin
              ShowMessage('Timeout Exiting');
              Exit;
          End;
     End;
end;

function RunNoGUI;
const
   BUMPS_PER_45_DEG = 2;
   NEWLINECODE = #13#10;
   REPORT_LENGTHS = False;
   SMART_PLACE_DEFAULT = True;
var
   Arc      : IPCB_Arc;
   Track, Bump : IPCB_Track;
   gap, width: Double;
   i, layerCnt, layer_n, bmpChainCnt, bumpsNeeded, bumpsAdded: Integer;
   trkLen, trkLen2, trkBend : Double;
   NetList, LayerList : TStringList;
   shortLen, side_len, run_len, flat_len : Double;
   AllTracksList, TrackList1, TrackList2, ShortTrkList, LongTrkList, DiffList, RuleList: TInterfaceList;
   layerName, pairNet1, pairNet2, resultTrk1, resultTrk2, resultMsg: String;
   StartTime, EndTime, DeltaTime: TDateTime;
   smart_place: Boolean;
   DiffPair: IPCB_DifferentialPair;
   DiffRule: IPCB_Rule;
begin
   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then exit;

   RuleList := GetDiffPairRuleList(Board);

   StartTime := GetTime();

   if SmartPlace = nil then
   begin
       smart_place := SMART_PLACE_DEFAULT;
   end
   else
   begin
       smart_place := SmartPlace;
   end;

   ResultList := TStringList.Create;
   resultMsg := '';

   NetList := TStringList.Create;
   NetList := GetSelectedNetList(Board);

   if NetList.Count = 0 then
   begin
       ShowMessage('No tracks selected. Please select differential pair tracks before running.');
       exit;
   end
   else if NetList.Count = 1 then
   begin
       ShowMessage('Only 1 track selected. Please select differential pair tracks before running.');
       exit;
   end;

   // Progress Bar
   if RunningGUI = True then
   begin
       ProgressBar1.Position := 1;
       ProgressBar1.Update;
       ProgressBar1.Max := Int(NetList.Count/2)+1;
   end;

   DiffList := GetDiffPairList(NetList);

   SelectAllTracks();

   AllTracksList := GetSelectedTrackList(''); // Pass empty string to get all nets

   // Iterate Nets
   while NetList.Count > 0 do
   begin
       pairNet1 := NetList.Get(0);
       DiffPair := GetDiffPair(pairNet1, DiffList, pairNet2);
       DiffList.Remove(DiffPair);

       LayerList := GetLayersFromTrackList(AllTracksList, pairNet1);
       
       // Iterate Layers
       for layer_n := 0 to LayerList.Count - 1 do
       begin
           layerName := LayerList.Get(layer_n);

           // Get track lists for given layer & net for each of the diff pair
           TrackList1 := FilterTrackList(AllTracksList, pairNet1, layerName);
           if pairNet2 = '' then
           begin
               pairNet2 := GetPairNet(TrackList1[0], AllTracksList); // Don't execute for every layer
           end;

           TrackList2 := FilterTrackList(AllTracksList, pairNet2, layerName);

           // SORT Tracks in track lists
           TrackList1 := SortTrackList(TrackList1);
           TrackList2 := SortTrackList(TrackList2);

           // If the last track is closer than the first track, reverse order
           if Board.PrimPrimDistance(TrackList1[0], TrackList2[0]) > Board.PrimPrimDistance(TrackList1[0], TrackList2[TrackList2.Count-1]) then
           begin
              TrackList2 := CopyList(TrackList2, True); // Copy in reverse order
           end;

           //gap := GetDiffPairGap(TrackList1, TrackList2);
           DiffRule := GetDiffPairRuleForTrack(RuleList, TrackList1[0]);
           width := CoordToMils(DiffRule.PreferedWidth[TrackList1[0].Layer]);
           gap := CoordToMils(DiffRule.PreferedGap[TrackList1[0].Layer]);

           // Get overall bend on layer for diff pair
           trkBend := GetDiffPairBend(TrackList1, TrackList2, gap);
           if trkBend = -1 then
           begin
               ShowMessage('Error getting differential pair bend. Exiting.');
               exit;
           end;
           bumpsNeeded := Round(abs(trkBend)/45)*2; // Number of bumps to match length

           if bumpsNeeded > 0 then
           begin
           
               // Get shortest differential pair
               shortLen := GetTrackLength(TrackList1);
               ShortTrkList := CopyList(TrackList1, False);
               LongTrkList := CopyList(TrackList2, False);
               if GetTrackLength(TrackList2) < shortLen then
               begin
                   shortLen := GetTrackLength(TrackList2);
                   ShortTrkList := CopyList(TrackList2, False);
                   LongTrkList := CopyList(TrackList1, False);
               end;
               if shortLen = 0 then exit;

               //width := CoordToMils(TrackList1[0].Width);
               CalculateBump(width, gap, side_len, run_len);
               flat_len := 2*run_len + 2*(side_len/sqrt(2));

               if smart_place then
               begin
                   ShortTrkList := SortTracksByLengthOffset(ShortTrkList, LongTrkList);
               end;

               PCBServer.PreProcess;

               bumpsAdded := 0;
               i := 0;
               while (ShortTrkList.Count > 0) and (bumpsAdded < bumpsNeeded) do
               begin
                   Track := ShortTrkList[i];
                   trkLen := CoordToMils(Track.GetState_Length());

                   bmpChainCnt := 0;
                   while CoordToMils(Track.GetState_Length()) > flat_len+run_len do
                   begin
                       if AddBumpToTrack(LongTrkList, gap, Track) then
                       begin
                           Inc(bmpChainCnt);
                           Inc(bumpsAdded);
                       end;
                       if (bumpsAdded >= bumpsNeeded) or (bmpChainCnt >= BUMPS_PER_45_DEG) then break;
                   end;

                   if (CoordToMils(Track.GetState_Length()) <= flat_len+run_len) then ShortTrkList.Remove(Track);

                   Inc(i);
                   if i >= ShortTrkList.Count then i:=0;
               end;

               PCBServer.PostProcess;

               // Get new lengths
               if REPORT_LENGTHS then
               begin
                   TrackList1 := UpdateTrackList(pairNet1, layerName);
                   TrackList2 := UpdateTrackList(pairNet2, layerName);
                   trkLen := GetTrackLength(TrackList1);
                   trkLen2 := GetTrackLength(TrackList2);
                   resultTrk1 := pairNet1+': '+FloatToStr(trkLen);
                   resultTrk2 := pairNet2+': '+FloatToStr(trkLen2);

                   ResultList.Add(layerName+' - '+resultTrk1+', '+resultTrk2+', Delta: '+FloatToStr(Round(abs(trkLen - trkLen2))));
               end
               else
               begin
                   ResultList.Add(pairNet1+';'+pairNet2+';'+layerName);
               end;
               
               ShortTrkList.Clear; LongTrkList.Clear;
           end; // End if bumpsNeeded > 0

           TrackList1.Clear; TrackList2.Clear;
        end; // End For Loop
        
        NetList.Delete(NetList.IndexOf(pairNet1));
        NetList.Delete(NetList.IndexOf(pairNet2));

        if RunningGUI then
        begin
            ProgressBar1.Position := ProgressBar1.Position + 1;
            ProgressBar1.Update;
        end;
        
   end; // End While

   Board.ViewManager_FullUpdate;

   EndTime := GetTime();
   DeltaTime := abs(EndTime - StartTime)*100000;

   // Create Show Message String
   if REPORT_LENGTHS then
   begin
       for i:=0 to ResultList.Count-1 do
       begin
           resultMsg := resultMsg + ResultList[i] + NEWLINECODE;
       end;
       ShowMessage(resultMsg);
   end;

   if not RunningGUI then ShowMessage('Script Complete.');
end;

Procedure RunGUI;
Begin
    // GUI Init
    btnGetReport.Visible := False;
    MemoReport.Enabled := False;
    MemoReport.Text := '';
    RunningGUI := True;

    // Open Form
    FormAddBumps.ShowModal;
    btnGetReport.Visible := True;
End;

procedure TFormAddBumps.btnRunClick(Sender: TObject);
var
    txtInt: Integer;
begin
    SmartPlace := cbSmartPlace.Checked;
    btnRun.Enabled := False;
    RunNoGUI;

    btnGetReport.Visible := True;
end;

procedure TFormAddBumps.btnGetReportClick(Sender: TObject);
const
    NEWLINECODE = #13#10;
    TABCODE = #9;
var
    i: Integer;
    ParseResult: TStringList;
    Net1, Net2, LayerName, ReportStr: String;
    TrackList1, TrackList2: TInterfaceList;
    trkLen1, trkLen2, delta: Double;
begin
   btnGetReport.Enabled := False;
   MemoReport.Enabled := True;
   TrackList1 := TInterfaceList.Create; TrackList2 := TInterfaceList.Create;

   ParseResult := TStringList.Create;
   ParseResult.Delimiter := ';';
   ParseResult.StrictDelimiter := True; // Otherwise it splits on spaces

   ProgressBar1.Position := 1;
   ProgressBar1.Update;
   ProgressBar1.Max := ResultList.Count + 1;

   for i:=0 to ResultList.Count-1 do
   begin
       ParseResult.DelimitedText := ResultList.Get(i);
       Net1 := ParseResult.Get(0);
       Net2 := ParseResult.Get(1);
       LayerName := ParseResult.Get(2);

       TrackList1 := UpdateTrackList(Net1, LayerName);
       TrackList2 := UpdateTrackList(Net2, LayerName);
       trkLen1 := GetTrackLength(TrackList1);
       trkLen2 := GetTrackLength(TrackList2);
       delta := Round(abs(trkLen2-trkLen1));

       if delta = 0 then
       begin
           ReportStr := ReportStr+LayerName+': ('+Net1+'/'+Net2+') MATCH'+NEWLINECODE;
       end
       else
       begin
           ReportStr := ReportStr+LayerName+':    Delta='+FloatToStr(delta)+NEWLINECODE;
           ReportStr := ReportStr+TABCODE+Net1+' = '+FloatToStr(trkLen1)+NEWLINECODE;
           ReportStr := ReportStr+TABCODE+Net2+' = '+FloatToStr(trkLen2)+NEWLINECODE;
           ReportStr := ReportStr+NEWLINECODE;
       end;

       ProgressBar1.Position := ProgressBar1.Position + 1;
       ProgressBar1.Update;
   end;
   MemoReport.Text := ReportStr;
end;

