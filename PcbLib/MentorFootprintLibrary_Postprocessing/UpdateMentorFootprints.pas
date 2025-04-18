function MoveObjectsOnLayerToLayer(Footprint: IPCB_LibComponent, OnLayer: Integer, ToLayer: Integer): Integer;
Var
    Iterator          : IPCB_GroupIterator;
    AObject           : IPCB_Primitive;
    moveCnt, objID: Integer;
Begin
    Iterator := Footprint.GroupIterator_Create;
    Iterator.AddFilter_LayerSet(MkSet(OnLayer));
    Iterator.SetState_FilterAll;

    moveCnt := 0;
    AObject := Iterator.FirstPCBObject;
    While (AObject <> Nil) Do
    Begin
         If AObject.Layer = OnLayer Then
         Begin
              PCBServer.SendMessageToRobots(AObject.I_ObjectAddress, c_Broadcast, PCBM_BeginModify, c_NoEventData);
              objID := AObject.ObjectId;
              AObject.Layer := ToLayer;
              PCBServer.SendMessageToRobots(AObject.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_NoEventData);

              Inc(moveCnt);
         End;

        AObject := Iterator.NextPCBObject;
    End;
    Footprint.GroupIterator_Destroy(Iterator);

    result := moveCnt;
End;

function MoveObjectsOnLayerToLayer2(Footprint: IPCB_LibComponent, OnLayer: Integer, ToLayer: Integer): Integer;
Var
    Iterator          : IPCB_BoardIterator;
    AObject           : IPCB_Primitive;
    moveCnt, objID: Integer;
Begin
    Iterator := Footprint.Board.BoardIterator_Create;
    //Iterator.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject));
    Iterator.AddFilter_LayerSet(MkSet(OnLayer));
    Iterator.AddFilter_Method(eProcessAll);

    moveCnt := 0;
    AObject := Iterator.FirstPCBObject;
    While AObject <> nil Do
    Begin
        PCBServer.SendMessageToRobots(AObject.I_ObjectAddress, c_Broadcast, PCBM_BeginModify, c_NoEventData);
        objID := AObject.ObjectId;
        AObject.Layer := ToLayer;
        PCBServer.SendMessageToRobots(AObject.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_NoEventData);

        Inc(moveCnt);

        AObject := Iterator.NextPCBObject;
    End;
    Footprint.Board.BoardIterator_Destroy(Iterator);
    Footprint.Board.ViewManager_FullUpdate;

    result := moveCnt;
End;

function MoveObjectsToLayer(Footprint: IPCB_LibComponent, ObjList: TInterfaceList, ToLayer: Integer): Integer;
Var
    i: Integer;
    Obj: IPCB_Primitive;
    moveCnt: Integer;
Begin

    moveCnt := 0;
    for i:=0 to ObjList.Count-1 do
    begin
        Obj := ObjList[i];

        PCBServer.SendMessageToRobots(Obj.I_ObjectAddress, c_Broadcast, PCBM_BeginModify, c_NoEventData);
        Obj.Layer := ToLayer;
        PCBServer.SendMessageToRobots(Obj.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_NoEventData);

        Inc(moveCnt);
    end;
    result := moveCnt;
End;


function GetObjectsOnLayer(Footprint: IPCB_LibComponent, Layer: Integer):TInterfaceList;
var
    Iterator: IPCB_BoardIterator;
    ObjList: TInterfaceList;
    i: Integer;
    Obj: IPCB_Primitive;
begin
    ObjList := TInterfaceList.Create;

    Iterator := Footprint.Board.BoardIterator_Create;
    Iterator.AddFilter_LayerSet(MkSet(Layer));
    Iterator.AddFilter_Method(eProcessAll);

    Obj := Iterator.FirstPCBObject;
    While Obj <> nil Do
    Begin
        ObjList.Add(Obj);

        Obj := Iterator.NextPCBObject;
    End;
    result := ObjList;
end;

function GetObjects(Footprint: IPCB_LibComponent);
var
    Iterator: IPCB_BoardIterator;
    i: Integer;
    Obj: IPCB_Primitive;
    LayerName: String;
    Layer: Integer;
begin
    Iterator := Footprint.Board.BoardIterator_Create;
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    Obj := Iterator.FirstPCBObject;
    While Obj <> nil Do
    Begin
        Layer := Obj.Layer;
        LayerName := Layer2String(Layer);

        Obj := Iterator.NextPCBObject;
    End;
end;

function EnableLayer(Footprint: IPCB_LibComponent, Layer: Integer, Enable: Boolean);
var
    MechLayer: IPCB_MechanicalLayer;
begin
    MechLayer := Footprint.Board.LayerStack.LayerObject[Layer];
    if (not Enable) and (MechLayer.UsedByPrims) then exit; // Layer still has objects on it
    MechLayer.MechanicalLayerEnabled := Enable;
end;

function EnableLayer2(Footprint: IPCB_LibComponent, Layer: Integer, Enable: Boolean);
var
    MechLayer: IPCB_MechanicalLayer;
begin
    MechLayer := Footprint.Board.LayerStack_V7.LayerObject_V7[Layer];
    MechLayer.MechanicalLayerEnabled := Enable;
end;

function RenameLayer(Footprint: IPCB_LibComponent, Layer: Integer, Name: String);
var
    MechLayer: IPCB_MechanicalLayer;
begin
    MechLayer := Footprint.Board.LayerStack.LayerObject[Layer];
    MechLayer.Name := Name;
end;

function MoveObjectsFromMechLayer(Board: IPCB_Board, MechLayerNumber: Integer, ToLayer: Integer);
var
    i : Integer;
begin
   Board.CurrentLayer := ILayer.MechanicalLayer(MechLayerNumber);
   Client.SendMessage('PCB:Select', 'Scope = Layer' , 255, Client.CurrentView); // Select All On Layer

   if Board.SelectecObjectCount = 0 then exit;

   i:= Board.SelectecObjectCount;
   for i := 0 to Board.SelectecObjectCount - 1 do
   begin
       if ILayer.MechanicalLayer(MechLayerNumber) = Board.SelectecObject[i].Layer then
       begin
           Board.SelectecObject[i].Layer := ToLayer;
       end;
   end;

   Client.SendMessage('PCB:DeSelect', 'Scope=All', 255, Client.CurrentView);
end;

function GetDesignatorCoords(Footprint: IPCB_LibComponent, var x: Integer, var y: Integer, var width: Integer, var height: Integer);
var
    i: Integer;
    ObjList: TInterfaceList;
    minX, minY, maxX, maxY: Integer;
    firstFlag: Boolean;
    Obj: IPCB_Primitive;
begin
    firstFlag := True;
    ObjList := GetObjectsOnLayer(Footprint, eMechanical2);
    for i:=0 to ObjList.Count-1 do
    begin
        if ObjList[i].ObjectId = eTrackObject then
        begin
            Obj := ObjList[i];

            // On first execution
            if (firstFlag) then
            begin
                if Obj.x1 < Obj.x2 then
                begin
                    minX := Obj.x1; maxX := Obj.x2;
                end
                else
                begin
                    minX := Obj.x2; maxX := Obj.x1;
                end;
                if Obj.y1 < Obj.y2 then
                begin
                    minY := Obj.y1; maxY := Obj.y2;
                end
                else
                begin
                    minY := Obj.y2; maxY := Obj.y1;
                end;

                firstFlag := False;
            end
            else
            begin
                if Obj.x1 < minX then minX := Obj.x1;
                if Obj.x2 < minX then minX := Obj.x2;
                if Obj.x1 > maxX then maxX := Obj.x1;
                if Obj.x2 > maxX then maxX := Obj.x2;
                if Obj.y1 < minY then minY := Obj.y1;
                if Obj.y2 < minY then minY := Obj.y2;
                if Obj.y1 > maxY then maxY := Obj.y1;
                if Obj.y2 > maxY then maxY := Obj.y2;
            end;
        end;
    end;

    width := maxX - minX;
    height := maxY - minY;
    x := Int(width/2) + minX;
    y := Int(height/2) + minY;
end;

function AddTopAssemblyDesignator(Footprint: IPCB_LibComponent);
var
    Board: IPCB_Board;
    TextObj: IPCB_Text;
    Rec: TCoordRect;
    x, y, w, h, size, width: Integer;
    xOff, yOff: Integer;
    xmil, ymil, wmil, hmil: Double;
    xOffMil, yOffMil: Double;
begin
    Board := Footprint.Board;

    TextObj := PCBServer.PCBObjectFactory(eTextObject, eNoDimension, eCreate_Default);

    PCBServer.SendMessageToRobots(TextObj.I_ObjectAddress, c_Broadcast, PCBM_BeginModify, c_NoEventData);

    TextObj.Layer     := eMechanical2;
    TextObj.Text      := '.Designator';

    x := 0; y := 0; w := 0; h := 0;
    GetDesignatorCoords(Footprint, x, y, w, h);
    size := Int(h/2);
    if h > w then
    begin
        size := Int(w/2);
        TextObj.Rotation := 90;
    end;

    width := 2*(size/10);
    TextObj.Size := size;
    TextObj.Width := 2*(TextObj.Size/10);

    Board.AddPCBObject(TextObj);

    TextObj.SetState_Multiline(True);
    TextObj.MultilineTextAutoPosition := eAutoPos_CenterCenter;

    xOff := 0; yOff := 0;
    Rec := TextObj.BoundingRectangle;
    xOff := Int((Rec.Right - Rec.Left)/2);
    yOff := Int((Rec.Top - Rec.Bottom)/2);
    if h > w then xOff := xOff*-1;
    xOffMil := CoordToMils(Rec.Right - Rec.Left);
    yOffMil := CoordToMils(Rec.Top - Rec.Bottom);

    xmil := CoordToMils(x);
    ymil := CoordToMils(y);
    wmil := CoordToMils(w);
    hmil := CoordToMils(h);
    TextObj.XLocation := x - xOff; // + Board.XOrigin
    TextObj.YLocation := y - yOff; // + Board.YOrigin

    TextObj.SetState_XSizeYSize;
    TextObj.GraphicallyInvalidate;

    PCBServer.SendMessageToRobots(TextObj.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_NoEventData);
    PCBServer.SendMessageToRobots(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, TextObj.I_ObjectAddress);

    Board.ViewManager_FullUpdate;

end;

Procedure Run;
Var
    CurrentLib        : IPCB_Library;
    FootprintIterator : IPCB_LibraryIterator;
    Footprint         : IPCB_LibComponent;
    FirstTime, InStack         : Boolean;
    i, NoOfPrims         : Integer;
    Layer, Pair1, Pair2 : TV6_Layer;
    MechLayerPairs: IPCB_MechanicalLayerPairs;
    MechLayer: IPCB_MechanicalLayer;
    LayerName, FootName: String;
    moveCount: Integer;
    Mech15List, Mech20List, Mech21List: TInterfaceList;
    MechIter: IPCB_LayerObjectIterator;
    LayerObj: IPCB_LayerObject;
    eMechanical20, eMechanical21: Integer;
Begin
    CurrentLib := PCBServer.GetCurrentPCBLibrary;
    If CurrentLib = Nil Then
    Begin
        ShowMessage('This is not a PCB Library document');
        Exit;
    End;

    // For each page of library is a footprint
    FootprintIterator := CurrentLib.LibraryIterator_Create;
    FootprintIterator.SetState_FilterAll;

    Try
        // Within each page, fetch primitives of the footprint
        // A footprint is a IPCB_LibComponent inherited from
        // IPCB_Group which is a container object that stores primitives.
        Footprint := FootprintIterator.FirstPCBObject;
        While Footprint <> Nil Do
        Begin
           FootName := Footprint.Name;
           CurrentLib.SetBoardToComponentByName(FootName);

           Footprint.Board.ViewManager_FullUpdate;

           PCBServer.PreProcess;
           Footprint.BeginModify;

           Mech15List := GetObjectsOnLayer(Footprint, eMechanical15);

           MechLayerPairs := Footprint.Board.MechanicalPairs;
           // Remove 1/2 Pair, Should be 2/3 Pair
           if MechLayerPairs.PairDefined(eMechanical1, eMechanical2) then
           begin
               MechLayerPairs.RemovePair(eMechanical1, eMechanical2);
           end;

           // Disable Mechanical 1
           EnableLayer(Footprint, eMechanical1, False);

           // Enable Mechanical 2 & Rename Layer
           EnableLayer(Footprint, eMechanical2, True);
           RenameLayer(Footprint, eMechanical2, 'Top Assembly');

           // Enable Mechanical 3 & Rename Layer
           EnableLayer(Footprint, eMechanical3, True);
           RenameLayer(Footprint, eMechanical3, 'Bottom Assembly');

           // Make 2&3 an Assembly Layer Pair
           if not MechLayerPairs.PairDefined(eMechanical2, eMechanical3) then MechLayerPairs.AddPair(eMechanical2, eMechanical3);

           // Move M15 to Assembly Top and Rename M15-->Courtyard
           MoveObjectsToLayer(Footprint, Mech15List, eMechanical2);
           RenameLayer(Footprint, eMechanical15, 'Courtyard');

           // Move M20 to Courtyard & remove M20
           MoveObjectsFromMechLayer(Footprint.Board, 20, eMechanical15);
           EnableLayer2(Footprint, ILayer.MechanicalLayer(20), False);


           // Enable & Rename M13
           EnableLayer(Footprint, eMechanical13, True);
           RenameLayer(Footprint, eMechanical13, 'Component Outline');

           // Move M21 to Component Outline
           MoveObjectsFromMechLayer(Footprint.Board, 21, eMechanical13);
           EnableLayer2(Footprint, ILayer.MechanicalLayer(21), False);

           // Disable M94 & M111
           MoveObjectsFromMechLayer(Footprint.Board, 94, eMechanical2);
           MoveObjectsFromMechLayer(Footprint.Board, 111, eMechanical2);
           EnableLayer2(Footprint, ILayer.MechanicalLayer(94), False);
           EnableLayer2(Footprint, ILayer.MechanicalLayer(111), False);

           // Add Designator to Assembly Top
           Footprint.Board.CurrentLayer := eMechanical2;
           AddTopAssemblyDesignator(Footprint);

           Footprint.EndModify;
           PCBServer.PostProcess;

           Footprint.Board.ViewManager_UpdateLayerTabs;

           Footprint := FootprintIterator.NextPCBObject;
        End;
    Finally
        CurrentLib.LibraryIterator_Destroy(FootprintIterator);
    End;


End;
{..............................................................................}

{..............................................................................}
