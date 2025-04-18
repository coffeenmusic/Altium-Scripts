Function CheckNoNets(ObjSet: TObjSet, LayerSet: TLayerSet);
Var
    Board         : IPCB_Board;
    Iterator      : IPCB_BoardIterator;
    PCBObject     : IPCB_Primitive;
    LayerName     : String;
Begin
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then
    Begin
        ShowMessage('No PCB document loaded.');
        Exit;
    End;

    Iterator := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(ObjSet);
    Iterator.AddFilter_LayerSet(LayerSet);
    Iterator.AddFilter_Method(eProcessAll);

    PCBObject := Iterator.FirstPCBObject;
    While PCBObject <> Nil Do
    Begin
        If PCBObject.Net = Nil Then
        Begin
            LayerName := Layer2String(PCBObject.Layer);
            //ShowMessage(Format('No Net object found: %s on layer %s', [PCBObject.ObjectIdString, LayerName]));
            PCBObject.Selected := True; // Select the object in PCB editor
        End;
        PCBObject := Iterator.NextPCBObject;
    End;

    Board.BoardIterator_Destroy(Iterator);
    Board.ViewManager_FullUpdate;
End;

Procedure CheckNoNetObjects;
Begin
    CheckNoNets(MkSet(eTrackObject, eArcObject, ePolyObject), MkSetRange(eTopLayer, eBottomLayer));
    CheckNoNets(MkSet(eViaObject), MkSet(eMultiLayer));
End;
