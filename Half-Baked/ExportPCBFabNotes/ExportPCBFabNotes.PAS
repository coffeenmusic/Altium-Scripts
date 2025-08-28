{..............................................................................}
{ Summary Iterates Nets of the current PCB document.                            }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}
Procedure Run;
Var
    Board       : IPCB_Board;
    Text         : IPCB_Text;
    Iterator    : IPCB_BoardIterator;
    Identifier : String;
    X, Y : Float;
    Size, Width: Integer;
Begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    // Create the iterator that will look for Net objects only
    Iterator        := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eTextObject));
    Iterator.AddFilter_LayerSet(MkSet(eMechanical2));
    Iterator.AddFilter_Method(eProcessAll);

    // Search for Net objects and get their Net Name values
    Text := Iterator.FirstPCBObject;
    While (Text <> Nil) Do
    Begin

        If Not(Text.InComponent) And (Text.IsFreePrimitive) Then
        Begin
            X := CoordToMils(Text.XLocation - Board.XOrigin);
            Y := CoordToMils(Text.YLocation - Board.YOrigin);
        End;        

        Text := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);
 End;
{..............................................................................}

{..............................................................................}

