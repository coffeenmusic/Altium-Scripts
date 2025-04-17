{..............................................................................}
{ Net Current Capacity Analyzer and Colorizer                                 }
{                                                                             }
{ Analyzes all nets on a PCB and colors them based on their                   }
{ worst-case current capacities across complete signal paths                  }
{..............................................................................}

const
    constScriptProjectName = 'CurrentColorizer';  // Set this to your actual script project name

// Forward declarations to resolve circular references
Procedure FindConnectedObjects(Board : IPCB_Board; Net : IPCB_Net; X, Y : Integer;
                              Layer : TLayer; Width : TCoord; ProcessedTracks : TObjectList;
                              ProcessedVias : TObjectList; CurrentPath : TObjectList;
                              Tolerance : TCoord); Forward;

Procedure FindConnectedLayerTracks(Board : IPCB_Board; Net : IPCB_Net; X, Y : Integer;
                                 Layer : TLayer; Width : TCoord; ProcessedTracks : TObjectList;
                                 ProcessedVias : TObjectList; CurrentPath : TObjectList;
                                 Tolerance : TCoord); Forward;

// Power function from the original script
Function pow(base, exponent: double): double;
Begin
    result := Exp(Exponent*Ln(Base));
    if (base = 0) then result := 0;
End;

// Custom minimum function for doubles
Function MinDouble(Value1, Value2: Double): Double;
Begin
    if (Value1 < Value2) then
        Result := Value1
    else
        Result := Value2;
End;

{..............................................................................}
{ Get path of this script project.                                             }
{..............................................................................}
function ScriptProjectPath(Workspace: IWorkspace) : String;
var
  Project   : IProject;
  scriptsPath : TDynamicString;
  projectCount : Integer;
  i      : Integer;
begin
  if (Workspace = nil) then begin result:=''; exit; end;

  { Get a count of the number of currently opened projects.  The script project
    from which this script runs must be one of these. }
  projectCount := Workspace.DM_ProjectCount();

  { Loop over all the open projects.  We're looking for constScriptProjectName
    (of which we are a part).  Once we find this, we want to record the
    path to the script project directory. }
  scriptsPath:='';
  for i:=0 to projectCount-1 do
  begin
    { Get reference to project # i. }
    Project := Workspace.DM_Projects(i);
    { See if we found our script project. }
    if (AnsiPos(constScriptProjectName, Project.DM_ProjectFullPath) > 0) then
    begin
      { Strip off project name to give us just the path. }
      scriptsPath := StringReplace(Project.DM_ProjectFullPath, '\' +
      constScriptProjectName + '.PrjScr','', MkSet(rfReplaceAll,rfIgnoreCase));
    end;
  end;
  result := scriptsPath;
end;

// Function to save original net colors to a CSV file
Procedure SaveOriginalNetColors(NetList : TObjectList; FilePath : String);
Var
    ResultsList : TStringList;
    i : Integer;
    CurrentNet : IPCB_Net;
Begin
    // Create string list for results
    ResultsList := TStringList.Create;

    Try
        // Add CSV header
        ResultsList.Add('Net Name,Original Color');

        // Add each net and its original color
        For i := 0 to NetList.Count - 1 Do
        Begin
            CurrentNet := NetList[i];
            // Add net name and color to the list
            ResultsList.Add(CurrentNet.Name + ',' + IntToStr(CurrentNet.Color));
        End;

        // Save to file
        ResultsList.SaveToFile(FilePath);
    Finally
        // Free string list
        ResultsList.Free;
    End;
End;

// Procedure to restore original net colors from a CSV file
Procedure RestoreNetColors;
Const
    DEFAULT_FILE = 'OriginalNetColors.csv';
Var
    Board : IPCB_Board;
    NetList : TObjectList;
    NetIterator : IPCB_BoardIterator;
    CurrentNet : IPCB_Net;
    i : Integer;
    ColorMap, SavedColors : TStringList;
    RowData : TStringList;
    FilePath, ScriptPath : String;
    row_idx : Integer;
    NetName : String;
    NetColor : Integer;
Begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If (Board = Nil) Then
    begin
        ShowMessage('No board found');
        Exit;
    end;

    // Get script path
    ScriptPath := ScriptProjectPath(GetWorkspace);
    if ScriptPath = '' then
        ScriptPath := '.'; // Use current directory if script path not found

    // Set default file path to script project path
    FilePath := ScriptPath + '\' + DEFAULT_FILE;

    // Create color map for lookup
    SavedColors := TStringList.Create;

    // Create color map for lookup
    ColorMap := TStringList.Create;
    ColorMap.NameValueSeparator := '=';

    // Create row data parser
    RowData := TStringList.Create;
    RowData.Delimiter := ',';
    RowData.StrictDelimiter := True;

    // Create list to store unique nets
    NetList := CreateObject(TObjectList);
    NetList.OwnsObjects := False; // Don't destroy nets when list is freed

    Try
        // Load CSV file
        SavedColors.LoadFromFile(FilePath);

        // Skip header row
        if SavedColors.Count > 0 then
        begin
            // Create a fast lookup dictionary
            for row_idx := 1 to SavedColors.Count-1 do // Start from 1 to skip header
            begin
                RowData.DelimitedText := SavedColors.Get(row_idx);
                if RowData.Count >= 2 then
                begin
                    NetName := Trim(RowData.Get(0));
                    ColorMap.Values[NetName] := Trim(RowData.Get(1));
                end;
            end;
        end;

        // Create iterator for net objects
        NetIterator := Board.BoardIterator_Create;
        NetIterator.AddFilter_ObjectSet(MkSet(eNetObject));
        NetIterator.AddFilter_LayerSet(AllLayers);
        NetIterator.AddFilter_Method(eProcessAll);

        // First pass: collect all unique nets
        CurrentNet := NetIterator.FirstPCBObject;
        While (CurrentNet <> Nil) Do
        Begin
            NetList.Add(CurrentNet);
            CurrentNet := NetIterator.NextPCBObject;
        End;

        Board.BoardIterator_Destroy(NetIterator);

        // Start modifying colors
        PCBServer.PreProcess;

        // For each net, restore its original color if found in the map
        For i := 0 to NetList.Count - 1 Do
        Begin
            CurrentNet := NetList[i];

            // Check if this net's name is in our color map
            if ColorMap.IndexOfName(CurrentNet.Name) >= 0 then
            begin
                // Get the original color
                NetColor := StrToIntDef(ColorMap.Values[CurrentNet.Name], CurrentNet.Color);

                // Restore original color
                CurrentNet.Color := NetColor;
                CurrentNet.OverrideColorForDraw := False;

                Board.ViewManager_GraphicallyInvalidatePrimitive(CurrentNet);
            end;
        End;

        PCBServer.PostProcess;

        // Refresh display
        Client.SendMessage('PCB:Zoom', 'Action=Redraw', 255, Client.CurrentView);

        ShowMessage('Original net colors restored');
    Finally
        // Free string lists
        ColorMap.Free;
        RowData.Free;
    End;
End;

// Helper function to calculate current capacity for a track path
Function CalculatePathCurrentCapacity(Tracks : TObjectList; Board : IPCB_Board; LStack : IPCB_LayerStack; TempRiseC : Double) : Double;
Var
    h : Double; // Layer Thickness in mils
    w : Double; // Track Width in mils
    k, c, b : Double; // IPC-2221 Constants
    z : Double; // Area
    L1 : IPCB_LayerObject;
    isMidLayer : Boolean;
    I_temp : Double; // Current for specified temperature rise
    tk : Double; // Temperature rise constant
    TotalLength : Double; // Total path length in mils
    Track : IPCB_Primitive;
    TrackLength : Double;
    i : Integer;
    CurrentLayer : TLayer;
    CurrentWidth : TCoord;
Begin
    Result := 0; // Initialize with 0

    // No tracks in path
    if (Tracks = nil) or (Tracks.Count = 0) then Exit;

    // Get the first track to determine width and layer
    Track := Tracks[0];

    // Skip if not a track or arc
    if not ((Track.ObjectID = eTrackObject) or (Track.ObjectID = eArcObject)) then Exit;

    // Get layer information
    L1 := LStack.LayerObject(Track.Layer);
    if (L1 = nil) then Exit; // Skip if layer not found

    // Get Layer Thickness in mils
    h := coordToMils(L1.CopperThickness);

    // Determine width based on object type of first track
    Case Track.ObjectID of
        eTrackObject:
            w := coordToMils(Track.Width);
        eArcObject:
            w := coordToMils(Track.LineWidth);
        else
            Exit; // Exit with result = 0
    End;

    // Skip if width or thickness is zero
    if ((w <= 0) or (h <= 0)) then Exit;

    // We need to separate calculations by layer and width
    // Group tracks by layer/width and calculate capacity for each group
    TotalLength := 0;
    CurrentWidth := 0;
    CurrentLayer := 0;

    for i := 0 to Tracks.Count - 1 do
    begin
        Track := Tracks[i];

        // Only process tracks and arcs
        if ((Track.ObjectID = eTrackObject) or (Track.ObjectID = eArcObject)) then
        begin
            // Calculate length based on object type
            Case Track.ObjectID of
                eTrackObject:
                begin
                    if ((CurrentWidth <> Track.Width) or (CurrentLayer <> Track.Layer)) then
                    begin
                        CurrentWidth := Track.Width;
                        CurrentLayer := Track.Layer;
                    end;

                    TrackLength := coordToMils(sqrt(sqr(Track.x2 - Track.x1) + sqr(Track.y2 - Track.Y1)));
                end;
                eArcObject:
                begin
                    if ((CurrentWidth <> Track.LineWidth) or (CurrentLayer <> Track.Layer)) then
                    begin
                        CurrentWidth := Track.LineWidth;
                        CurrentLayer := Track.Layer;
                    end;

                    TrackLength := coordToMils(((Track.StartAngle - Track.EndAngle)/360) * pi * Track.Radius);
                end;
                else
                    TrackLength := 0;
            End;

            TotalLength := TotalLength + TrackLength;
        end;
    end;

    // Assign IPC-2221 constants
    c := 0.725;
    b := 0.44;

    // Check if on middle layer (using the same logic as the original script)
    isMidLayer := ((Track.Layer <> 1) and (Track.Layer <> 32));

    // Assign k value based on layer location
    if (isMidLayer) then
        k := 0.024 // Internal layer
    else
        k := 0.048; // External layer

    // Calculate temperature constant for the specified temperature rise (using the original pow function)
    tk := k * pow(TempRiseC, b);

    // Calculate cross-sectional area
    z := pow((h * w), c);

    // Calculate current for the specified temperature rise
    I_temp := z * tk;

    Result := I_temp;
End;

// Helper function to trace a complete path of connected tracks
Function TraceCompletePath(Board : IPCB_Board; StartTrack : IPCB_Primitive;
                          ProcessedTracks : TObjectList; ProcessedVias : TObjectList) : TObjectList;
Var
    CurrentPath : TObjectList;
    X1, Y1, X2, Y2 : Integer;
    Width : TCoord;
    Layer : TLayer;
    Tolerance : TCoord;
Begin
    // Create object list for the current path
    CurrentPath := CreateObject(TObjectList);
    CurrentPath.OwnsObjects := False; // Don't destroy track objects

    // Only process tracks and arcs
    if not ((StartTrack.ObjectID = eTrackObject) or (StartTrack.ObjectID = eArcObject)) then
    begin
        Result := CurrentPath;
        Exit;
    end;

    // Add starting track to path and processed list
    CurrentPath.Add(StartTrack);
    ProcessedTracks.Add(StartTrack);

    // Get track properties
    Layer := StartTrack.Layer;

    Case StartTrack.ObjectID of
        eTrackObject:
        begin
            Width := StartTrack.Width;
            X1 := StartTrack.x1;
            Y1 := StartTrack.y1;
            X2 := StartTrack.x2;
            Y2 := StartTrack.y2;
        end;
        eArcObject:
        begin
            Width := StartTrack.LineWidth;
            X1 := StartTrack.StartX;
            Y1 := StartTrack.StartY;
            X2 := StartTrack.EndX;
            Y2 := StartTrack.EndY;
        end;
        else
        begin
            // Not a track or arc
            Result := CurrentPath;
            Exit;
        end;
    End;

    // Set tolerance for connection detection (1 mil)
    Tolerance := MilsToCoord(1);

    // Trace in both directions from start track
    FindConnectedObjects(Board, StartTrack.Net, X1, Y1, Layer, Width,
                        ProcessedTracks, ProcessedVias, CurrentPath, Tolerance);
    FindConnectedObjects(Board, StartTrack.Net, X2, Y2, Layer, Width,
                        ProcessedTracks, ProcessedVias, CurrentPath, Tolerance);

    Result := CurrentPath;
End;

// Helper function to find connected tracks on a specific layer
Procedure FindConnectedLayerTracks(Board : IPCB_Board; Net : IPCB_Net; X, Y : Integer;
                                 Layer : TLayer; Width : TCoord; ProcessedTracks : TObjectList;
                                 ProcessedVias : TObjectList; CurrentPath : TObjectList;
                                 Tolerance : TCoord);
Var
    SIter : IPCB_SpatialIterator;
    Primitive : IPCB_Primitive;
    Track : IPCB_Primitive;
    ConnectedX, ConnectedY : Integer;
    IsConnected : Boolean;
    TrackWidth : TCoord;
Begin
    // Create spatial iterator to find nearby tracks on this layer
    SIter := Board.SpatialIterator_Create;
    SIter.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject));
    SIter.AddFilter_LayerSet(MkSet(Layer));
    SIter.AddFilter_Area(X - Tolerance, Y - Tolerance, X + Tolerance, Y + Tolerance);

    // Find all connected tracks
    Primitive := SIter.FirstPCBObject;
    While (Primitive <> nil) do
    Begin
        // Check if track is in the same net
        if ((Primitive.InNet) and (Primitive.Net.Name = Net.Name)) then
        begin
            // Only process tracks with the width we're looking for
            if (Primitive.ObjectID = eTrackObject) then
                TrackWidth := Primitive.Width
            else
                TrackWidth := Primitive.LineWidth;

            // Use relaxed width matching for tracks connected via vias
            if (TrackWidth > 0) then // Use the current track width
            begin
                // Check if this track is already processed
                if (ProcessedTracks.IndexOf(Primitive) = -1) then
                begin
                    Track := Primitive;
                    IsConnected := False;

                    Case Track.ObjectID of
                        eTrackObject:
                        begin
                            // Check if either endpoint connects
                            if ((Abs(Track.x1 - X) <= Tolerance) and (Abs(Track.y1 - Y) <= Tolerance)) then
                            begin
                                ConnectedX := Track.x2;
                                ConnectedY := Track.y2;
                                IsConnected := True;
                            end
                            else if ((Abs(Track.x2 - X) <= Tolerance) and (Abs(Track.y2 - Y) <= Tolerance)) then
                            begin
                                ConnectedX := Track.x1;
                                ConnectedY := Track.y1;
                                IsConnected := True;
                            end;
                        end;
                        eArcObject:
                        begin
                            // Check if either endpoint connects
                            if ((Abs(Track.StartX - X) <= Tolerance) and (Abs(Track.StartY - Y) <= Tolerance)) then
                            begin
                                ConnectedX := Track.EndX;
                                ConnectedY := Track.EndY;
                                IsConnected := True;
                            end
                            else if ((Abs(Track.EndX - X) <= Tolerance) and (Abs(Track.EndY - Y) <= Tolerance)) then
                            begin
                                ConnectedX := Track.StartX;
                                ConnectedY := Track.StartY;
                                IsConnected := True;
                            end;
                        end;
                    end;

                    // If connected, add to path and continue tracing
                    if (IsConnected) then
                    begin
                        // Add to processed list and current path
                        ProcessedTracks.Add(Track);
                        CurrentPath.Add(Track);

                        // Get the actual width of this track
                        if (Track.ObjectID = eTrackObject) then
                            Width := Track.Width
                        else
                            Width := Track.LineWidth;

                        // Recursively find next connected objects
                        FindConnectedObjects(Board, Net, ConnectedX, ConnectedY, Layer, Width,
                                           ProcessedTracks, ProcessedVias, CurrentPath, Tolerance);
                    end;
                end;
            end;
        end;

        Primitive := SIter.NextPCBObject;
    End;

    // Clean up
    Board.SpatialIterator_Destroy(SIter);
End;

// Helper function to find connected tracks and vias at a point
Procedure FindConnectedObjects(Board : IPCB_Board; Net : IPCB_Net; X, Y : Integer;
                              Layer : TLayer; Width : TCoord; ProcessedTracks : TObjectList;
                              ProcessedVias : TObjectList; CurrentPath : TObjectList;
                              Tolerance : TCoord);
Var
    SIter : IPCB_SpatialIterator;
    Primitive : IPCB_Primitive;
    Via : IPCB_Via;
    Track : IPCB_Primitive;
    ConnectedX, ConnectedY : Integer;
    IsConnected : Boolean;
    TrackWidth : TCoord;
    CurLayer : TLayer;
Begin
    // Create spatial iterator to find nearby objects
    SIter := Board.SpatialIterator_Create;
    SIter.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject, eViaObject));
    SIter.AddFilter_Area(X - Tolerance, Y - Tolerance, X + Tolerance, Y + Tolerance);

    // Find all connected objects
    Primitive := SIter.FirstPCBObject;
    While (Primitive <> nil) do
    Begin
        // Check if object is in the same net
        if ((Primitive.InNet) and (Primitive.Net.Name = Net.Name)) then
        begin
            Case Primitive.ObjectID of
                eTrackObject, eArcObject:
                begin
                    // Only process tracks with same width and same layer
                    if (Primitive.ObjectID = eTrackObject) then
                        TrackWidth := Primitive.Width
                    else
                        TrackWidth := Primitive.LineWidth;

                    if ((Primitive.Layer = Layer) and (TrackWidth = Width)) then
                    begin
                        // Check if this track is already processed
                        if (ProcessedTracks.IndexOf(Primitive) = -1) then
                        begin
                            Track := Primitive;
                            IsConnected := False;

                            Case Track.ObjectID of
                                eTrackObject:
                                begin
                                    // Check if either endpoint connects
                                    if ((Abs(Track.x1 - X) <= Tolerance) and (Abs(Track.y1 - Y) <= Tolerance)) then
                                    begin
                                        ConnectedX := Track.x2;
                                        ConnectedY := Track.y2;
                                        IsConnected := True;
                                    end
                                    else if ((Abs(Track.x2 - X) <= Tolerance) and (Abs(Track.y2 - Y) <= Tolerance)) then
                                    begin
                                        ConnectedX := Track.x1;
                                        ConnectedY := Track.y1;
                                        IsConnected := True;
                                    end;
                                end;
                                eArcObject:
                                begin
                                    // Check if either endpoint connects
                                    if ((Abs(Track.StartX - X) <= Tolerance) and (Abs(Track.StartY - Y) <= Tolerance)) then
                                    begin
                                        ConnectedX := Track.EndX;
                                        ConnectedY := Track.EndY;
                                        IsConnected := True;
                                    end
                                    else if ((Abs(Track.EndX - X) <= Tolerance) and (Abs(Track.EndY - Y) <= Tolerance)) then
                                    begin
                                        ConnectedX := Track.StartX;
                                        ConnectedY := Track.StartY;
                                        IsConnected := True;
                                    end;
                                end;
                            end;

                            // If connected, add to path and continue tracing
                            if (IsConnected) then
                            begin
                                // Add to processed list and current path
                                ProcessedTracks.Add(Track);
                                CurrentPath.Add(Track);

                                // Recursively find next connected objects
                                FindConnectedObjects(Board, Net, ConnectedX, ConnectedY, Layer, Width,
                                                   ProcessedTracks, ProcessedVias, CurrentPath, Tolerance);
                            end;
                        end;
                    end;
                end;
                eViaObject:
                begin
                    Via := Primitive;
                    // Check if via is already processed
                    if (ProcessedVias.IndexOf(Via) = -1) then
                    begin
                        // Check if via is connected at this point
                        if ((Abs(Via.x - X) <= Tolerance) and (Abs(Via.y - Y) <= Tolerance)) then
                        begin
                            if (Via.IntersectLayer(Layer)) then
                            begin
                                // Add via to processed list and current path
                                ProcessedVias.Add(Via);
                                CurrentPath.Add(Via);

                                // Find tracks on all other layers this via connects to
                                // Create a spatial iterator for each layer
                                for CurLayer := eTopLayer to eBottomLayer do
                                begin
                                    if ((Via.IntersectLayer(CurLayer)) and (CurLayer <> Layer)) then
                                    begin
                                        // Find tracks on this layer
                                        FindConnectedLayerTracks(Board, Net, Via.x, Via.y, CurLayer, Width,
                                                                ProcessedTracks, ProcessedVias, CurrentPath, Tolerance);
                                    end;
                                end;
                            end;
                        end;
                    end;
                end;
            end;
        end;

        Primitive := SIter.NextPCBObject;
    End;

    // Clean up
    Board.SpatialIterator_Destroy(SIter);
End;

// Helper function to get color based on capacity
Function GetColorForCapacity(CurrentCapacity, MinCapacity, MaxCapacity : Double) : Integer;
Var
    Ratio : Double;
    Red, Green, Blue : Integer;
Begin
    // Calculate ratio (0-1) where 0 is minimum capacity (worst) and 1 is maximum (best)
    Ratio := (CurrentCapacity - MinCapacity) / (MaxCapacity - MinCapacity);

    // Clamp between 0 and 1
    if (Ratio < 0) then Ratio := 0;
    if (Ratio > 1) then Ratio := 1;

    // Create color gradient: Red (worst) to Yellow to Green (best)
    if (Ratio < 0.5) then
    begin
        // Red to Yellow gradient (low capacity)
        Red := 255;
        Green := Round(255 * (Ratio * 2));
        Blue := 0;
    end
    else
    begin
        // Yellow to Green gradient (high capacity)
        Red := Round(255 * (1 - (Ratio - 0.5) * 2));
        Green := 255;
        Blue := 0;
    end;

    // Convert RGB to integer color directly (RGB format)
    Result := (Red) + (Green shl 8) + (Blue shl 16);
End;

// Procedure to export a template CSV with all net names
Procedure ExportCSVTemplate;
Const
    DEFAULT_TEMPLATE_FILE = 'MyCurrents.csv'
Var
    Board : IPCB_Board;
    NetList : TObjectList;
    NetIterator : IPCB_BoardIterator;
    CurrentNet : IPCB_Net;
    i : Integer;
    ResultsList : TStringList;
    ScriptPath : String;
    DEFAULT_FILE : String;
Begin
    DEFAULT_FILE := 'NetCurrentsTemplate.csv';

    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If (Board = Nil) Then
    begin
        ShowMessage('No board found');
        Exit;
    end;

    // Create list to store unique nets
    NetList := CreateObject(TObjectList);
    NetList.OwnsObjects := False; // Don't destroy nets when list is freed

    // Create string list for results
    ResultsList := TStringList.Create;

    Try
        // Add CSV header
        ResultsList.Add('Net Name,Current (A)');

        // Create iterator for net objects
        NetIterator := Board.BoardIterator_Create;
        NetIterator.AddFilter_ObjectSet(MkSet(eNetObject));
        NetIterator.AddFilter_LayerSet(AllLayers);
        NetIterator.AddFilter_Method(eProcessAll);

        // Collect all unique nets
        CurrentNet := NetIterator.FirstPCBObject;
        While (CurrentNet <> Nil) Do
        Begin
            NetList.Add(CurrentNet);
            CurrentNet := NetIterator.NextPCBObject;
        End;

        Board.BoardIterator_Destroy(NetIterator);

        // Add each net to the template with empty current value
        For i := 0 to NetList.Count - 1 Do
        Begin
            CurrentNet := NetList[i];
            ResultsList.Add(CurrentNet.Name + ',');
        End;

        ScriptPath := ScriptProjectPath(GetWorkspace);

        // Save the template to the selected file
        ResultsList.SaveToFile(ScriptPath + '\' + DEFAULT_TEMPLATE_FILE);
        ShowMessage('Template saved to: ' + ScriptPath);
    Finally
        // Free objects
        ResultsList.Free;
    End;
End;

// Procedure to color nets based on imported currents from CSV
Procedure ColorFromImportedCurrents;
Var
    Board : IPCB_Board;
    NetList : TObjectList;
    NetIterator : IPCB_BoardIterator;
    CurrentNet : IPCB_Net;
    i : Integer;
    CurrentsMap, ResultsCSV : TStringList;
    RowData : TStringList;
    OpenDialog : TOpenDialog;
    FilePath : String;
    row_idx : Integer;
    NetName : String;
    NetCurrent : Double;
    MinNetCurrent, MaxNetCurrent : Double;
    ColorValue : Integer;
    ColorsPath, ScriptPath : String;
    DEFAULT_COLORS_FILE : String;
    DEFAULT_LOW_CURRENT : Double;
    SaveOriginals : Boolean;
Begin
    DEFAULT_COLORS_FILE := 'OriginalNetColors.csv';
    DEFAULT_LOW_CURRENT := 0.001; // Default low current value for nets not in CSV

    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If (Board = Nil) Then
    begin
        ShowMessage('No board found');
        Exit;
    end;

    // Get script path
    ScriptPath := ScriptProjectPath(GetWorkspace);
    if ScriptPath = '' then
        ScriptPath := '.'; // Use current directory if script path not found

    // Create open dialog to prompt for CSV file
    OpenDialog := TOpenDialog.Create(nil);
    OpenDialog.Title := 'Select the CSV file with net currents';
    OpenDialog.Filter := 'CSV file|*.csv';
    OpenDialog.DefaultExt := 'csv';
    OpenDialog.FilterIndex := 0;

    // If dialog is canceled, exit
    if not OpenDialog.Execute then
    begin
        OpenDialog.Free;
        Exit;
    end;

    FilePath := OpenDialog.FileName;
    OpenDialog.Free;

    // Create list to store unique nets
    NetList := CreateObject(TObjectList);
    NetList.OwnsObjects := False; // Don't destroy nets when list is freed

    // Create current map for lookup
    CurrentsMap := TStringList.Create;
    CurrentsMap.NameValueSeparator := '=';

    // Create list for CSV data
    ResultsCSV := TStringList.Create;

    // Create row data parser
    RowData := TStringList.Create;
    RowData.Delimiter := ',';
    RowData.StrictDelimiter := True;

    Try
        // Load CSV file
        ResultsCSV.LoadFromFile(FilePath);

        // Skip header row
        if ResultsCSV.Count > 0 then
        begin
            // Create a fast lookup dictionary
            for row_idx := 1 to ResultsCSV.Count-1 do // Start from 1 to skip header
            begin
                RowData.DelimitedText := ResultsCSV.Get(row_idx);
                if RowData.Count >= 2 then
                begin
                    NetName := Trim(RowData.Get(0));

                    // Only add if the current value is not empty
                    if Trim(RowData.Get(1)) <> '' then
                    begin
                        CurrentsMap.Values[NetName] := Trim(RowData.Get(1));
                    end;
                end;
            end;
        end;

        // Check if we found any currents
        if CurrentsMap.Count = 0 then
        begin
            ShowMessage('No valid current values found in the CSV file. Please ensure the file has the correct format: Net Name,Current (A)');
            Exit;
        end;

        // Create iterator for net objects
        NetIterator := Board.BoardIterator_Create;
        NetIterator.AddFilter_ObjectSet(MkSet(eNetObject));
        NetIterator.AddFilter_LayerSet(AllLayers);
        NetIterator.AddFilter_Method(eProcessAll);

        // First pass: collect all unique nets
        CurrentNet := NetIterator.FirstPCBObject;
        While (CurrentNet <> Nil) Do
        Begin
            NetList.Add(CurrentNet);
            CurrentNet := NetIterator.NextPCBObject;
        End;

        Board.BoardIterator_Destroy(NetIterator);

        // Find min and max current values for scaling
        MinNetCurrent := 999999.9;
        MaxNetCurrent := 0;

        // First check the values from the CSV
        for i := 0 to CurrentsMap.Count - 1 do
        begin
            NetCurrent := StrToFloatDef(CurrentsMap.ValueFromIndex[i], 0);
            if (NetCurrent > 0) then
            begin
                if (NetCurrent < MinNetCurrent) then MinNetCurrent := NetCurrent;
                if (NetCurrent > MaxNetCurrent) then MaxNetCurrent := NetCurrent;
            end;
        end;

        // Also consider the default low current value
        if (DEFAULT_LOW_CURRENT < MinNetCurrent) then MinNetCurrent := DEFAULT_LOW_CURRENT;

        // If no valid range found, use default values
        if (MinNetCurrent >= MaxNetCurrent) or (MinNetCurrent >= 999999) then
        begin
            MinNetCurrent := DEFAULT_LOW_CURRENT;
            if (MaxNetCurrent <= 0) then MaxNetCurrent := 1;
        end;

        // Set colors path to script project path
        ColorsPath := ScriptPath + '\' + DEFAULT_COLORS_FILE;

        // Ask if user wants to save original colors for later restoration
        SaveOriginals := ConfirmNoYes('Save original net colors for later restoration?');

        // Save original colors if requested
        if SaveOriginals then
        begin
            SaveOriginalNetColors(NetList, ColorsPath);
            ShowMessage('Original net colors saved to: ' + ColorsPath);
        end;

        // Start modifying colors
        PCBServer.PreProcess;

        // For each net, set its color based on current
        For i := 0 to NetList.Count - 1 Do
        Begin
            CurrentNet := NetList[i];

            // Check if this net's name is in our currents map
            if CurrentsMap.IndexOfName(CurrentNet.Name) >= 0 then
            begin
                // Get the current value from CSV
                NetCurrent := StrToFloatDef(CurrentsMap.Values[CurrentNet.Name], DEFAULT_LOW_CURRENT);

                // Ensure a positive value
                if (NetCurrent <= 0) then NetCurrent := DEFAULT_LOW_CURRENT;
            end
            else
            begin
                // Use default low current for nets not in the CSV
                NetCurrent := DEFAULT_LOW_CURRENT;
            end;

            // Calculate color based on current value
            ColorValue := GetColorForCapacity(NetCurrent, MinNetCurrent, MaxNetCurrent);

            // Set net color
            CurrentNet.Color := ColorValue;
            CurrentNet.OverrideColorForDraw := True;

            Board.ViewManager_GraphicallyInvalidatePrimitive(CurrentNet);
        End;

        PCBServer.PostProcess;

        // Refresh display
        Client.SendMessage('PCB:Zoom', 'Action=Redraw', 255, Client.CurrentView);

        ShowMessage('All nets colored by current values (Red=Low, Yellow=Medium, Green=High). Min: ' +
                    FloatToStrF(MinNetCurrent, ffFixed, 10, 4) + 'A, Max: ' +
                    FloatToStrF(MaxNetCurrent, ffFixed, 10, 4) + 'A. Nets not in CSV defaulted to ' +
                    FloatToStrF(DEFAULT_LOW_CURRENT, ffFixed, 10, 4) + 'A');

    Finally
        // Free string lists
        CurrentsMap.Free;
        RowData.Free;
        ResultsCSV.Free;
    End;
End;

Procedure ColorFromCalculatedCurrentCapactity;
Const
    DEFAULT_FILE = 'NetCurrents.csv';
    DEFAULT_COLORS_FILE = 'OriginalNetColors.csv';
Var
    Board : IPCB_Board;
    LStack : IPCB_LayerStack;
    NetList : TObjectList;
    NetIterator : IPCB_BoardIterator;
    CurrentNet : IPCB_Net;
    TrackIter : IPCB_GroupIterator;
    Track : IPCB_Primitive;
    ProcessedTracks : TObjectList;
    ProcessedVias : TObjectList;
    CurrentPath : TObjectList;
    PathCapacity : Double;
    i : Integer;
    ResultsList : TStringList;
    NetCapacities : TStringList;
    MinNetCapacity, MaxNetCapacity : Double;
    HasPaths : Boolean;
    ColorValue : Integer;
    SaveCSV, ColorNets : Boolean;
    ColorsPath, ScriptPath : String;
    TempRiseC : Double;
    TempRiseStr : String;
Begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If (Board = Nil) Then
    begin
        ShowMessage('No board found');
        Exit;
    end;

    // Get script path
    ScriptPath := ScriptProjectPath(GetWorkspace);
    if ScriptPath = '' then
        ScriptPath := '.'; // Use current directory if script path not found

    // Prompt for temperature rise
    TempRiseStr := InputBox('Temperature Rise', 'Enter temperature rise in °C:', '10');
    TempRiseC := StrToFloatDef(TempRiseStr, 10);

    // Ensure valid temperature rise value
    if TempRiseC <= 0 then
    begin
        ShowMessage('Invalid temperature rise. Using default value of 10°C.');
        TempRiseC := 10;
    end;

    // Ask if user wants to color nets
    ColorNets := ConfirmNoYes('Color nets based on current capacity?');

    // Ask if user wants to save CSV
    SaveCSV := ConfirmNoYes('Save minimum current carrying capacity for each net in a csv file?');

    // Get the layer stack
    LStack := Board.LayerStack;

    // Create list to store unique nets
    NetList := CreateObject(TObjectList);
    NetList.OwnsObjects := False; // Don't destroy nets when list is freed

    // Create list to track processed tracks and vias
    ProcessedTracks := CreateObject(TObjectList);
    ProcessedTracks.OwnsObjects := False; // Don't destroy tracks

    ProcessedVias := CreateObject(TObjectList);
    ProcessedVias.OwnsObjects := False; // Don't destroy vias

    // Create string list for results
    ResultsList := TStringList.Create;

    // Create string list for net capacities
    NetCapacities := TStringList.Create;

    Try
        // Add CSV header
        ResultsList.Add('Net Name,Current Capacity (A) for ' + FloatToStr(TempRiseC) + '°C rise');

        // Create iterator for net objects
        NetIterator := Board.BoardIterator_Create;
        NetIterator.AddFilter_ObjectSet(MkSet(eNetObject));
        NetIterator.AddFilter_LayerSet(AllLayers);
        NetIterator.AddFilter_Method(eProcessAll);

        // First pass: collect all unique nets
        CurrentNet := NetIterator.FirstPCBObject;
        While (CurrentNet <> Nil) Do
        Begin
            NetList.Add(CurrentNet);
            NetCapacities.Add('999999.9'); // Initialize with a high value
            CurrentNet := NetIterator.NextPCBObject;
        End;

        Board.BoardIterator_Destroy(NetIterator);

        // Set colors path to script project path
        ColorsPath := ScriptPath + '\' + DEFAULT_COLORS_FILE;

        // Always save original colors for potential restoration
        SaveOriginalNetColors(NetList, ColorsPath);

        // Clear processed lists
        ProcessedTracks.Clear;
        ProcessedVias.Clear;

        // Initialize min/max capacity values
        MinNetCapacity := 999999.9;
        MaxNetCapacity := 0;

        // Second pass: find worst-case current capacity for each net
        For i := 0 to NetList.Count - 1 Do
        Begin
            CurrentNet := NetList[i];

            // Skip power/ground nets if desired
            // if (CurrentNet.Name = 'GND') or (CurrentNet.Name = 'VCC') then Continue;

            // Create group iterator for this net's tracks
            TrackIter := CurrentNet.GroupIterator_Create;
            TrackIter.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject));

            HasPaths := False;

            // Process each track that hasn't been processed yet
            Track := TrackIter.FirstPCBObject;
            While (Track <> Nil) Do
            Begin
                // Only process tracks we haven't seen yet
                if (ProcessedTracks.IndexOf(Track) = -1) then
                begin
                    // Trace the complete path starting from this track
                    CurrentPath := TraceCompletePath(Board, Track, ProcessedTracks, ProcessedVias);

                    // Calculate capacity for this path with the specified temperature rise
                    if (CurrentPath.Count > 0) then
                    begin
                        PathCapacity := CalculatePathCurrentCapacity(CurrentPath, Board, LStack, TempRiseC);

                        // If valid capacity, update minimum for this net
                        if (PathCapacity > 0) then
                        begin
                            if (not HasPaths) or (PathCapacity < StrToFloat(NetCapacities[i])) then
                            begin
                                NetCapacities[i] := FloatToStrF(PathCapacity, ffFixed, 10, 4);
                                HasPaths := True;
                            end;
                        end;
                    end;
                end;

                Track := TrackIter.NextPCBObject;
            End;

            // Destroy iterator
            CurrentNet.GroupIterator_Destroy(TrackIter);

            // Update min/max for all nets (for color scaling)
            if (HasPaths) then
            begin
                PathCapacity := StrToFloat(NetCapacities[i]);
                if (PathCapacity < MinNetCapacity) then MinNetCapacity := PathCapacity;
                if (PathCapacity > MaxNetCapacity) then MaxNetCapacity := PathCapacity;
            end;
        End;

        // Ensure we have a valid range
        if (MinNetCapacity >= MaxNetCapacity) then
        begin
            MinNetCapacity := 0;
            if (MaxNetCapacity <= 0) then MaxNetCapacity := 1;
        end;

        ShowMessage('Min capacity: ' + FloatToStrF(MinNetCapacity, ffFixed, 10, 4) + 'A, Max capacity: ' +
                    FloatToStrF(MaxNetCapacity, ffFixed, 10, 4) + 'A for ' + FloatToStr(TempRiseC) + '°C temperature rise');

        // Third pass: color the nets based on capacity if requested
        if (ColorNets Or SaveCSV) then
        begin
            PCBServer.PreProcess;

            For i := 0 to NetList.Count - 1 Do
            Begin
                CurrentNet := NetList[i];

                // Skip nets with no valid capacity
                if (NetCapacities[i] = '999999.9') then Continue;

                // Get color based on capacity
                PathCapacity := StrToFloat(NetCapacities[i]);
                ColorValue := GetColorForCapacity(PathCapacity, MinNetCapacity, MaxNetCapacity);

                // Set net color
                If ColorNets Then
                Begin
                    CurrentNet.Color := ColorValue;
                    CurrentNet.OverrideColorForDraw := True;

                    Board.ViewManager_GraphicallyInvalidatePrimitive(CurrentNet);
                End;

                If SaveCSV Then
                Begin
                    if (NetCapacities[i] = '999999.9') then
                        ResultsList.Add(CurrentNet.Name + ',N/A')
                    else
                        ResultsList.Add(CurrentNet.Name + ',' + NetCapacities[i]);
                End;
            End;

            PCBServer.PostProcess;

            // Refresh display
            Client.SendMessage('PCB:Zoom', 'Action=Redraw', 255, Client.CurrentView);

            ShowMessage('Nets colored by current capacity (Red=Low, Yellow=Medium, Green=High) for ' +
                        FloatToStr(TempRiseC) + '°C temperature rise. Select All Nets from PCB Panel to refresh the colors.');
        end;

        // Save results to CSV file if requested
        if (SaveCSV) then
        begin
            ResultsList.SaveToFile(ScriptPath + '\' + DEFAULT_FILE);
            ShowMessage('Results saved to ' + ScriptPath + '\' + DEFAULT_FILE);
        end;

        // Refresh display
        Client.SendMessage('PCB:Zoom', 'Action=Redraw', 255, Client.CurrentView);

    Finally
        // Free string lists
        ResultsList.Free;
        NetCapacities.Free;
    End;
End;
