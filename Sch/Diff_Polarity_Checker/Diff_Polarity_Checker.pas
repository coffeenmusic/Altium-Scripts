Function DiffPolarity(txt: String): Integer;
var
   polarity : Integer;
   t : String;
   isIn : Boolean;
Begin
     result := 0;

     t := LowerCase(txt);

     // If Else to Force priority ordering
     If      AnsiEndsStr('_dp', t) Then
     Begin
         result := 1;
     End
     Else If AnsiEndsStr('_dn', t) Then
     Begin
         result := -1;
     End
     Else If AnsiEndsStr('_dm', t) Then
     Begin
         result := -1;
     End
     Else If AnsiEndsStr('-dp', t) Then
     Begin
         result := 1;
     End
     Else If AnsiEndsStr('-dn', t) Then
     Begin
         result := -1;
     End
     Else If AnsiEndsStr('-dm', t) Then
     Begin
         result := -1;
     End
     Else If AnsiEndsStr('_p', t) Then
     Begin
         result := 1;
     End
     Else If AnsiEndsStr('_n', t) Then
     Begin
         result := -1;
     End
     Else If AnsiEndsStr('-p', t) Then
     Begin
         result := 1;
     End
     Else If AnsiEndsStr('-n', t) Then
     Begin
         result := -1;
     End
     Else If AnsiEndsStr('dp', t) Then
     Begin
         result := 1;
     End
     Else If AnsiEndsStr('dn', t) Then
     Begin
         result := -1;
     End
     Else If AnsiEndsStr('dm', t) Then
     Begin
         result := -1;
     End
     Else If AnsiEndsStr('_h', t) Then
     Begin
         result := 1;
     End
     Else If AnsiEndsStr('_l', t) Then
     Begin
         result := -1;
     End
     Else If AnsiEndsStr('-h', t) Then
     Begin
         result := 1;
     End
     Else If AnsiEndsStr('-l', t) Then
     Begin
         result := -1;
     End
     Else If AnsiEndsStr('+', t) Then
     Begin
         result := 1;
     End
     Else If AnsiEndsStr('-', t) Then
     Begin
         result := -1;
     End
     Else If ContainsText(t, '_dp') Then
     Begin
       result := 1;
     End
     Else If ContainsText(t, '_dn') Then
     Begin
       result := -1;
     End
     Else If ContainsText(t, '_dm') Then
     Begin
       result := -1;
     End
     Else If ContainsText(t, '-dp') Then
     Begin
       result := 1;
     End
     Else If ContainsText(t, '-dn') Then
     Begin
       result := -1;
     End
     Else If ContainsText(t, '-dm') Then
     Begin
       result := -1;
     End
     Else If ContainsText(t, '_p') Then
     Begin
       result := 1;
     End
     Else If ContainsText(t, '_n') Then
     Begin
       result := -1;
     End
     Else If ContainsText(t, '-p') Then
     Begin
       result := 1;
     End
     Else If ContainsText(t, '-n') Then
     Begin
       result := -1;
     End   
     Else If ContainsText(t, '+') And Not(AnsiStartsStr('+', t)) Then
     Begin
       result := 1;
     End
     Else If ContainsText(t, 'dp') And Not(AnsiStartsStr('dp', t)) Then
     Begin
       result := 1;
     End
     Else If ContainsText(t, 'dn') And Not(AnsiStartsStr('dn', t)) Then
     Begin
       result := -1;
     End
     Else If ContainsText(t, 'dm') And Not(AnsiStartsStr('dm', t)) Then
     Begin
       result := -1;
     End
     Else If ContainsText(t, 'p_') Then
     Begin
       result := 1;
     End
     Else If ContainsText(t, 'n_') Then
     Begin
       result := -1;
     End
     Else If ContainsText(t, '_h') Then
     Begin
       result := 1;
     End
     Else If ContainsText(t, '_l') Then
     Begin
       result := -1;
     End
     Else If ContainsText(t, '-h') Then
     Begin
       result := 1;
     End
     Else If ContainsText(t, '-l') Then
     Begin
       result := -1;
     End
     Else If ContainsText(t, 'h_') Then
     Begin
       result := 1;
     End
     Else If ContainsText(t, 'l_') Then
     Begin
       result := -1;
     End
     Else If ContainsText(t, 'p-') Then
     Begin
       result := 1;
     End
     Else If ContainsText(t, 'n-') Then
     Begin
       result := -1;
     End
     Else If ContainsText(t, 'h-') Then
     Begin
       result := 1;
     End
     Else If ContainsText(t, 'l-') Then
     Begin
       result := -1;
     End;
End;

Function IsPotentialDiffPair(txt: String): Boolean;
var
   polarity : Integer;
   t : String;
   isIn : Boolean;
Begin
     result := False;

     t := LowerCase(txt);

     if Not(AnsiStartsStr('net', t)) and (txt <> 'gnd') then
     Begin
         polarity := DiffPolarity(txt);
         if (polarity = -1) or (polarity = 1) then
         begin
             result := True;
         end;
     End;
End;

Function GetAllPinsSortedByComponent(Project: IProject): TStringList;
Var
    Doc         : IDocument;
    DocCnt, i : Integer;
    cmps : TStringList;
    pin                 :   INetItem;
    net                 :   INet;
    NetCnt, PinCnt : Integer;
    CmpDes, PinName, PinDes, NetName, DocName : String;
Begin
    cmps := TStringList.Create;
    cmps.NameValueSeparator := '=';

    For DocCnt := 0 to Project.DM_LogicalDocumentCount - 1 Do
    Begin
        Doc := Project.DM_LogicalDocuments(DocCnt);
        If Doc.DM_DocumentKind = 'SCH' Then
        Begin
            DocName := Doc.DM_FileName;
            // for each DM_Nets in document...
            for NetCnt := 0 to Doc.DM_NetCount - 1 do
            begin
                net := Doc.DM_Nets(NetCnt);

                // for each DM_Pins in nets...
                for PinCnt := 0 to net.DM_PinCount - 1 do
                begin
                    pin := net.DM_Pins(PinCnt);
                    CmpDes := pin.DM_LogicalPartDesignator;
                    PinName := pin.DM_PinNameNoPartId;
                    PinDes := pin.DM_PinName;
                    NetName := net.DM_NetName;

                    If IsPotentialDiffPair(NetName) And IsPotentialDiffPair(PinDes) Then
                    Begin
                        If cmps.IndexOfName(CmpDes) >= 0 Then
                        Begin
                            cmps.Values[CmpDes] := cmps.Values[CmpDes] + ';' + PinDes+':'+NetName;
                        End
                        Else
                        Begin
                            cmps.Add(CmpDes + '=' + PinDes+':'+NetName);
                        End;
                    End;

                end;
            end;
        End;
    End;

    i := cmps.Count;
    result := cmps;
End;

Function IsOffByNChar(txt1: String, txt2: String, n: Integer): Boolean;
Var
    i, matchCnt, digCnt : Integer;
    ch1, ch2 : Char;
    ch1Valid, ch2Valid: Boolean;
Begin
    result := False;
    ch1Valid := False;
    ch2Valid := False;

    matchCnt := 0;
    digCnt := 0;
    For i := 1 to Length(txt1) Do
    Begin
        ch1 := LowerCase(txt1[i]);
        ch2 := LowerCase(txt2[i]);

        If ch1 = ch2 Then
        Begin
           Inc(matchCnt);
        End
        Else
        Begin
           If (ch1 = 'p') or (ch1 = 'n') or (ch1 = 'm') or (ch1 = '+') or (ch1 = '-') or (ch1 = 'h') or (ch1 = 'l') Then ch1Valid := True;
           If (ch2 = 'p') or (ch2 = 'n') or (ch2 = 'm') or (ch2 = '+') or (ch2 = '-') or (ch2 = 'h') or (ch2 = 'l') Then ch2Valid := True;
        End;

    End;

    if (matchCnt >= Length(txt1)-n) and ch1Valid and ch2Valid Then result := True;
End;

Function CompareSameLenNets(matches: TStringList, cnts: TStringList): TStringList;
Var
    cnt, viewed : TStringList;
    i, j, cntIdx, cntsLen, cntLen: Integer;
    net1, net2 : String;
    IsMatch : Boolean;
Begin
    cnt := TStringList.Create;
    cnt.Delimiter := ';';
    cnt.StrictDelimiter := True;

    viewed := TStringList.Create;
    viewed.Sorted := True;
    viewed.Duplicates := dupIgnore;

    cntsLen := cnts.Count;

    for cntIdx := 0 to cnts.Count-1 do
    begin
        cnt.DelimitedText := cnts.ValueFromIndex[cntIdx];

        cntLen := cnt.Count;

        for i := 0 to cnt.Count - 1 do
        begin
            net1 := cnt[i];

            for j := 0 to cnt.Count - 1 do
            begin
                if i <> j then
                begin
                    net2 := cnt[j];

                    if viewed.IndexOf(net1 + net2) = -1 then
                    begin
                        IsMatch := IsOffByNChar(net1, net2, 1);

                        If IsMatch Then
                        Begin
                            matches.Add(net1);
                            matches.Add(net2);
                        End;

                        viewed.Add(net1+net2);
                        viewed.Add(net2+net1);
                    end;
                end;
            end;
        end;
        viewed.Clear;
    end;

    cnt.Free;
    viewed.Free;
End;

Function FindDiffPairCandidates(cmps: TStringList): TStringList;
Const
    pIdx = 0;
    nIdx = 1;
Var
    pinStr, netStr, cmpDes: String;
    cmpIdx, pinIdx, pinsCnt, pinCnt, len: Integer;
    pins, pin, cnts, tmp, matches: TStringList;
Begin
    pins := TStringList.Create;
    pins.Delimiter := ';';
    pins.StrictDelimiter := True;

    pin := TStringList.Create;
    pin.Delimiter := ':';
    pin.StrictDelimiter := True;

    tmp := TStringList.Create;
    tmp.Delimiter := ';';
    tmp.StrictDelimiter := True;

    cnts := TStringList.Create;
    cnts.NameValueSeparator := '=';

    matches := TStringList.Create;
    matches.Sorted := True;
    matches.Duplicates := dupIgnore;

    // Build a dictionary of dict[StringLength] = DelimitedNetList
    for cmpIdx := 0 to cmps.Count-1 do
    begin
        pins.DelimitedText := cmps.ValueFromIndex[cmpIdx];
        pinsCnt := pins.Count;

        for pinIdx := 0 to pins.Count - 1 do
        begin
            pin.DelimitedText := pins[pinIdx];
            pinCnt := pin.Count;

            if pinCnt = 2 Then
            Begin
                pinStr := pin[pIdx];
                netStr := pin[nIdx];

                // Filter out all other nets for speed
                If IsPotentialDiffPair(netStr) And IsPotentialDiffPair(pinStr) Then
                Begin
                    len := IntToStr(Length(netStr));

                    If cnts.IndexOfName(len) >= 0 Then
                    Begin
                        tmp.DelimitedText := cnts.Values[len];
                        If tmp.IndexOf(netStr) = -1 Then
                        Begin
                            cnts.Values[len] := cnts.Values[len] + ';' + netStr;
                        End;
                    End
                    Else
                    Begin
                        cnts.Add(len + '=' + netStr);
                    End;

                End;
            End;

        end;

        CompareSameLenNets(matches, cnts); // Iterate Same Length Strings and compare
        cnts.Clear;
    end;

    pins.Free;
    pin.Free;
    cnts.Free;
    tmp.Free;

    result := matches;
End;

Procedure Run;
Const
    DEFAULT_FILE = 'report.csv';
Var
    Project     : IProject;
    Doc         : IDocument;
    pin                 :   INetItem;
    net                 :   INet;
    i, DocCnt, NetCnt, PinCnt, PinPolarity, NetPolarity : Integer;
    CmpDes, PinName, PinDes, NetName, msg, DocName, SavePath : String;
    cmps, diffs, Report : TStringList;
    Next : Boolean;
    saveDialog : TSaveDialog;
Begin
    Project := GetWorkspace.DM_FocusedProject;
    If Project = Nil Then Exit;

    // Open Save File Dialog GUI
    saveDialog := TSaveDialog.Create(nil);
    saveDialog.Title := 'Please select a location and filename for the saved dataset.';
    saveDialog.Filter := 'CSV file|*.csv';
    saveDialog.DefaultExt := 'csv';
    saveDialog.FilterIndex := 0;
    saveDialog.FileName := DEFAULT_FILE;
    if saveDialog.Execute then
       SavePath := saveDialog.FileName
    else exit;

    cmps := GetAllPinsSortedByComponent(Project); // Create dictionary[Component] = Pin1:Net1;Pin2:Net2
    diffs := FindDiffPairCandidates(cmps); // Create TStringList = ['PotentialDiffNet1:PotentialDiffComplement1', '...']

    Report := TStringList.Create;
    Report.Clear;
    Report.Add('Component,PinNumber,PinName,NetName');

    For DocCnt := 0 to Project.DM_LogicalDocumentCount - 1 Do
    Begin
        Doc := Project.DM_LogicalDocuments(DocCnt);
        If Doc.DM_DocumentKind = 'SCH' Then
        Begin
                DocName := Doc.DM_FileName;
                // for each DM_Nets in document...
                for NetCnt := 0 to Doc.DM_NetCount - 1 do
                begin
                    net := Doc.DM_Nets(NetCnt);

                    // for each DM_Pins in nets...
                    for PinCnt := 0 to net.DM_PinCount - 1 do
                    begin
                        pin := net.DM_Pins(PinCnt);
                        CmpDes := Trim(pin.DM_LogicalPartDesignator);
                        PinName := Trim(pin.DM_PinNameNoPartId);
                        PinDes := Trim(pin.DM_PinName);
                        NetName := Trim(net.DM_NetName);

                        // If Net in list of potential differential pairs, check it's polarity compared to the pin
                        If diffs.IndexOf(NetName) >= 0 Then
                        Begin
                            PinPolarity := DiffPolarity(PinDes);
                            NetPolarity := DiffPolarity(NetName);

                            If (PinPolarity = 0) Or (NetPolarity = 0) Then Continue;

                            If (PinPolarity <> NetPolarity) Then
                            Begin
                                msg := CmpDes + ',' + PinName + ',' + PinDes + ',' + NetName;
                                Report.Add(msg);
                            End;
                        End;
                    end;
                end;
        End;
    End;

    Report.SaveToFile(SavePath);
    cmps.Free;
    diffs.Free;
    Report.Free;

    ShowMessage('Script Complete. Review the report file and compare Pin Names to Net Names to see if it found swizzled pins.');
End;
{..............................................................................}

{..............................................................................}
End.
