unit Protocol;
{ MiniCoin Wallet and Miner Copyright (c) 2019 by Preben Bjorn Biermann Madsen
  Based on Kreditz Coin Copyright (c) 2018 Pjor, Kreditz Team
  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the MiniCoin Project, an infinitely scalable
  cryptocurrency. Find us here:

  If you like it, consider a donation using:

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Master, Crypto, CommandLineParser, Blocks,
  Timeunit, IdGlobal, Zipper;

{$I config.inc}
function IsValidProtocol(line: string): boolean;
function JoinString(): string;
procedure ParseProtocolConnection(Lines: TStringList; Slot: int64);
procedure PTC_Join(TextLine: string; slot: int64; Answer: boolean);
procedure PTC_ServerClose(slot: int64);
procedure SendOutGoingMessages();
procedure SendPTCMessage(Slot: int64; Message: string);
procedure PTC_ACRE(Textline: string; slot: int64);
function TranxAlreadyPending(TypeOfTrx, ValTrx: string; ParamNum: int64): boolean;
procedure PTC_SendPending(Slot: int64);
function ReplaceTimestamp(TextLine: string): string;
procedure AddPendingByTimestamp(Pending: string);
procedure PTC_Newblock(TextLine: string);
procedure SendAccDataFile(slot: int64);
procedure PTC_TRFR(Textline: string);
function TrxExistsInLastBlock(hash: string): boolean;
function GetNodesString(): string;
procedure PTC_GetNodes(slot: int64);
procedure PTC_SaveNodes(Textline: string);
function GetComisionValue(Monto: int64): int64;
function GetComisionIncluded(Monto: int64): int64; //*??
function SendFundsFromAddress(Destination: string; AddressIndex, Restante: int64; concepto: string;
  comisionrestante: int64): int64;
function GetAddressPaymentsOnPending(Address: string): int64;
procedure PTC_SendZipedBlocks(textline: string; slot: int64);
procedure UnzipBlockFile(filename: string);
function GetCharsFromMinerDiff(minerdiff: string): int64;
function GetStepsFromMinerDiff(minerdiff: string): int64;
function GetNodeFromString(DataString: string): NodeData;
procedure PTC_InBlRe();

implementation

uses
  MC_Main;

// RETURNS IF A STRING IS A VALID PROTOCOL MESSAGE
function IsValidProtocol(line: string): boolean;
var
  Start, Finish: string;
begin
  Start := copy(line, 1, 4);
  finish := copy(line, length(line) - 3, 4);
  if ((start = '{MIC') and (finish = 'END}')) then
    result := true
  else
    result := false;
end;

// RETURNS THE STRING FOR JOIN MESSAGES
function JoinString(): string;
var
  PortData: string;
begin
  if Form1.IdTCPServer1.Active = false then
    PortData := '-' + OptionsData.ListeningPort
  else
    PortData := OptionsData.ListeningPort;
  result := '{MIC JOIN ' + IntToStr(GetTotalConex) + ' ' + GetTimeStamp() + ' ' + IntToStr(LOCAL_MyLastBlock) + ' ' +
    LOCAL_LastBlockHash + ' ' + IntToStr(LOCAL_MyLastAccount) + ' ' + IntToStr(PendingTXs.Count) + ' ' +
    PortData + ' ' + LOCAL_MyAccsumHash + ' ' + MAIN_Version + ' END}';
end;

// PARSES A PROTOCOL LINE
procedure ParseProtocolConnection(Lines: TStringList; Slot: int64);
var
  CommandUsed: string = '';
  TextLine: string = '';
begin
  while Lines.Count > 0 do
  begin
    if ((not IsValidProtocol(Lines[0])) and (not Conexiones[slot].Autentic)) then
    begin
      OutputText('CONNECTION REJECTED: INVALID PROTOCOL -> ' + conexiones[slot].ip);
      if not BLNodeExists(conexiones[slot].ip, '') then
        AddNewBLNode(conexiones[slot].ip, '');
      CloseConnectionSlot(Slot);
      exit;
    end;
    TextLine := copy(Lines[0], 6, length(Lines[0]) - 10);
    CommandUsed := GetCommandLineCommand(TextLine);
    if UpperCase(CommandUsed) = 'JOIN' then
      PTC_Join(TextLine, slot, true)
    else if UpperCase(CommandUsed) = 'JOIR' then
      PTC_Join(TextLine, slot, false)
    else if UpperCase(CommandUsed) = 'SERVERCLOSE' then
      PTC_ServerClose(slot)
    else if UpperCase(CommandUsed) = 'ACRE' then
      PTC_ACRE(Textline, slot)
    else if UpperCase(CommandUsed) = 'GETPENDING' then
      PTC_SendPending(slot)
    else if UpperCase(CommandUsed) = 'NWBL' then
      PTC_Newblock(Textline)
    else if UpperCase(CommandUsed) = 'LASTBLOCK' then
      PTC_SendZipedBlocks(Textline, slot)
    else if UpperCase(CommandUsed) = 'LASTACC' then
      SendAccDataFile(slot)
    else if UpperCase(CommandUsed) = 'TRFR' then
      PTC_TRFR(Textline)
    else if UpperCase(CommandUsed) = 'GETNODES' then
      PTC_GetNodes(slot)
    else if UpperCase(CommandUsed) = 'NODES' then
      PTC_SaveNodes(Textline)
    else if UpperCase(CommandUsed) = 'INVALIDBLOCKREQUEST' then
      PTC_InBlRe()
    else
      Outputtext('Unknown command: ' + TextLine + ' from ' + conexiones[slot].ip);
    if Lines.Count > 0 then
      Lines.Delete(0);
  end;
end;

// HELLO MESSAGES BETWEN PEERS
procedure PTC_Join(TextLine: string; slot: int64; Answer: boolean);
var
  conections, block, lastblockhash, account, pending, port, accsumhash, version: string;
  peertime: string;
  Listening: boolean;
  paramlist: tstringlist;
begin
  STATUS_IncomingPings := STATUS_IncomingPings + 1;
  paramlist := TStringList.Create;
  try
    GetParams(paramlist, TextLine);
    conections := paramlist[1];
    peertime := paramlist[2];
    block := paramlist[3];
    lastblockhash := paramlist[4];
    account := paramlist[5];
    pending := paramlist[6];
    port := paramlist[7];
    if AnsiContainsStr(Port, '-') then
      listening := false
    else
      listening := true;
    port := StringReplace(port, '-', '', [rfReplaceAll, rfIgnoreCase]);
    accsumhash := paramlist[8];
    version := paramlist[9];
    if not IsValidInt(conections) then
      exit;
    conexiones[slot].Autentic := true;
    conexiones[slot].Connections := StrToInt(conections);
    conexiones[slot].Lastblock := block;
    conexiones[slot].LastblockHash := lastblockhash;
    conexiones[slot].Accounts := account;
    conexiones[slot].Pending := pending;
    conexiones[slot].ListenPort := port;
    conexiones[slot].lastping := GetTimeStamp();
    conexiones[slot].AccountsHash := accsumhash;
    conexiones[slot].Version := version;
    conexiones[slot].Listening := listening;
    conexiones[slot].offset := IntToStr(abs(StrToInt64(peertime) - StrToInt64(GetTimeStamp())));
    if not NodeExists(conexiones[slot].ip, conexiones[slot].ListenPort) then
      AddNewNode(conexiones[slot].ip, conexiones[slot].ListenPort);
    if Answer then
      SendPTCMessage(slot, StringReplace(JoinString(), 'JOIN', 'JOIR', [rfReplaceAll, rfIgnoreCase]));
    STATUS_LastPing := StrToInt64(copy(conexiones[slot].lastping, 1, 10));
  finally
    FreeAndNil(paramlist);
  end;
end;



// SEND A PROTOCOL MESSAGE TO A SPECIFIC SLOT
procedure SendPTCMessage(Slot: int64; Message: string);
begin
  if conexiones[Slot].tipo = 'client' then
    try
      Conexiones[Slot].context.Connection.IOHandler.writeln(Message);
    except
      On E: Exception do
      begin
        outputtext(E.Message);
        ClearConection('client', conexiones[Slot].ip);
      end;
    end;
  if conexiones[Slot].tipo = 'server' then
    try
      ConexionesCliente[Slot].IOHandler.writeln(Message);
    except
      On E: Exception do
      begin
        outputtext(E.Message);
        ClearConection('server', conexiones[Slot].ip);
      end;
    end;
end;

// CLIENT RECEIVES A NOTIFICATION THAT A SERVER CLOSED THE CONNECTION
procedure PTC_ServerClose(slot: int64);
begin
  try
  ReadsFromSlots[slot].Clear;
  ConexionesCliente[Slot].Disconnect;
  OutputText('Outgoing connection closed: ' + conexiones[slot].ip + ' -> Server disconnected');
  ClearConection('server', conexiones[Slot].ip);
  except
    OutPutText('Error in PTC_ServerClose');
  end;
end;

// SEND ALL OUTGOING MESSAGES TO ALL AVAILABLE PEERS
procedure SendOutGoingMessages();
var
  Slot: integer = 1;
begin
  while OutGoingMessages.Count > 0 do
  begin
    for Slot := 1 to CONST_MAXConections do
    begin
      if conexiones[Slot].tipo <> '' then
        SendPTCMessage(Slot, ReplaceTimeStamp(OutGoingMessages[0]));
      if OutGoingMessages.Count > 0 then
        OutGoingMessages.Delete(0);
    end;
  end;
end;

// PROCESS A NEW ACCOUNT REQUEST
procedure PTC_ACRE(Textline: string; slot: int64);
var
  timestamp, ip, publickey, ADhash, OPHash, SignedString: string;
  ResultStr: string;
  paramlist: tstringlist;
begin
  paramlist := TStringList.Create;
  try
    GetParams(paramlist, TextLine);
    timestamp := paramlist[1];
    ip := paramlist[2];
    publickey := paramlist[3];
    ADhash := paramlist[4];
    OPHash := paramlist[5];
    SignedString := paramlist[6];
    if TranxAlreadyPending('ACRE', ADHash, 5) then
      exit;
    if GetAddressPubKey(ADHash) <> '' then
      exit;
    if GetAddressFromPublicKey(publickey) <> ADhash then
      exit;
    if not IsValidAddress(ADHash) then
      exit;
    if not VerifySignedString('MY ADDRESS', SignedString, publickey) then
      exit;
    if ip = '0.0.0.0' then
      ip := conexiones[slot].ip;
    ResultStr := ('{MIC ACRE ' + timestamp + ' ' + Ip + ' ' + publickey + ' ' + ADhash + ' ' +
      ophash + ' ' + SignedString + ' END}');
    AddPendingByTimestamp(ResultStr); // ORDERED BY TIMESTAMP
    OutGoingMessages.Add(ResultStr);
  finally
    FreeAndNil(paramlist);
  end;
end;

// REPLACE TIMESTAMP FOR OUTGOING MESSAGES
function ReplaceTimestamp(TextLine: string): string;
begin
  result := StringReplace(TextLine, '$timestamp$', GetTImeStamp(), [rfReplaceAll, rfIgnoreCase]);
end;

// ADD A PENDING TRX BY TIMESTAMP
procedure AddPendingByTimestamp(Pending: string);
var
  i: int64 = 0;
  Timevalue, TimePending: int64;
  resultado: int64 = 0;
  Insertar: boolean = false;
begin
  Timevalue := StrToInt64(GetParameterFromCommandLine(Pending, 2));
  while i < PendingTXs.Count do
  begin
    TimePending := StrToInt64(GetParameterFromCommandLine(PendingTXs[i], 2));
    if TimeValue < TimePending then
    begin
      Resultado := i;
      Insertar := true;
      break;
    end;
    i := i + 1;
  end;
  if Insertar then
    PendingTxs.Insert(Resultado, Pending)
  else
    PendingTxs.Add(Pending);
end;

// GET A NEW BLOCK MESSAGES
procedure PTC_Newblock(TextLine: string);
var
  timestamp, Account, Solution, NewBlHash, blockNumber, TargetHash, Difficulty: string;
  DoIt: boolean = true;
  paramlist: tstringlist;
begin
  if not STATUS_Updated then
  begin
    OutGoingMessages.Add('{MIC ' + Textline + ' END}');
    DoIt := false;
  end;
  paramlist := TStringList.Create;
  try
    GetParams(paramlist, TextLine);
    timestamp := paramlist[1];
    blockNumber := paramlist[2];
    Account := paramlist[3];
    Solution := paramlist[4];
    NewBlHash := paramlist[5];
    TargetHash := paramlist[6];
    Difficulty := paramlist[7];
    if StrToInt(blockNumber) < BlockSumLastBlock() then
      exit; // if nwbl < last is not valid
    if StrToInt(blockNumber) = BlockSumLastBlock() then  // If the same number, detect the valid
      if NewBlHash = GetBlockData(StrToInt(blockNumber)).BlockHash then // the same
      begin
        OutputText('Block ' + (blockNumber) + ' same hash received');
        exit;
      end
      else // NOT IDENTICAL
      if StrToInt64(timestamp) >= StrToInt64(GetBlockData(StrToInt64(blockNumber)).TimeEnd) then
      begin
        OutputText('Block ' + (blockNumber) + ' older received: Omitted');
        exit;
      end
      else if StrToInt64(timestamp) < StrToInt64(GetBlockData(StrToInt64(blockNumber)).TimeEnd) then
      // the new is the good one
      begin
        OutputText('*************************************');
        OutputText('Better block ' + blockNumber + ' received');
        OutputText('*************************************');
        UndoneLastBlock(StrToInt64(blockNumber));
        BuildNewBlock(StrToInt64(blockNumber), TimeStamp, Account, Solution, NewBlHash, TargetHash, Difficulty);
        exit;
      end;
    if StrToInt64(blockNumber) = BlockSumLastBlock() + 1 then
    begin
      if VerifyMinerResult(Solution, Difficulty, TargetHash, StrToInt64(blockNumber)) > 0 then
      begin
        outputtext('Wrong Solution Block ' + blockNumber + ' : ' + solution + ' for ' + MINER_TargetHash);
        DoIt := false;
      end;
      if DoIT then
      begin
        OutputText('Solution Ok for block: ' + blockNumber + '. Building block');
        BuildNewBlock(StrToInt64(blockNumber), TimeStamp, Account, Solution, NewBlHash, TargetHash, Difficulty);
      end;
    end;
  finally
    FreeAndNil(paramlist);
  end;
end;

// RETURNS IF A TRANSACTION IS ALREADY PENDING / PARAMNUM IS +1 SINCE COMMAND=1
function TranxAlreadyPending(TypeOfTrx, ValTrx: string; ParamNum: int64): boolean;
var
  i: integer = 0;
  TypeofPending: string;
  ValPending: string;
begin
  result := false;
  if PendingTXs.Count > 0 then
    for i := 0 to PendingTXs.Count - 1 do
    begin
      TypeofPending := GetParameterFromCommandLine(PendingTxs[i], 1);
      ValPending := GetParameterFromCommandLine(PendingTxs[i], ParamNum);
      if ((TypeofPending = TypeOfTrx) and (ValPending = ValTrx)) then
        result := true;
    end;
end;

// SEND ALL PENDING TRX TO PEER
procedure PTC_SendPending(Slot: int64);
var
  i: integer;
begin
  if PendingTXs.Count > 0 then
    for i := 0 to PendingTXs.Count - 1 do
      SendPTCMessage(Slot, PendingTXs[i]);
end;

// SEND ACCDATA FILE
procedure SendAccDataFile(slot: int64);
var
  AFileStream: TFileStream;
begin
  AFileStream := TFileStream.Create(CONST_ArchivoAccData, fmOpenRead + fmShareDenyNone);
  try
    try
    if conexiones[Slot].tipo = 'client' then
    begin
      Conexiones[Slot].context.Connection.IOHandler.writeln('FILEACCSUM');
      Conexiones[Slot].context.connection.IOHandler.write(AFileStream, 0, true);
    end;
    if conexiones[Slot].tipo = 'server' then
    begin
      ConexionesCliente[Slot].IOHandler.writeln('FILEACCSUM');
      ConexionesCliente[Slot].IOHandler.write(AFileStream, 0, true);
    end;
    except
      OutPutText('Error in SendAccDataFile');
    end;
  finally
    AFileStream.Free;
  end;
end;

// PROCESS A TRANSFER REQUEST
procedure PTC_TRFR(Textline: string);
var
  TimeStamp, Sender, Destination, Monto, SigHash, OpHash, concepto: string;
  Proceder: boolean = true;
  paramlist: tstringlist;
begin
  paramlist := TStringList.Create;
  try
    GetParams(paramlist, TextLine);
    TimeStamp := paramlist[1];
    Sender := paramlist[2];
    Destination := paramlist[3];
    Monto := paramlist[4];
    SigHash := paramlist[5];
    OpHash := paramlist[6];
    Concepto := paramlist[7];
    if GetAddressBalanceFromDisk(Sender) - GetAddressPaymentsOnPending(Sender) < StrToInt64(Monto) then
      Proceder := false;
    if TranxAlreadyPending('TRFR', OpHash, 7) then
      Proceder := false;
    if StrToInt64(TimeStamp) < StrToInt64(GetBlockData(BlockSumLastBlock()).TimeStart) then
      exit;
    if TrxExistsInLastBlock(OpHash) then
      exit;
    if not VerifySignedString(TimeStamp + Sender + Destination + Monto + Concepto, SigHash, GetAddressPubKey(Sender)) then
      exit;
    if proceder then
    begin
      Textline := '{MIC ' + Textline + ' END}';
      AddPendingByTimestamp(Textline); // ORDERED BY TIMESTAMP
      OutGoingMessages.Add(Textline);
    end;
  finally
    FreeAndNil(paramlist);
  end;
end;

// RETURNS IF A TRXID WAS ADDED IN THE LAST BLOCK
function TrxExistsInLastBlock(hash: string): boolean;
var
  counter: integer;
begin
  result := false;
  for counter := 0 to LASTBLOCK_TrxsIDs.Count - 1 do
    if LASTBLOCK_TrxsIDs[counter] = hash then
    begin
      result := true;
      exit;
    end;
end;

// RETURNS THE COMISION FOR A TRANSACTION
function GetComisionValue(Monto: int64): int64;
begin
  result := CONST_MinFee;
end;

// RETURN THE COMISION INCLUDED IN A VALUE
function GetComisionIncluded(Monto: int64): int64;
begin
  if Monto <= CONST_MinFee then
      result := 0
    else
      result := CONST_MinFee;
end;

// SEND THE FUNDS FROM AN SPECIFIED ADDRESS
function SendFundsFromAddress(Destination: string; AddressIndex, Restante: int64; concepto: string;
  comisionrestante: int64): int64;
var
  MontoFinal: int64;
  TimeStamp, Sender, SignedHash, TrxHash: string;
begin
  if GetAddressAvailable(ArrayMyAddresses[AddressIndex].Hash) > Restante then
    MontoFinal := Restante
  else
    MontoFinal := GetAddressAvailable(ArrayMyAddresses[AddressIndex].Hash);
  Sender := ArrayMyAddresses[AddressIndex].Hash;
  TimeStamp := GetTimeStamp();
  SignedHash := GetStringSigned(TimeStamp + Sender + Destination + IntToStr(MontoFinal) + Concepto,
    ArrayMyAddresses[AddressIndex].PrivateKey);
  TrxHash := HashMD5String(TimeStamp + Sender + Destination + IntToStr(MontoFinal) + SignedHash);
  outputtext('Send ' + IntToStr(montofinal) + ' from ' + ArrayMyAddresses[AddressIndex].Hash);
  OutGoingMessages.Add('{MIC TRFR ' + timestamp + ' ' + Sender + ' ' + Destination + ' ' + IntToStr(MontoFinal) +
    ' ' + SignedHash + ' ' + TrxHash + ' ' + Concepto + ' ' + 'END}');
  result := MontoFinal;
end;

// RETURNS THE TOTAL PAYMENTS PENDING FOR AN ADDRESS
function GetAddressPaymentsOnPending(Address: string): int64;
var
  i: integer;
  MontoTotal: int64 = 0;
  Tipo, Sender: string;
begin
  if PendingTxs.Count > 0 then
    for i := 0 to PendingTxs.Count - 1 do
    begin
      Tipo := GetParameterFromCommandLine(PendingTxs[i], 1);
      Sender := GetParameterFromCommandLine(PendingTxs[i], 3);
      if ((Tipo = 'TRFR') and (Sender = Address)) then
        MontoTotal := MontoTotal + StrToInt64(GetParameterFromCommandLine(PendingTxs[i], 5));
    end;
  result := MontoTotal;
end;

// SEND THE BLOCS ZIPPED
procedure PTC_SendZipedBlocks(textline: string; slot: int64);
var
  FirstBlock, LastBlock: int64;
  MyZipFile: TZipper;
  i: integer;
  AFileStream: TFileStream;
begin
  if not IsValidInt(GetParameterFromCommandLine(textline, 1)) then
  begin
    SendPTCMessage(slot, '{MIC INVALIDBLOCKREQUEST END}');
    exit;
  end;
  FirstBlock := StrToInt64(GetParameterFromCommandLine(textline, 1)) + 1;
  LastBlock := FirstBlock + 99;
  if LastBlock > LOCAL_MyLastBlock then
    LastBlock := LOCAL_MyLastBlock;
  MyZipFile := TZipper.Create;
  MyZipFile.FileName := CONST_DirBlocks + 'Blocks_' + IntToStr(FirstBlock) + '_' + IntToStr(LastBlock) + '.zip';
  for i := FirstBlock to LastBlock do
    MyZipFile.Entries.AddFileEntry(CONST_DirBlocks + IntToStr(i) + '.blk');
  MyZipFile.ZipAllFiles;
  AFileStream := TFileStream.Create(MyZipFile.FileName, fmOpenRead + fmShareDenyNone);
  try
    try
    if conexiones[Slot].tipo = 'client' then
    begin
      Conexiones[Slot].context.Connection.IOHandler.writeln('BLOCKZIP');
      Conexiones[Slot].context.connection.IOHandler.write(AFileStream, 0, true);
    end;
    if conexiones[Slot].tipo = 'server' then
    begin
      ConexionesCliente[Slot].IOHandler.writeln('BLOCKZIP');
      ConexionesCliente[Slot].IOHandler.write(AFileStream, 0, true);
    end;
  except
    OutPutText('Error in SendZipedBlocks');
  end;
  finally
    AFileStream.Free;
    MyZipFile.Free;
  end;
  deletefile(CONST_DirBlocks + 'Blocks_' + IntToStr(FirstBlock) + '_' + IntToStr(LastBlock) + '.zip');
end;

// UNZIP THE RECEIVED BLOCKS  //* !!!!!! is there stored filename in the zip for unzipping  securrity risk
procedure UnzipBlockFile(filename: string);
var
  UnZipper: TUnZipper;
begin
  UnZipper := TUnZipper.Create;
  try
    UnZipper.FileName := filename;
    UnZipper.OutputPath := '';
    UnZipper.Examine;
    UnZipper.UnZipAllFiles;
  finally
    UnZipper.Free;
  end;
  deletefile(filename);
end;

// OBTAINS THE NUMBER OF CHARACTERS FROM A DIFFICUL STRING
function GetCharsFromMinerDiff(minerdiff: string): int64;
var
  Lettra: char;
begin
  Lettra := minerdiff[1];
  result := Ord(Lettra) - 96;
end;

// OBTAINS THE NUMBER OF STEPS FROM A DIFFICUL STRING
function GetStepsFromMinerDiff(minerdiff: string): int64;
var
  Lettra: char;
begin
  Lettra := minerdiff[2];
  result := Ord(Lettra) - 96;
end;

// RETURNS THE STRING WITH THE FIRST 50 NODES
function GetNodesString(): string;
var
  NodesString: string = '';
  NodesAdded: integer = 0;
  Counter: integer;
begin
  for counter := 0 to length(ArrayNodos) - 1 do
  begin
    NodesString := NodesString + ' ' + ArrayNodos[counter].ip + ':' + ArrayNodos[counter].port + ':';
    NodesAdded := NodesAdded + 1;
    if NodesAdded > 50 then
      break;
  end;
  NodesString := '{MIC NODES' + NodesString + ' END}';
  result := NodesString;
end;

// SEND THE NODES TO PEER
procedure PTC_GetNodes(slot: int64);
var
  NodesString: string = '';
begin
  NodesString := GetNodesString();
  SendPTCMessage(slot, NodesString);
end;

// SAVE NODES RECEIVED FROM PEER
procedure PTC_SaveNodes(Textline: string);
var
  ArrParameters: array of string;
  i: integer = 1;
  ThisParam: string = '';
  MoreParam: boolean = true;
  ThisNode: NodeData;
begin
  SetLength(ArrParameters, 0);
  while MoreParam do
  begin
    ThisParam := GetParameterFromCommandLine(textline, i);
    if thisparam = '' then
      MoreParam := false
    else
    begin
      SetLength(ArrParameters, length(ArrParameters) + 1);
      ArrParameters[length(ArrParameters) - 1] := ThisParam;
    end;
    i := i + 1;
  end;
  for i := 0 to length(ArrParameters) - 1 do
  begin
    thisnode := GetNodeFromString(ArrParameters[i]);
    if uppercase(thisnode.ip) = 'LOCALHOST' then
      thisnode.ip := '127.0.0.1';
    if thisnode.ip = '127.0.0.1' then
      continue;
    if not NodeExists(thisnode.ip, thisnode.port) then
      AddNewNode(thisnode.ip, thisnode.port);
  end;
end;

// GET NODE DATA FROM STRING
function GetNodeFromString(DataString: string): NodeData;
var
  counter: integer;
  Founded: boolean = false;
  ThisData: string = '';
  Resultado: NodeData;
  ThisChar: char;
begin
  for counter := 1 to length(DataString) do
  begin
    ThisChar := DataString[counter];
    if ThisChar = ':' then
    begin
      if not Founded then
      begin
        Resultado.ip := ThisData;
        Founded := true;
        ThisData := '';
      end
      else
      begin
        Resultado.port := ThisData;
      end;
    end
    else
      ThisData := ThisData + ThisChar;
  end;
  Resultado.LastAvailable := '';
  result := Resultado;
end;

// READJUST LAST BLOCK REQUESTED IF PEER SAYS WE MADE A BAD REQUEST
procedure PTC_InBlRe();
begin
  if LOCAL_MyLastBlock < STATUS_LastBlockRequested then
    STATUS_LastBlockRequested := LOCAL_MyLastBlock;
end;

end.  // END UNIT
