unit CommandLineParser;
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
  Classes, SysUtils, Master, Crypto, Forms;

{$I config.inc}
procedure ParseCommandLine(Linetext: string);
function GetCommandLineCommand(LineText: string): string;
function GetParameterFromCommandLine(LineText: string; ParamNumber: int64): string;
procedure GetParams(sl: TStrings; line: string);
procedure ShowHelp();
procedure ShowNodes();
procedure ShowBLNodes();
procedure AddNode(linetext: string);
procedure DeleteNode(linetext: string);
procedure DeleteBLNode(linetext: string);
procedure ShowStatus();
procedure ShowConexSlots();
procedure GetNewWallet();
procedure CreateNewAddress();
procedure ShowOutGoing();
procedure ShowPending();
procedure showaccounts();
procedure TurnMinerOn();
procedure TurnMinerOff();
procedure SendFunds(linetext: string);
procedure ShowBlockSum();
procedure GetCurrCiff();
procedure ReconnectOn();
procedure ReconnectOff();
procedure SetPort(linetext: string);
procedure ShowSha256String(linetext: string);
procedure CheckAddress(linetext: string);
procedure MinerInfo();
procedure AutoConn(linetext: string);
procedure FullNode(linetext: string);
procedure UpdateNodes(linetext: string);

implementation

uses
  MC_Main, Protocol, Timeunit;

// PARSES COMANDLINE
procedure ParseCommandLine(Linetext: string);
var
  CommandUsed: string;
begin
  CommandUsed := GetCommandLineCommand(Linetext);
  if CommandUsed = '' then
    exit;
  OutPutText('>> ' + linetext, false);
  if UpperCase(CommandUsed) = 'HELP' then
    ShowHelp
  else if UpperCase(CommandUsed) = 'NODES' then
    ShowNodes
  else if UpperCase(CommandUsed) = 'BLNODES' then
    ShowBLNodes
  else if UpperCase(CommandUsed) = 'ADDNODE' then
    AddNode(linetext)
  else if UpperCase(CommandUsed) = 'DELETENODE' then
    DeleteNode(linetext)
  else if UpperCase(CommandUsed) = 'DELETEBL' then
    DeleteBLNode(linetext)
  else if UpperCase(CommandUsed) = 'STATUS' then
    ShowStatus()
  else if UpperCase(CommandUsed) = 'CONNECT' then
    ConnectToServers()
  else if UpperCase(CommandUsed) = 'DISCONNECT' then
    CloseConectionsToServers()
  else if UpperCase(CommandUsed) = 'LISTENOFF' then
    TurnListenOff()
  else if UpperCase(CommandUsed) = 'LISTENON' then
    TurnListenOn()
  else if UpperCase(CommandUsed) = 'CONSLOTS' then
    ShowConexSlots()
  else if UpperCase(CommandUsed) = 'OUTGOING' then
    ShowOutgoing() // TEMP
  else if UpperCase(CommandUsed) = 'PENDING' then
    ShowPending() // TEMP
  else if UpperCase(CommandUsed) = 'HASHCOUNTER' then
    OutputText(IntToStr(MINER_HashCounter)) //* TEMP
  else if UpperCase(CommandUsed) = 'ACCSUMHASH' then
    OutputText(LOCAL_MyAccsumHash)
  else if UpperCase(CommandUsed) = 'MINERON' then
    TurnMinerOn()
  else if UpperCase(CommandUsed) = 'MINEROFF' then
    TurnMinerOff()
  else if UpperCase(CommandUsed) = 'NETTIME' then
    OutputText(TimestampToDate(GetTimestamp()))
  else if UpperCase(CommandUsed) = 'CLS' then
    ClearMemoLines()
  else if UpperCase(CommandUsed) = 'ACCOUNTS' then
    showaccounts()
  else if UpperCase(CommandUsed) = 'BLOCKSUM' then
    ShowBlockSum()
  else if UpperCase(CommandUsed) = 'SENDTO' then
    SendFunds(linetext)
  else if UpperCase(CommandUsed) = 'LBNUM' then
    OutputText('Last Block on BlockSum: ' + IntToStr(BlockSumLastBlock()))
  else if UpperCase(CommandUsed) = 'GETDIFF' then
    GetCurrCiff()
  else if UpperCase(CommandUsed) = 'RECON' then
    ReconnectOn()
  else if UpperCase(CommandUsed) = 'RECOFF' then
    ReconnectOff()
  else if UpperCase(CommandUsed) = 'SETPORT' then
    SetPort(linetext)
  else if UpperCase(CommandUsed) = 'SHA256' then
    ShowSha256String(linetext)
  else if UpperCase(CommandUsed) = 'CHECKADDRESS' then
    CheckAddress(linetext)
  else if UpperCase(CommandUsed) = 'MINERINFO' then
    Minerinfo()
  else if UpperCase(CommandUsed) = 'AUTOCONN' then
    AutoConn(linetext)
  else if UpperCase(CommandUsed) = 'FULLNODE' then
    FullNode(linetext)
  else if UpperCase(CommandUsed) = 'UPDATENODES' then
    UpdateNodes(linetext)
  else if UpperCase(CommandUsed) = 'LBSTARTIME' then
    OutputText(TimestampToDate(GetBlockData(BlockSumLastBlock()).TimeStart))
  else if Uppercase(CommandUsed) = 'TEST' then
    CheckIfMyAddressesNeedsRegisterFromDisk()
  else
    OutPutText('Unknown command: ' + CommandUsed, false);
end;

// GETS THE REQUIRED FUNCTION ON THE COMMANDLINE
function GetCommandLineCommand(LineText: string): string;
var
  Temp: string = '';
  ThisChar: char;
  i: int64 = 1;
begin
  while i <= Length(LineText) do
  begin
    ThisChar := Linetext[i];
    if ThisChar = ' ' then
    begin
      result := temp;
      exit;
    end
    else
      temp := temp + ThisChar;
    i := i + 1;
  end;
  result := Temp;
end;

// GET A DETERMINED PARAMETER FROM COMMANDLINE
function GetParameterFromCommandLine(LineText: string; ParamNumber: int64): string;
var
  Temp: string = '';
  ThisChar: char;
  i: int64 = 1;
  WhiteSpaces: int64 = 0;
begin
  while i <= Length(LineText) do
  begin
    ThisChar := Linetext[i];
    if ThisChar = ' ' then
    begin
      WhiteSpaces := WhiteSpaces + 1;
      if WhiteSpaces > Paramnumber then
      begin
        result := temp;
        exit;
      end;
    end
    else if WhiteSpaces = ParamNumber then
      temp := temp + ThisChar;
    i := i + 1;
  end;
  result := Temp;
end;

// GET PARAMETERS FROM COMMANDLINE AS A STRINGLIST
procedure GetParams(sl: TStrings; line: string);
var
   i : integer;
   ln : string;
begin
   ln := line + ' ';
   sl.BeginUpdate;
   sl.Clear;
   try
     while Length(ln) > 0 do
     begin
       i := Pos(' ', ln) ;
       sl.Add(Copy(ln,1,i-1));
       ln := Copy(ln,i+1,length(ln)) ;
     end;
   finally
     sl.EndUpdate;
   end;
end;

// SHOWS HELP
procedure ShowHelp();
begin
  OutPutText('* All commands are case insensitive', false);
  OutPutText('Help          - This Info', false);
  OutPutText('Nodes         - List the existing nodes', false);
  OutPutText('AddNode xx yy - Adds Node xx at port yy', false);
  OutPutText('DeleteNode x  - Delete Node stored at x position', false);
  OutPutText('Status        - Show The overall status', false);
  OutPutText('Connect       - Try to connect to all known nodes', false);
  OutPutText('Disconnect    - Disconnects all outgoing connections', false);
end;

// SHOWS NODES
procedure ShowNodes();
var
  i: int64 = 0;
begin
  while i < length(arraynodos) do
  begin
    OutPutText(IntToStr(i + 1) + ' - ' + arraynodos[i].ip + ':' + arraynodos[i].port, false);
    i := i + 1;
  end;
  if Length(ArrayNodos) = 0 then
    OutPutText('No nodes registered', false);
end;

// SHOWS BLACKLISTED NODES
procedure ShowBLNodes();
var
  i: int64 = 0;
begin
  while i < length(ArrayBlacklisted) do
  begin
    OutPutText(IntToStr(i + 1) + ' - ' + ArrayBlacklisted[i].ip, false);
    i := i + 1;
  end;
  if Length(ArrayBlacklisted) = 0 then
    OutPutText('No Blacklisted nodes', false);
end;

// ADD A NEW NODE
procedure AddNode(linetext: string);
var
  NodeIp, NodePort: string;
begin
  NodeIp := GetParameterFromCommandLine(linetext, 1);
  if NodeIp = '' then
  begin
    Outputtext('Missing Parameter 1: IP', false);
    exit;
  end;
  NodePort := GetParameterFromCommandLine(linetext, 2);
  if NodePort = '' then
  begin
    Outputtext('Missing Parameter 2: PORT', false);
    exit;
  end;
  if NodeExists(NodeIp, NodePort) then
    OutPutText('Node already registered', false)
  else
  begin
    AddNewNode(NodeIp, NodePort);
    OutPutText('Node added at position ' + IntToStr(length(ArrayNodos)), false);
  end;
end;

// DELETE A NODE
procedure DeleteNode(linetext: string);
var
  NodeNumber: int64;
begin
  if not IsValidInt(GetParameterFromCommandLine(linetext, 1)) then
  begin
    OutPutText('Invalid Parameter: ' + GetParameterFromCommandLine(linetext, 1), false);
    exit;
  end;
  NodeNumber := StrToInt64(GetParameterFromCommandLine(linetext, 1));
  if NodeNumber > Length(ArrayNodos) then
  begin
    OutPutText('Parameter ' + IntToStr(NodeNumber) + ' out of bounds', false);
    exit;
  end;
  DeleteExistingNode(NodeNumber - 1); // -1 Since arraynodos is 0 indexed
  OutPutText('Node ' + IntToStr(NodeNumber) + ' deleted', false);
end;

// DELETE A BLACKLISTED NODE
procedure DeleteBLNode(linetext: string);
var
  NodeNumber: int64;
begin
  if not IsValidInt(GetParameterFromCommandLine(linetext, 1)) then
  begin
    OutPutText('Invalid Parameter: ' + GetParameterFromCommandLine(linetext, 1), false);
    exit;
  end;
  NodeNumber := StrToInt64(GetParameterFromCommandLine(linetext, 1));
  DeleteBlackListedNode(NodeNumber - 1); // -1 Since arraynodos is 0 indexed
  OutPutText('Blacklisted Node ' + IntToStr(NodeNumber) + ' deleted', false);
end;

// SHOW STATUS
procedure ShowStatus();
begin
  OutPutText('To be implemented', false);
end;

// SHOW CONNECTIONS STATUS
procedure ShowConexSlots();
var
  i: integer;
  ShowClientNumber: string = '';
begin
  for i := 1 to CONST_MAXConections do
  begin
    if Conexiones[i].tipo = 'server' then
      ShowClientNumber := ' - CLI:' + Conexiones[i].ClientConn
    else
      ShowClientNumber := '';
    OutputText(IntToStr(i) + ' - ' + Conexiones[i].tipo + ' - ' + Conexiones[i].ip + ShowClientNumber, false);
  end;
end;

// CREATES A NEW WALLET IF NOT EXISTS
procedure GetNewWallet();
begin
  AssignFile(Filawallet, CONST_ArchivoWallet);
  rewrite(FilaWallet);
  closefile(FilaWallet);
  CreateNewAddress();
  SaveAddressesToDisk();
  OutputText('Wallet Created');
end;

// CREATES A NEW ADDRESS
procedure CreateNewAddress();
var
  PublicKey, PrivateKey: string;
  MyData: MyWalletData;
  Address: string;
begin
  CreateKeyPair();
  PublicKey := GetPublicKey();
  Privatekey := GetPrivateKey();
  Address := GetAddressFromPublicKey(PublicKey);
  MyData.Hash := Address;
  Mydata.PublicKey := PublicKey;
  MyData.PrivateKey := PrivateKey;
  MyData.Balance := '-1';
  MyData.RegisterStatus := 0;
  SetLength(ArrayMyAddresses, Length(ArrayMyAddresses) + 1);
  ArrayMyAddresses[length(ArrayMyAddresses) - 1] := MyData;
  U_SaveWallet := true;
  OutputText('Address Created: ' + Address);
end;

// SHOWS THE OUTGOING PROTOCOL MESSAGES
procedure ShowOutGoing();
var
  i: integer = 0;
begin
  if OutGoingMessages.Count > 0 then
  begin
    OutputText('OutGoing Messages: ' + IntToStr(OutGoingMessages.Count));
    for i := 0 to OutGoingMessages.Count - 1 do
      Outputtext(OutGoingMessages[i], false);
  end
  else
    Outputtext('Not Outgoing Messages', false);
end;

// SHOWS THE PENDING TXS
procedure ShowPending();
var
  i: integer = 0;
begin
  if PendingTxs.Count > 0 then
    for i := 0 to PendingTxs.Count - 1 do
      Outputtext(PendingTxs[i], false)
  else
    Outputtext('Not Pending Transactions', false);
end;

// SHOW ALL ACCOUNTS
procedure showaccounts();
var
  i: int64 = 0;
  DataRead: AccountData;
  Registered: string;
begin
  assignfile(FilaAccData, CONST_ArchivoAccData);
  reset(FilaAccData);
  while i < filesize(FilaAccData) do
  begin
    seek(FilaAccData, i);
    read(FilaAccData, DataRead);
    if DataRead.PublicKey <> '' then
      Registered := 'REGISTERED'
    else
      Registered := 'Unreg';
    outputtext(IntToStr(i) + ' - ' + copy(DataRead.Hash, 1, 8) + ' - ' + Int2CurrencyStr(
      StrToInt64(Dataread.Balance)) + ' ' + Registered + ' ' + Dataread.Lastop, false);
    i := i + 1;
  end;
  closefile(FilaAccData);
end;

// TURN THE MINER ON
procedure TurnMinerOn();
begin
  if STATUS_Updated then
  begin
    OptionsData.Mining := true;
    U_SaveOptions := true;
    MINER_IsMinerOn := false;
  end
  else
  begin
    OutputText('You can not mine now', false);
    OptionsData.Mining := true;
    U_SaveOptions := true;
    MINER_IsMinerOn := false;
  end;
end;

// TURN THE MINER OFF
procedure TurnMinerOff();
begin
  CloseMiningThread();
  OptionsData.Mining := false;
  U_SaveOptions := true;
  MINER_IsMinerOn := false;
end;

// SEND FUNDS TO ACCOUNT OR ADDRESS
procedure SendFunds(linetext: string);
var
  Destination, Amount: string;
  Monto, comision, MontoMasComision, Restante, comisionrestante: int64;
  i: int64;
  MontoFromAddress: int64;
  Message: string;
begin
  if not STATUS_Updated then
  begin
    Outputtext('You need connect and update to the network', false);
    ShowMsg('Your wallet is not updated');
    exit;
  end;
  Destination := GetParameterFromCommandLine(linetext, 1);
  Amount := GetParameterFromCommandLine(linetext, 2);
  Message := GetParameterFromCommandLine(linetext, 3);
  if IsValidInt(Destination) then
  begin
    Destination := GetAddressFromAccountNumber(StrToInt64(Destination));
    if StrToInt64(GetAccountNumberFromAddress(Destination)) < 0 then
    begin
      Outputtext('Account number do not exists', false);
      ShowMsg('Account number do not exists');
      exit;
    end;
  end;
  if ((Destination = '') or (not IsValidAddress(Destination))) then
  begin
    Outputtext('Invalid Destination: ' + destination, false);
    ShowMsg('Invalid Destination ' + destination);
    exit;
  end;
  if IsAddressMine(Destination) >= 0 then
  begin
    Outputtext('Can not send to your addresses', false);
    ShowMsg('Can not send to your addresses');
    exit;
  end;
  if IsValidFLoat(Amount) then
    Monto := Round(StrToFloat(Amount) * 100)
  else
  begin
    Outputtext('Invalid Amount', false);
    ShowMsg('Invalid Amount');
    exit;
  end;
  if Monto < 0 then
  begin
    Outputtext('Sending negative values is a bad idea', false);
    ShowMsg('Sending negative values is a bad idea');
    exit;
  end;
  comision := GetComisionValue(Monto);
  MontoMasComision := Monto + Comision;
  if MontoMasComision > MAIN_AccountBalance - GetTotalAccountPendingPayments() then
  begin
    Outputtext('Insufficient funds' + SlineBreak + 'You need ' + Int2CurrencyStr(MontoMasComision) + ' MIC', false);
    ShowMsg('Insufficient funds' + SlineBreak + 'You need ' + Int2CurrencyStr(MontoMasComision) + ' MIC');
    exit;
  end;
  Restante := MontoMasComision;
  comisionrestante := comision;
  i := length(ArrayMyAddresses) - 1;
  while Restante > 0 do
  begin
    if StrToInt64(ArrayMyAddresses[i].Balance) > 0 then
    begin
      MontoFromAddress := SendFundsFromAddress(Destination, i, Restante, Message, comisionrestante);
      Restante := Restante - MontoFromAddress;
      comisionrestante := comisionrestante - MontoFromAddress;
      if comisionrestante < 0 then
        comisionrestante := 0;
    end;
    i := i - 1;
  end;
  ShowMsg('Transfer Sucessfull' + SLINEBREAK + 'Sent   : ' + Int2CurrencyStr(Monto) + SLINEBREAK +
    'Fee    : ' + Int2CurrencyStr(comision) + SLINEBREAK + 'Total  : ' + Int2CurrencyStr(
    MontoMasComision) + SLINEBREAK + 'To     : ' + Destination + SLINEBREAK + 'Message : ' + Message);
end;

// SHOWS THE BLOSKCUM ARRAY
procedure ShowBlockSum();
var
  i: integer;
  BlNumber, Trxs, TotTime, timestart, timeend, miner, blhash: string;
  BlockData: BlockSumData;
begin
  for i := 0 to length(ArrBlockSummary) - 1 do
  begin
    BlockData := ArrBlockSummary[i];
    BlNumber := BlockData.Number;
    Trxs := BlockData.TrxTot;
    timestart := blockdata.TimeStart;
    timeend := Blockdata.TimeEnd;
    TotTime := BlockData.TimeTot;
    Miner := BlockData.AccountMiner;
    blhash := Blockdata.BlockHash;
    OutputText('Block:' + BlNumber + ' Trxs:' + Trxs + ' Start:' + timestart + ' End: ' + timeend + ' Duration:' + TotTime, false);
    OutPutText('   Miner: ' + miner + ' Hash: ' + blhash, false);
  end;
  OutputText('Block Sumarry contains ' + IntToStr(length(ArrBlockSummary)) + ' blocks', false);
end;

// SHOW DIFFICULT OF CURRENT MINING
procedure GetCurrCiff();
begin
  if not STATUS_Updated then
    OutputText('Can not retrieve difficulty', false)
  else
    OutputText('Block: ' + IntToStr(BlockSumLastBlock() + 1) + ' -> ' +
      IntToStr(GetCharsFromMinerDiff(MINER_MineDiff)) + ' Chars, ' +
      IntToStr(GetStepsFromMinerDiff(MINER_MineDiff)) + ' Steps', false);
end;

// TURN AUTORECONNECTIONS ON
procedure ReconnectOn();
begin
  OptionsData.Reconnect := true;
  U_SaveOptions := true;
  OutPutText('Reconnections ENABLED', false);
end;

// TURN AUTORECONNECTIONS OFF
procedure ReconnectOff();
begin
  OptionsData.Reconnect := false;
  U_SaveOptions := true;
  OutPutText('Reconnections DISABLED', false);
end;

// SET LOCAL LISTENING PORT
procedure SetPort(linetext: string);
var
  PortNumber: string;
begin
  if not IsValidInt(GetParameterFromCommandLine(linetext, 1)) then
  begin
    OutPutText('ERROR: Invalid Parameter: ' + GetParameterFromCommandLine(linetext, 1), false);
    ShowMsg('ERROR: Invalid Parameter: ' + GetParameterFromCommandLine(linetext, 1));
    exit;
  end;
  PortNumber := GetParameterFromCommandLine(linetext, 1);
  if StrToInt(PortNumber) < 1 then
  begin
    OutPutText('ERROR: Invalid Parameter: ' + GetParameterFromCommandLine(linetext, 1), false);
    ShowMsg('ERROR: Invalid Parameter: ' + GetParameterFromCommandLine(linetext, 1));
    exit;
  end;
  OptionsData.ListeningPort := PortNumber;
  OutPutText('New Listening port set' + SLineBreak + 'Change will be effective on next connection', false);
  ShowMsg('New Listening port set' + SLineBreak + 'Change will be effective on next connection');
  U_SaveOptions := true;
end;

// SHOWS THE SHA256 OF THE GIVEN PARAMETER
procedure ShowSha256String(linetext: string);
var
  TextToSha: string;
begin
  TextToSha := GetParameterFromCommandLine(linetext, 1);
  OutputText(HashSha256String(TextToSha), false);
end;

// SHOWS IF A GIVEN ADDRESS IS VALID
procedure CheckAddress(linetext: string);
var
  AddToCheck: string;
begin
  AddToCheck := GetParameterFromCommandLine(linetext, 1);
  if IsValidAddress(AddToCheck) then
    OutputText('✔ VALID ADDRESS', false)
  else
    OutputText('✘ INVALID ADDRESS', false);
end;

// SHOWS THE MINER VARIABLES
procedure MinerInfo();
begin
  OutputText('IsMinerOn             : ' + Booltostr(MINER_IsMinerOn, true), false);
  OutputText('OptionsData.Mining    : ' + Booltostr(OptionsData.Mining, true), false);
  OutputText('STATUS_Updated        : ' + Booltostr(STATUS_Updated, true), false);
  OutputText('MINER_BlockFound      : ' + Booltostr(MINER_BlockFound, true), false);
  OutputText('LASTBLOCK_Duration    : ' + IntToStr(LASTBLOCK_Duration), false);
  OutputText('MINER_TargetHash      : ' + MINER_TargetHash, false);
  OutputText('NETWORK_LastBlockHash : ' + NETWORK_LastBlockHash, false);
  OutputText('MINER_FoundedSteps    : ' + IntToStr(MINER_FoundedSteps), false);
  OutputText('MINER_Steps           : ' + IntToStr(MINER_Steps), false);
end;

// SET AUTOCONNECTION OPTION
procedure AutoConn(linetext: string);
var
  Option: string;
begin
  Option := GetParameterFromCommandLine(linetext, 1);
  if UpperCase(Option) = 'ON' then
  begin
    OptionsData.AutoConnect := true;
    U_SaveOptions := true;
  end
  else if UpperCase(Option) = 'OFF' then
  begin
    OptionsData.AutoConnect := false;
    U_SaveOptions := true;
  end
  else
    OutputText('Invalid Parameter. Use ON of OFF', false);
end;

// SET FULLNODE OPTION
procedure FullNode(linetext: string);
var
  Option: string;
begin
  Option := GetParameterFromCommandLine(linetext, 1);
  if UpperCase(Option) = 'ON' then
  begin
    OptionsData.FullNode := true;
    U_SaveOptions := true;
  end
  else if UpperCase(Option) = 'OFF' then
  begin
    OptionsData.FullNode := false;
    U_SaveOptions := true;
  end
  else
    OutputText('Invalid Parameter. Use ON of OFF', false);
end;

// SET GETNODES OPTION
procedure UpdateNodes(linetext: string);
var
  Option: string;
begin
  Option := GetParameterFromCommandLine(linetext, 1);
  if UpperCase(Option) = 'ON' then
  begin
    OptionsData.GetNodes := true;
    U_SaveOptions := true;
  end
  else if UpperCase(Option) = 'OFF' then
  begin
    OptionsData.GetNodes := false;
    U_SaveOptions := true;
  end
  else
    OutputText('Invalid Parameter. Use ON of OFF', false);
end;

end. // END UNIT
