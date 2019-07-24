unit Master;
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
  Classes, SysUtils, Dialogs, FileUtil, Forms, Controls, ExtCtrls, Graphics, StdCtrls,
  IdContext, Dateutils,
  IdGlobal;

{$I config.inc}
type
  nodedata = packed record
    ip: string[15];
    port: string[8];
    LastAvailable: string[17];
  end;

  conectiondata = packed record
    Autentic: boolean;
    Connections: int64;
    tipo: string[8];
    ip: string[20];
    lastping: string[255];
    context: TIdContext;
    ClientConn: string[255];
    Lastblock: string[255];
    LastblockHash: string[255];
    Accounts: string[255];
    AccountsHash: string[255];
    Pending: string[255];
    ListenPort: string[255];
    Version: string[13];
    Listening: boolean;
    offset: string[4];
  end;

  UserData = packed record
    ListeningPort: string[8];
    Mining: boolean;
    Reconnect: boolean;
    MinimToTray: boolean;
    AutoConnect: boolean;
    FullNode: boolean;
    GetNodes: boolean;
    ShowMinned: boolean;
  end;

  AccountData = packed record  // Type used for file AccSum
    Number: string[13];
    PublicKey: string[130];
    Hash: string[40];
    Balance: string[13];
    //*    NumTrans: string[13];
    Lastop: string[13];
  end;

  TranxData = packed record
    block: string[13];
    TypeTx: string[6];
    TimeStamp: string[17];
    Sender: string[40];
    Receiver: string[40];
    Ammount: string[13];
    //*    NumTrans: string[13];
    Signature: string[130];
    Hash: string[40];
    Message: string[40];
  end;

  BlockSumData = packed record
    Number: string[13];
    TimeStart: string[20];
    TimeEnd: string[20];
    TimeTot: string[10];
    TrxTot: string[10];
    TargetHash: string[34];
    Difficult: string[3];
    NxtBlkDiff: string[3];
    BlockHash: string[40];
    AccountMiner: string[40];
    MinerFee: string[10];
  end;

  WalletData = packed record   // type used for wallet.dat
    Hash: string[40];
    PublicKey: string[130];
    PrivateKey: string[130];
  end;

  MyWalletData = packed record  // type used for internal array: ArrayMyAddresses
    Hash: string[40];
    PublicKey: string[130];
    PrivateKey: string[130];
    Balance: string[13];
    RegisterStatus: int64;
  end;

  BlockHeaderData = packed record
    Number: int64;
    TimeStart: int64;
    TimeEnd: int64;
    TimeTot: int64;
    TrxTot: int64;
    TargetHash: string[34];
    Difficult: string[3];
    NxtBlkDiff: string[3];
    AccountMiner: string[40];
    MinerFee: int64;
    Reward: int64;
    SolutionLength: int64;
  end;

  BlockArrTrxsData = array of TranxData;

// General
function IsValidInt(cadena: string): boolean;
function IsValidFloat(cadena: string): boolean;
function verifyfiles(): boolean;

// Nodes
function NodeExists(ip, port: string): boolean;
function BLNodeExists(ip, port: string): boolean;
procedure LoadNodesFromDisk();
procedure LoadBLNodesFromDisk();
procedure AddNewNode(Address, Port: string);
procedure AddNewBLNode(Address, Port: string);
procedure DeleteExistingNode(Number: int64);
procedure DeleteBlackListedNode(Number: int64);
procedure SaveNodesToDisk();
procedure SaveBLNodesToDisk();
procedure DeleteNodeAddress(Address: string);
function GetReachableNodes(): int64;

// User options
function LoadOptionsFromDisk(): UserData;
procedure SaveOptionsToDisk();

// Network data
function UpdateNetworkLastBlockData(): int64;
function UpdateNetworkAccountSumData(): int64;
function UpdateNetworkPendingData(): int64;

// Account and addresses
function IsAddressMine(address: string): int64;
procedure LoadWalletData();
procedure SaveAddressesToDisk();
function GetTotalAccountBalance(): int64;
function GetTotalAccountPendingPayments(): int64;
function GetAddressAvailable(Address: string): int64;
function GetAccountNumberFromAddress(address: string): string;
function GetAddressFromAccountNumber(Accnumber: int64): string;
function GetAddressBalanceFromDisk(Account: string): int64;
function GetAddressPubKey(Address: string): string;
procedure CheckIfMyAddressesNeedsRegisterFromDisk();
procedure AddAddressIfNotExists(address: string);
procedure VerifyUserAddressesForAcre();
function GetLastAccountUpdated(): int64;
function GetMyAccSumHash(): string;

// Blocks and blocksum
function GetMyLastUpdatedBlock(): int64;
function GetMyLastBLockHash(Lastblock: string): string;
function BlockSumLastBlock(): int64;
function GetBlockData(blnumber: int64): BlockSumData;
function GetBlockDataFromDisk(BlockNumber: int64): BlockSumData;
procedure BuildBlockSum();
procedure AdjustBlockSum();
function GetMyLastUpdatedBlockTrxs(): integer;
procedure SetMyLastUpdatedBlockTrxs(Number: integer);
function GetBlockTrxsFromDisk(BlockNumber: integer): BlockArrTrxsData;
procedure SaveToMyTrxs(transaction: TranxData);
procedure BuiltMyTrxs();

// Connections
function GetActiveConex(tipo: string): int64;
function GetTotalConex(): int64;
function SaveConection(tipo, ipuser: string; contextdata: TIdContext; clientnumber: string): int64;
function ClearConection(tipo, ipuser: string): boolean;
function GetFreeConexSlot(): int64;
function GetFreeCliente(): int64;
function ConnectClient(Address, Port: string): int64;
function AreWeConnectedTo(address: string): boolean;
function GetSlotFromIP(Ip: string): int64;
procedure ConnectToServers();
procedure TryConnectToNode(NodeNumber: int64);
procedure CloseConectionsToServers();
procedure TurnListenOff();
procedure TurnListenOn();
procedure CloseConnectionSlot(Slot: int64);

const
  CONST_ArchivoNodos = 'data/nodes.mic';
  CONST_Blacklist = 'data/blnodes.mic';
  CONST_ArchivoUser = 'data/user.mic';
  CONST_ArchivoAccData = 'data/accsum.mic';
  CONST_DirBlocks = 'data/blocks/';
  CONST_ArchivoWallet = 'data/wallet.mic';
  CONST_ArchivoBlockSum = 'data/blocksum.mic';
  CONST_ArchivoMyTxs = 'data/mytxs.mic';
  CONST_FileKeyDat = 'data/key.mic';

  CONST_MinimunConnectionsToWork = 1;
  CONST_PinsToStart = CONST_MinimunConnectionsToWork * 2;
  CONST_BlockSummaryLength = 20;
  CONST_ExpectedBlockDuration = 300;
  {$IFDEF PORT80}
  CONST_DefaultServerPort = 8080;
  {$ELSE}
  CONST_DefaultServerPort = 8081;
  {$ENDIF}
  CONST_MaxIncomingConections = 5;
  CONST_MaxOutgoingConections = 5;
  CONST_MinFee = 10;
  CONST_MAXConections = CONST_MaxIncomingConections + CONST_MaxOutgoingConections;

var
  FilaNodos: file of nodedata;  // Nodes
  U_SaveNodeFile: boolean = false;
  FilaBlackList: file of nodedata;
  U_SaveBlacklist: boolean = false;
  FilaUser: file of Userdata; // Options
  U_SaveOptions: boolean = false;
  FilaAccData: file of AccountData;
  FilaBlock: file of TranxData;
  FilaWallet: file of WalletData;
  U_SaveWallet: boolean = false;
  FilaSumary: file of BlocksumData;
  FilaMytxs: file of TranxData;
  U_RebuildMyTxs: boolean = false;

  OptionsData: UserData; // Options values
  LastCommandline: string = '';
  AutoRetryConnect: boolean = false; // Auto retry connection to nodes
  GLOBAL_TimeDiff: integer = 0;

  CONNECT_LastTime: int64 = 0;
  CONNECT_LastNode: int64 = 0;

  STATUS_Connected: boolean = false;
  STATUS_Synzed: boolean = false;
  STATUS_LastBlockRequested: int64 = 0;
  STATUS_LastAccountRequested: boolean = false;
  STATUS_Updated: boolean = false;
  STATUS_IncomingPings: int64 = 0;
  STATUS_LastPing: int64 = 0;

  LASTBLOCK_PendingTxs: TStringList;
  LASTBLOCK_UNDONE: int64 = 0;
  LASTBLOCK_ArrBlockSum: array of BlockSumData;
  LASTBLOCK_ArrMyAddresses: array of MyWalletData;
  LASTBLOCK_Duration: int64;
  LASTBLOCK_TrxsIDs: TStringList;

  MAIN_FirstFormShow: boolean = true;
  MAIN_AccountNumber: string = '';
  U_MyAccountNumber: boolean = false;
  MAIN_AccountHash: string = '';
  MAIN_AccountBalance: int64 = 0;
  U_MyBalance: boolean = false;
  MAIN_AccountPublic: string = '';
  MAIN_AccountPrivate: string = '';
  MAIN_Version: string = '0.0.4';
  MAIN_USER_IP: string = '';
  MAIN_ProgramStarted: boolean = false;

  MINER_IsMinerOn: boolean = false; // is the miner activated?
  MINER_HashCounter: int64 = 0; // the longint counter for mining
  MINER_LastHashCounter: int64 = 0; // used to calculate the mining hash speed
  MINER_HashSeed: string = ''; // the prefix for mining
  MINER_BlockFound: boolean = false; // true when the user mines the block
  MINER_ResultHash: string = ''; // result when block is found
  MINER_FoundedSteps: int64 = 0;  // founded steps of this block
  MINER_TargetHash: string = ''; // TARGET HASH TO FOUND THE NEW BLOCK
  MINER_MineDiff: string = 'fe';  // string containing the block difficulty
  MINER_TargetChars: int64 = 6; // Number of consecutive operations to mine the block
  MINER_Steps: int64 = 1; // number of steps to mine the block
  MINER_BlockDiffSet: int64 = -1; // block the user is mining
  MINER_CurrStepTarget: string = '';
  MINER_ThreadActiv: boolean = false; // is the miner activated?

  LOCAL_MyLastBlock: int64 = 0; // last block the user have in disk
  U_MylastBlock: boolean = false;
  LOCAL_LastBlockHash: string = ''; // hash of the last user block on disk
  U_MyLastBLockHash: boolean = false;
  LOCAL_MyLastAccount: int64 = 0; // last account user have registered on disk
  U_MyLastAccount: boolean = false;
  LOCAL_MyAccsumHash: string = ''; // Hash of the local account sumary
  U_MyAccsumHash: boolean = false;

  NETWORK_LastBLock: int64 = 0; // lastblock in the network
  NETWORK_LastBlockHash: string = ''; // target hash for next block
  NETWORK_LastAccount: int64 = 0; // acounnts registered on network
  NETWORK_AccsumHash: string = '';  // Accounts file hash
  NETWORK_Pending: int64 = 0;
  NETWORK_SendPing: boolean = false;

  ArrayNodos: array of nodedata;
  ArrayBlacklisted: array of nodedata;
  ArrayMyAddresses: array of MyWalletData;
  ReadsFromSlots: array [1..CONST_MAXConections] of TStringList;
  ArrayThreads: Array of int64;
  PendingTXs: TStringList;
  OutGoingMessages: TStringList;
  MainMemoStrings: TStringList;
  ProcessLines: TStringList;
  MyPendingTxs: TStringList;  // Pending Txs involving my addresses
  ArrBlockSummary: array of BlockSumData;
  Conexiones: array [1..CONST_MaxIncomingConections + CONST_MaxOutgoingConections] of conectiondata;

  FUserPassword, FPrivateKey, FPublicKey: string;
  SavedTrxs: integer = 0;

implementation

uses
  MC_Main, CommandLineParser, Protocol, Blocks, Crypto, Timeunit;

{*******************************************************************************
                                    GENERAL
*******************************************************************************}

// DETECTS IF A STRING IS A VALID int64
function IsValidInt(cadena: string): boolean;
begin
  result := true;
  try
    StrToInt64(cadena);
  except
    On E: EConvertError do
      result := false;
  end;
end;

// DETECTS IF A STRING IS A VALID FLOAT
function IsValidFloat(cadena: string): boolean;
begin
  result := true;
  try
    StrtoFloat(cadena);
  except
    On E: EConvertError do
      result := false;
  end;
end;

// VERIFY THE SYSTEM FILES AT LAUNCH
function verifyfiles(): boolean;
var
  FirsTimeUser: UserData;
  MiFirstTrx: TranxData;
begin
  result := false;
  // verify if data folder exists
  if directoryexists('data') = false then
  begin
    Application.ProcessMessages;
    CreateDir('data');
    outputtext('data folder created');
  end;
  // verify if user options file exists
  if not FileExists(CONST_ArchivoUser) then
  begin
    assignfile(FilaUser, CONST_ArchivoUser);
    rewrite(FilaUser);
    FirsTimeUser.ListeningPort := IntToStr(CONST_DefaultServerPort);
    FirsTimeUser.Mining := false;
    FirsTimeUser.Reconnect := true;
    FirsTimeUser.MinimToTray := false;
    FirsTimeUser.AutoConnect := false;
    FirsTimeUser.FullNode := true;
    FirsTimeUser.GetNodes := false;
    FirsTimeUser.ShowMinned := true;
    write(FilaUser, FirsTimeUser);
    OptionsData := FirsTimeUser;
    closefile(FilaUser);
  end
  else
    OptionsData := LoadOptionsFromDisk();
  // Verify if accounts balance summary exist
  if not FileExists(CONST_ArchivoAccData) then
  begin
    assignfile(FilaAccData, CONST_ArchivoAccData);
    rewrite(FilaAccData);
    closefile(FilaAccData);
  end;
  // verify if block folder exists
  if directoryexists(CONST_DIRBLOCKS) = false then
  begin
    CreateDir(CONST_DIRBLOCKS);
    outputtext('BLOCKS folder created');
    CreateBlockZero();
  end;
  InitTime();
  // Verify if blocksummary file exists {to be implemented}
  if not FileExists(CONST_ArchivoBlockSum) then
  begin
    assignfile(FilaSumary, CONST_ArchivoBlockSum);
    rewrite(FilaSumary);
    closefile(FilaSumary);
  end;
  // verify if nodes file exists.
  if not FileExists(CONST_ArchivoNodos) then
  begin
    assignfile(FilaNodos, CONST_ArchivoNodos);
    rewrite(FilaNodos);
    closefile(FilaNodos);
    // add seed nodes here
    SaveNodesToDisk();
    outputtext('Nodes not found. You will need add then manually.');
  end;
  // verify if blacklisted nodes file exists.
  if not FileExists(CONST_Blacklist) then
  begin
    assignfile(FilaBlackList, CONST_Blacklist);
    rewrite(FilaBlackList);
    closefile(FilaBlackList);
    outputtext('Blacklisted nodes file created.');
  end;
  // verify file with my txs
  if not FileExists(CONST_ArchivoMyTxs) then
  begin
    MiFirstTrx := default(TranxData);
    assignfile(FilaMytxs, CONST_ArchivoMyTxs);
    rewrite(FilaMytxs);
    MiFirstTrx.block := '0';
    write(FilaMytxs, MiFirstTrx);
    closefile(FilaMytxs);
    outputtext('User transactions file created.');
  end;
  // in not wallet.dat found, then create a new one
  if not FileExists(CONST_ArchivoWallet) then
    GetNewWallet();
  LoadWalletData();
  result := true;
end;

{*******************************************************************************
                                       NODES
*******************************************************************************}

// RETURNS IF A NODE EXISTS OR NOT
function NodeExists(ip, port: string): boolean;
var
  i: int64 = 0;
begin
  result := false;
  while i < length(arraynodos) do
  begin
    if ((arraynodos[i].ip = ip) and (arraynodos[i].port = port)) then
      result := true;
    i := i + 1;
  end;
end;

// RETURNS IF A BLACKLISTED NODE EXISTS OR NOT
function BLNodeExists(ip, port: string): boolean;
var
  i: int64 = 0;
begin
  result := false;
  while i < length(ArrayBlacklisted) do
  begin
    if ((ArrayBlacklisted[i].ip = ip) and (ArrayBlacklisted[i].port = port)) then
      result := true;
    i := i + 1;
  end;
end;

// LOAD NODES FROM DISK
procedure LoadNodesFromDisk();
var
  DataLeida: nodedata;
  i: int64;
begin
  assignfile(FilaNodos, CONST_ArchivoNodos);
  i := 0;
  reset(FilaNodos);
  SetLength(ArrayNodos, 0);
  SetLength(ArrayNodos, filesize(FilaNodos));
  while i < (filesize(FilaNodos)) do
  begin
    seek(FilaNodos, i);
    read(FilaNodos, DataLeida);
    Arraynodos[i] := dataleida;
    i := i + 1;
  end;
  closefile(FilaNodos);
end;

// LOAD BLACKLISTED NODES FROM DISK
procedure LoadBLNodesFromDisk();
var
  DataLeida: nodedata;
  i: int64;
begin
  assignfile(FilaBlacklist, CONST_Blacklist);
  i := 0;
  reset(FilaBlacklist);
  SetLength(ArrayBlacklisted, 0);
  SetLength(ArrayBlacklisted, filesize(FilaBlacklist));
  while i < (filesize(FilaBlacklist)) do
  begin
    seek(FilaBlacklist, i);
    read(FilaBlacklist, DataLeida);
    ArrayBlacklisted[i] := dataleida;
    i := i + 1;
  end;
  closefile(FilaBlacklist);
end;

// ADD A NEW NODE
procedure AddNewNode(Address, Port: string);
var
  DataLeida: nodedata;
begin
  Dataleida.ip := Address;
  Dataleida.port := Port;
  dataleida.LastAvailable := GetTimeStamp();
  SetLength(arraynodos, Length(arraynodos) + 1);
  arraynodos[length(arraynodos) - 1] := Dataleida;
  U_SaveNodeFile := true;
//*  UpdatesgNodes();
end;

// ADD A NEW BLACKLISTED NODE
procedure AddNewBLNode(Address, Port: string);
var
  DataLeida: nodedata;
begin
  Dataleida.ip := Address;
  Dataleida.port := Port;
  dataleida.LastAvailable := GetTimeStamp();
  SetLength(ArrayBlacklisted, Length(ArrayBlacklisted) + 1);
  ArrayBlacklisted[length(ArrayBlacklisted) - 1] := Dataleida;
  U_SaveBlacklist := true;
//*  UpdatesgBLNodes();
end;

// DELETE AN EXISTING NODE
procedure DeleteExistingNode(Number: int64);
begin
  while Number < length(ArrayNodos) do
  begin
    arraynodos[Number] := arraynodos[Number + 1];
    Number := Number + 1;
  end;
  SetLength(arraynodos, Length(arraynodos) - 1);
  U_SaveNodeFile := true;
//*  UpdatesgNodes();
end;

// DELETE AN EXISTING BLACKLISTED NODE
procedure DeleteBlackListedNode(Number: int64);
begin
  while Number < length(ArrayBlackListed) do
  begin
    ArrayBlackListed[Number] := ArrayBlackListed[Number + 1];
    Number := Number + 1;
  end;
  SetLength(ArrayBlackListed, Length(ArrayBlackListed) - 1);
  U_SaveBlacklist := true;
//*  UpdatesgBLNodes();
end;

// SAVE NODES ARRAY TO DISK
procedure SaveNodesToDisk();
var
  i: int64 = 0;
begin
  assignfile(FilaNodos, CONST_ArchivoNodos);
  rewrite(FilaNodos);
  while i < length(arraynodos) do
  begin
    write(FilaNodos, arraynodos[i]);
    i := i + 1;
  end;
  closefile(FilaNodos);
  U_SaveNodeFile := false;
end;

// SAVE BLACKLISTED NODES ARRAY TO DISK
procedure SaveBLNodesToDisk();
var
  i: int64 = 0;
begin
  assignfile(FilaBlacklist, CONST_Blacklist);
  rewrite(FilaBlacklist);
  while i < length(ArrayBlacklisted) do
  begin
    write(FilaBlacklist, ArrayBlacklisted[i]);
    i := i + 1;
  end;
  closefile(FilaBlacklist);
  U_SaveBlacklist := false;
end;

// DELETE A NODE FROM ITS ADDRESS
procedure DeleteNodeAddress(Address: string);
var
  Resultado: int64 = -1;
  i: integer;
begin
  for i := 0 to length(ArrayNodos) - 1 do
    if ArrayNodos[i].ip = Address then
      Resultado := i;
  if Resultado > -1 then
    DeleteExistingNode(Resultado);
end;

// RETURNS THE REACHABLE NODES
function GetReachableNodes(): int64;
var
  i: integer = 1;
begin
  result := 0;
  for i := 1 to CONST_MAXConections do
    if Conexiones[i].tipo <> '' then
      result := result + Conexiones[i].Connections;
end;

{*******************************************************************************
                                   USER OPTIONS
*******************************************************************************}

// LOAD DEFAULT OPTIONS DATA
function LoadOptionsFromDisk(): UserData;
var
  DataFromFile: UserData;
begin
  assignfile(FilaUser, CONST_ArchivoUser);
  reset(FilaUser);
  read(filauser, DataFromFile);
  closefile(FilaUser);
  result := DataFromFile;
end;

// SAVE TO DISK THE CURRENT OPTIONS DATA
procedure SaveOptionsToDisk();
begin
  assignfile(FilaUser, CONST_ArchivoUser);
  rewrite(FilaUser);
  write(FilaUser, OptionsData);
  closefile(FilaUser);
  U_SaveOptions := false;
end;

{*******************************************************************************
                                   NETWORK DATA
*******************************************************************************}

// UPDATE THE NETWORK BLOCK DATA: LAST AND HASH
function UpdateNetworkLastBlockData(): int64;
var
  i: integer = 1;
  Higher, slot: int64;
begin
  Higher := -1;
  for i := 1 to CONST_MAXConections do
    if StrToIntDef(Conexiones[i].Lastblock, 0) > Higher then
    begin
      Higher := StrToIntDef(Conexiones[i].Lastblock, 0);
      slot := i;
    end;
  NETWORK_LastBLock := Higher;
  NETWORK_LastBlockHash := Conexiones[slot].LastblockHash;
  result := slot;
end;

// UPDATES THE NETWORKS ACCSUM DATA: LAST AND HASH
function UpdateNetworkAccountSumData(): int64;
var
  i: integer = 1;
  Higher, slot: int64;
begin
  Higher := -1;
  for i := 1 to CONST_MAXConections do
    if StrToIntDef(Conexiones[i].Accounts, 0) > Higher then
    begin
      Higher := StrToIntDef(Conexiones[i].Accounts, 0);
      slot := i;
    end;
  NETWORK_LastAccount := Higher;
  NETWORK_AccsumHash := Conexiones[slot].AccountsHash;
  result := slot;
end;

// UPDATES THE NETWORK PENDING TRXS
function UpdateNetworkPendingData(): int64;
var
  i: integer = 1;
  Higher, slot: int64;
begin
  Higher := -1;
  for i := 1 to CONST_MAXConections do
    if StrToIntDef(Conexiones[i].Pending, 0) > Higher then
    begin
      Higher := StrToIntDef(Conexiones[i].Pending, 0);
      slot := i;
    end;
  NETWORK_Pending := Higher;
  result := slot;
end;

{*******************************************************************************
                              ACCOUNT AND ADDRESSES
*******************************************************************************}

// IF the USER OWNS A SPECIFIED ADDRESS RETURNS INDEX OF ArrayMyAddresses, ELSE RETURNS -1
function IsAddressMine(address: string): int64;
var
  i: integer = 0;
begin
  result := -1;
  for i := 0 to length(ArrayMyAddresses) - 1 do
    if ArrayMyAddresses[i].Hash = address then
    begin
      result := i;
      exit;
    end;
end;

// LOAD MY ADDRESSES DATA FROM DISK
procedure LoadWalletData();
var
  ReadData: WalletData;
  WriteData: MyWalletData;
  i: int64 = 0;
begin
  assignfile(FilaWallet, CONST_ArchivoWallet);
  reset(FilaWallet);
  SetLength(ArrayMyAddresses, 0);
  SetLength(ArrayMyAddresses, filesize(FilaWallet));
  while i < (filesize(FilaWallet)) do
  begin
    seek(FilaWallet, i);
    read(FilaWallet, ReadData);
    WriteData.Hash := ReadData.Hash;
    WriteData.PublicKey := ReadData.PublicKey;
    WriteData.PrivateKey := ReadData.PrivateKey;
    WriteData.Balance := IntToStr(GetAddressBalanceFromDisk(ReadData.Hash));
    WriteData.RegisterStatus := 0;
    ArrayMyAddresses[i] := WriteData;
    if i = 0 then
      MAIN_AccountNumber := GetAccountNumberFromAddress(WriteData.Hash);
    i := i + 1;
  end;
  closefile(FilaWallet);
  MAIN_AccountHash := ArrayMyAddresses[0].Hash;
  MAIN_AccountPublic := ArrayMyAddresses[0].PublicKey;
  MAIN_AccountPrivate := ArrayMyAddresses[0].PrivateKey;
  MAIN_AccountBalance := GetTotalAccountBalance();
end;

// SAVE THE ADDRESSES ARRAY TO DISK (Wallet.dat)
procedure SaveAddressesToDisk();
var
  ReadData: MyWalletData;
  WriteData: WalletData;
  i: int64 = 0;
begin
  assignfile(FilaWallet, CONST_ArchivoWallet);
  rewrite(FilaWallet);
  while i < length(ArrayMyAddresses) do
  begin
    ReadData := ArrayMyAddresses[i];
    WriteData.Hash := ReadData.Hash;
    WriteData.PublicKey := ReadData.PublicKey;
    WriteData.PrivateKey := ReadData.PrivateKey;
    write(FilaWallet, WriteData);
    i := i + 1;
  end;
  closefile(FilaWallet);
  U_SaveWallet := false;
end;

// CALCULATES THE TOTAL ACCOUNT BALANCE
function GetTotalAccountBalance(): int64;
var
  i: integer = 0;
  Total: int64 = 0;
begin
  for i := 0 to length(ArrayMyAddresses) - 1 do
    if StrToInt64(ArrayMyAddresses[i].Balance) > -1 then
      Total := Total + StrToInt64(ArrayMyAddresses[i].Balance);
  result := total;
end;

// RETURNS THE TOTAL PENDING PAYMENTS FROM THE ACCOUNT
function GetTotalAccountPendingPayments(): int64;
var
  i: integer = 0;
  Total: int64 = 0;
  ThisAddress: int64 = 0;
begin
  for i := 0 to length(ArrayMyAddresses) - 1 do
  begin
    ThisAddress := 0;
    ThisAddress := GetAddressPaymentsOnPending(ArrayMyAddresses[i].Hash);
    Total := Total + ThisAddress;
  end;
  result := total;
end;

// RETURNS THE AVAILABLE BALANCE IN A ADDRESS
function GetAddressAvailable(Address: string): int64;
var
  Balance: int64;
  Pending: int64;
begin
  if IsAddressMine(Address) >= 0 then
    Balance := StrToInt64(ArrayMyAddresses[IsAddressMine(Address)].Balance)
  else
    Balance := GetAddressBalanceFromDisk(Address);
  Pending := GetAddressPaymentsOnPending(Address);
  result := Balance - Pending;
end;

// RETURNS THE ACCOUNT NUMBER OF A SPECIFIED ADDRESS
function GetAccountNumberFromAddress(address: string): string;
var
  DataRead: AccountData;
  i: int64 = 0;
begin
  result := '-1';
  assignfile(FilaAccData, CONST_ArchivoAccData);
  Reset(FilaAccData);
  while i < filesize(FilaAccData) do
  begin
    seek(FilaAccData, i);
    read(FilaAccData, DataRead);
    if Dataread.Hash = address then
    begin
      result := IntToStr(i);
      Closefile(FilaAccData);
      exit;
    end;
    i := i + 1;
  end;
  Closefile(FilaAccData);
end;

// RETURNS THE ADDRESS  OF A SPECIFIED ACCOUNT NUMBER
function GetAddressFromAccountNumber(Accnumber: int64): string;
var
  DataRead: AccountData;
begin
  result := '';
  assignfile(FilaAccData, CONST_ArchivoAccData);
  Reset(FilaAccData);
  try
    seek(FilaAccData, Accnumber);
    read(FilaAccData, DataRead);
    result := DataRead.Hash;
  except
    on E: Exception do
    begin
      OutPutText('Exception in GetAddressFromAccountNumber');
    end;
  end;
  Closefile(FilaAccData);
end;

// RETURNS THE ADDRESS BALANCE FROM SUMARY; -1 if account is not registered yet
function GetAddressBalanceFromDisk(Account: string): int64;
var
  DataRead: AccountData;
  i: int64 = 0;
begin
  result := -1;
  assignfile(FilaAccData, CONST_ArchivoAccData);
  Reset(FilaAccData);
  while i < filesize(FilaAccData) do
  begin
    seek(FilaAccData, i);
    read(FilaAccData, DataRead);
    if Dataread.Hash = Account then
    begin
      result := StrToInt64(dataread.Balance);
      Closefile(FilaAccData);
      exit;
    end;
    i := i + 1;
  end;
  Closefile(FilaAccData);
end;

// RETURNS THE ADDRESS PUBLIC KEY
function GetAddressPubKey(Address: string): string;
var
  DataRead: AccountData;
  i: int64 = 0;
begin
  result := 'FAIL';
  assignfile(FilaAccData, CONST_ArchivoAccData);
  Reset(FilaAccData);
  while i < filesize(FilaAccData) do
  begin
    seek(FilaAccData, i);
    read(FilaAccData, DataRead);
    if Dataread.Hash = Address then
    begin
      result := DataRead.PublicKey;
      Closefile(FilaAccData);
      exit;
    end;
    i := i + 1;
  end;
  Closefile(FilaAccData);
end;

// CHECK IF MY ADDRESSES ARE REGISTERED; IF SO, AND NO PUBKEY, SEND THE ACRE REQUEST
procedure CheckIfMyAddressesNeedsRegisterFromDisk();
var
  DataRead: AccountData;
  i: int64 = 0;
begin
  assignfile(FilaAccData, CONST_ArchivoAccData);
  Reset(FilaAccData);
  while i < filesize(FilaAccData) do
  begin
    seek(FilaAccData, i);
    read(FilaAccData, DataRead);
    if ((IsAddressMine(Dataread.Hash) > -1) and (Dataread.PublicKey = '')) then
    begin
      ArrayMyAddresses[IsAddressMine(Dataread.Hash)].RegisterStatus := 1;
      OutGoingMessages.Add('{MIC ACRE ' + '$timestamp$' + ' 0.0.0.0 ' +
        ArrayMyAddresses[IsAddressMine(Dataread.Hash)].PublicKey + ' ' +
        ArrayMyAddresses[IsAddressMine(Dataread.Hash)].Hash + ' ' +
        HashMD5String(GetTimeStamp()+'0.0.0.0'+ArrayMyAddresses[IsAddressMine(Dataread.Hash)].PublicKey+ArrayMyAddresses[IsAddressMine(Dataread.Hash)].Hash)+' '+
        GetStringSigned('MY ADDRESS',ArrayMyAddresses[IsAddressMine(Dataread.Hash)].PrivateKey)+
        ' END}');
    end;
    if ((IsAddressMine(Dataread.Hash) > -1) and (Dataread.PublicKey <> '')) then
      ArrayMyAddresses[IsAddressMine(Dataread.Hash)].RegisterStatus := 2;
    i := i + 1;
  end;
  Closefile(FilaAccData);
end;

// CHECK IF ADDRESS EXISTS IN ACCSUM; IF NOT, ADD IT AT THE END
procedure AddAddressIfNotExists(address: string);
var
  DataRead: AccountData;
  i: int64 = 0;
  NewAccount: AccountData;
begin
  assignfile(FilaAccData, CONST_ArchivoAccData);
  Reset(FilaAccData);
  while i < filesize(FilaAccData) do
  begin
    seek(FilaAccData, i);
    read(FilaAccData, DataRead);
    if Dataread.Hash = address then
    begin
      Closefile(FilaAccData);
      exit;
    end;
    i := i + 1;
  end;
  // add the new address
  seek(FilaAccData, filesize(FilaAccData));
  NewAccount := Default(AccountData);
  NewAccount.Number := IntToStr(filesize(FilaAccData));
  NewAccount.PublicKey := '';
  NewAccount.Hash := address;
  NewAccount.Balance := '0';
  NewAccount.Lastop := '0';
  write(FilaAccData, NewAccount);
  Closefile(FilaAccData);
  if IsAddressMine(NewAccount.Hash) > -1 then
  begin
    ArrayMyAddresses[IsAddressMine(NewAccount.Hash)].RegisterStatus := 1;
  end;
  if length(ArrayMyAddresses) > 0 then
    if NewAccount.Hash = ArrayMyAddresses[0].Hash then
      U_MyAccountNumber := true;
end;

// VERIFY IF ANY USER ADDRESS HAVE REGISTER STATUS = 1 AND SEND ACRE
procedure VerifyUserAddressesForAcre();
var
  counter: integer;
begin
  for counter := 0 to length(ArrayMyAddresses) - 1 do
    if ArrayMyAddresses[counter].RegisterStatus = 1 then
      OutGoingMessages.Add('{MIC ACRE ' + '$timestamp$' + ' 0.0.0.0 ' +
        ArrayMyAddresses[counter].PublicKey + ' ' + ArrayMyAddresses[counter].Hash + ' ' +
        HashMD5String(GetTimeStamp()+'0.0.0.0'+ArrayMyAddresses[counter].PublicKey+ArrayMyAddresses[counter].Hash)+' '+
        GetStringSigned('MY ADDRESS',ArrayMyAddresses[counter].PrivateKey)+
        ' END}');
end;

// RETURNS THE NUMBER OF ACCOUNTS IN THE ACCOUNT SUMMARY
function GetLastAccountUpdated(): int64;
begin
  assignfile(FilaAccData, CONST_ArchivoAccData);
  reset(FilaAccData);
  result := filesize(FilaAccData);
  LOCAL_MyLastAccount := filesize(FilaAccData);
  closefile(FilaAccData);
  U_MyLastAccount := false;
end;

// RETURN THE HASH OF MY ACCOUNT SUMMARY
function GetMyAccSumHash(): string;
begin
  LOCAL_MyAccsumHash := HashMD5File(CONST_ArchivoAccData);
  result := LOCAL_MyAccsumHash;
  U_MyAccsumHash := false;
end;

{*******************************************************************************
                             BLOCKS AND BLOCKSUM
*******************************************************************************}

// RETURNS THE LAST DOWNLOADED BLOCK
function GetMyLastUpdatedBlock(): int64;
var
  BlockFiles: TStringList;
  i: int64 = 0;
  LastBlock: int64 = 0;
  OnlyNumbers: string;
begin
  BlockFiles := TStringList.Create;
  try
    try
      FindAllFiles(BlockFiles, CONST_DIRBLOCKS, '*.blk', true);
      while i < BlockFiles.Count do
      begin
        OnlyNumbers := copy(BlockFiles[i], 13, length(BlockFiles[i]) - 16);
        if StrToInt(OnlyNumbers) > Lastblock then
          LastBlock := StrToInt(OnlyNumbers);
        i := i + 1;
      end;
    except
      OutPutText('Error in GetMyLastUpdatedBlock');
    end;
  finally
    BlockFiles.Free;
  end;
  LOCAL_MyLastBlock := LastBlock;
  result := LastBlock;
  U_MylastBlock := false;
end;

// RETURNS THE LAST BLOCK HASH
function GetMyLastBLockHash(Lastblock: string): string;
begin
  try
    LOCAL_LastBlockHash := HashMD5File(CONST_DirBlocks + lastblock + '.blk');
    result := LOCAL_LastBlockHash;
    U_MyLastBLockHash := false;
  except
      OutPutText('Error in GetMyLastBLockHash');
  end;
end;

// RETURNS THE LAST BLOCK BUILD FROM SUMMARY
function BlockSumLastBlock(): int64;
begin
  if length(ArrBlockSummary) > 0 then
    result := StrToInt(ArrBlockSummary[length(ArrBlockSummary) - 1].Number)
  else
    result := 0;
end;

// RETURNS THE BLOCK DATA FROM BLOCKSUM
function GetBlockData(blnumber: int64): BlockSumData;
var
  i: integer = 0;
begin
  for i := 0 to length(ArrBlockSummary) - 1 do
    if StrToInt(ArrBlockSummary[i].Number) = blnumber then
    begin
      result := ArrBlockSummary[i];
      exit;
    end;
end;

// RETURNS THE BLOCK DATA FROM DISK FOR SUMMARY (Headers)
function GetBlockDataFromDisk(BlockNumber: int64): BlockSumData;
var
  Resultado: BlockSumData;
  MemStr: TMemoryStream;
  Header: BlockHeaderData;
  ArchData: string;
begin
  if not OptionsData.FullNode then
  begin
     if BlockNumber < NETWORK_LastBLock - 20 then
       BlockNumber := NETWORK_LastBLock - 20;
  end;
  ArchData := CONST_DIRBLOCKS + IntToStr(BlockNumber) + '.blk';
  MemStr := TMemoryStream.Create;
  try
    try
    MemStr.LoadFromFile(ArchData);
    MemStr.Position := 0;
    MemStr.read(Header, SizeOf(Header));
    except
      OutPutText('Error in GetBlockDataFromDisk');
    end;
  finally
    MemStr.Free;
  end;
  Resultado := Default(BlockSumData);
  Resultado.Number := IntToStr(BlockNumber);
  Resultado.TimeStart := IntToStr(Header.TimeStart);
  Resultado.TimeEnd := IntToStr(Header.TimeEnd);
  Resultado.TimeTot := IntToStr(Header.TimeTot);
  Resultado.TrxTot := IntToStr(Header.TrxTot);
  Resultado.TargetHash := Header.TargetHash;
  Resultado.Difficult := Header.Difficult;
  Resultado.NxtBlkDiff := Header.NxtBlkDiff;
  Resultado.BlockHash := HashMD5File(ArchData);
  Resultado.AccountMiner := Header.AccountMiner;
  Resultado.MinerFee := IntToStr(Header.MinerFee);
  result := resultado;
end;

// BUILD THE BLOCK SUMMARY
procedure BuildBlockSum();
var
  FirstBlock, Arrlen: int64;
  i: integer;
begin
  if LOCAL_MyLastBlock > -1 then
  begin
    SetLength(ArrBlockSummary, 0);
    FirstBlock := LOCAL_MyLastBlock - CONST_BlockSummaryLength + 1;
    if FirstBlock < 0 then
      FirstBlock := 0;
    ArrLen := LOCAL_MyLastBlock - FirstBlock + 1;
    SetLength(ArrBlockSummary, ArrLen);
    for i := 0 to Arrlen - 1 do
      ArrBlockSummary[i] := GetBlockDataFromDisk(FirstBlock + i);
  end;
end;

// ADJUST BLOCKSUM TO MAXIMUN SIZE
procedure AdjustBlockSum();
var
  Number: int64 = 0;
begin
  if length(ArrBlockSummary) > CONST_BlockSummaryLength then
  begin
    while Number < length(ArrBlockSummary) do
    begin
      ArrBlockSummary[Number] := ArrBlockSummary[Number + 1];
      Number := Number + 1;
    end;
    SetLength(ArrBlockSummary, Length(ArrBlockSummary) - 1);
  end;
end;

// returns the last block from where user trxs was extracted
function GetMyLastUpdatedBlockTrxs(): integer;
var
  FirstTrx: TranxData;
begin
  assignfile(FilaMytxs, CONST_ArchivoMyTxs);
  reset(FilaMytxs);
  seek(FilaMytxs, 0);
  read(FilaMytxs, FirstTrx);
  Closefile(FilaMytxs);
  result := StrToInt(FirstTrx.block);
end;

// SET THE LAST CHECKED BLOCK FOR USER TRXS
procedure SetMyLastUpdatedBlockTrxs(Number: integer);
var
  FirstTrx: TranxData;
begin
  FirstTrx := Default(TranxData);
  FirstTrx.block := IntToStr(Number);
  assignfile(FilaMytxs, CONST_ArchivoMyTxs);
  reset(FilaMytxs);
  seek(FilaMytxs, 0);
  write(FilaMytxs, FirstTrx);
  Closefile(FilaMytxs);
end;

// returns an array with all the trxs from a specified block
function GetBlockTrxsFromDisk(BlockNumber: integer): BlockArrTrxsData;
var
  ArrTrxs: BlockArrTrxsData;
  MemStr: TMemoryStream;
  Header: BlockHeaderData;
  ArchData: string;
  counter: integer;
  TotalTrxs: integer;
begin
  Setlength(ArrTrxs, 0);
  ArchData := CONST_DIRBLOCKS + IntToStr(BlockNumber) + '.blk';
  MemStr := TMemoryStream.Create;
  try
  try
    MemStr.LoadFromFile(ArchData);
    MemStr.Position := 0;
    MemStr.read(Header, SizeOf(Header));
    TotalTrxs := header.TrxTot;
    SetLength(ArrTrxs, TotalTrxs);
    for Counter := 0 to TotalTrxs - 1 do
      MemStr.read(ArrTrxs[Counter], Sizeof(ArrTrxs[Counter])); // read each record
  except
    on E: Exception do // nothing, the block is not founded
      OutPutText('Error in GetBlockTrxsFromDisk');
  end;
  finally
    MemStr.Free;
  end;
  result := ArrTrxs;
end;

// SAVE TRANSACTION TO MY TRXS
procedure SaveToMyTrxs(transaction: TranxData);
begin
  assignfile(FilaMytxs, CONST_ArchivoMyTxs);
  reset(FilaMytxs);
  seek(FilaMytxs, filesize(FilaMytxs));
  write(FilaMytxs, transaction);
  Closefile(FilaMytxs);
end;

// BUILT USER TRXS
procedure BuiltMyTrxs();
var
  LastUpdatedBlock: integer;
  counter, counter2: integer;
  BlockArrTrxs: array of TranxData;
  Header: BlockSumData;
  TrxMinned: TranxData;
begin
  LastUpdatedBlock := GetMyLastUpdatedBlockTrxs();
  for counter := LastUpdatedBlock + 1 to LOCAL_MyLastBlock do
  begin
    try
      BlockArrTrxs := Copy(GetBlockTrxsFromDisk(counter));
      for counter2 := 0 to length(BlockArrTrxs) - 1 do
        if BlockArrTrxs[counter2].TypeTx = 'TRFR' then
          if ((IsAddressMine(BlockArrTrxs[counter2].Sender) > -1) or
            (IsAddressmine(BlockArrTrxs[counter2].Receiver) > -1)) then
          begin
            SaveToMyTrxs(BlockArrTrxs[counter2]);
            SavedTrxs := SavedTrxs + 1;
          end;
      application.ProcessMessages;
    finally
    end;
    Header := GetBlockDataFromDisk(counter);
    if IsAddressMine(header.AccountMiner) > -1 then
    begin
      TrxMinned := default(Tranxdata);
      TrxMinned.block := header.Number;
      TrxMinned.TypeTx := 'MINE';
      TrxMinned.TimeStamp := header.TimeEnd;
      TrxMinned.Sender := header.NxtBlkDiff;
      TrxMinned.Receiver := header.AccountMiner;
      TrxMinned.Ammount := IntToStr(StrToInt64(header.MinerFee) + GetBlockReward(StrToInt64(header.Number)));
      TrxMinned.Signature := '';
      TrxMinned.Hash := '';
      TrxMinned.Message := '';
      SaveToMyTrxs(TrxMinned);
      SavedTrxs := SavedTrxs + 1;
    end;
  end;
  if LOCAL_MyLastBlock > LastUpdatedBlock + 1 then
    OutputText('Blocks Rebuilded: ' + IntToStr(LastUpdatedBlock + 1) + ' to ' + IntToStr(LOCAL_MyLastBlock));
  SetMyLastUpdatedBlockTrxs(LOCAL_MyLastBlock);// set last block checked
  U_RebuildMyTxs := false;
end;

{*******************************************************************************
                                   CONNECTIONS
*******************************************************************************}

// RETURN THE NUMBER OF ACTIVE CONNECTIONS (CLIENT OR SERVER)
function GetActiveConex(tipo: string): int64;
var
  i: integer = 1;
begin
  result := 0;
  for i := 1 to CONST_MAXConections do
    if Conexiones[i].tipo = tipo then
      result := result + 1;
end;

// RETURN THE NUMBER OF TOTAL ACTIVE CONEX
function GetTotalConex(): int64;
var
  i: integer = 1;
begin
  result := 0;
  for i := 1 to CONST_MAXConections do
    if Conexiones[i].tipo <> '' then
      result := result + 1;
end;

// SAVE CONECTION TO SLOT
function SaveConection(tipo, ipuser: string; contextdata: TIdContext; clientnumber: string): int64;
var
  i: integer = 1;
  Slot: int64 = 0;
begin
  for i := 1 to CONST_MAXConections do
    if Conexiones[i].tipo = '' then
    begin
      Conexiones[i].Autentic := false;
      Conexiones[i].Connections := 0;
      Conexiones[i].tipo := tipo;
      Conexiones[i].ip := ipuser;
      Conexiones[i].lastping := Gettimestamp();
      Conexiones[i].context := contextdata;
      Conexiones[i].ClientConn := clientnumber;
      Conexiones[i].Lastblock := '0';
      Conexiones[i].Accounts := '0';
      Conexiones[i].Pending := '0';
      Conexiones[i].ListenPort := '0';
      slot := i;
      ReadsFromSlots[slot].Clear;
      break;
    end;
  result := slot;
end;

// CLEAR A CONECTION SLOT
function ClearConection(tipo, ipuser: string): boolean;
var
  i: int64 = 1;
begin
  result := false;
  while i < CONST_MAXConections + 1 do
  begin
    if ((Conexiones[i].tipo = tipo) and (Conexiones[i].ip = ipuser)) then
    begin
      Conexiones[i].Autentic := false;
      Conexiones[i].Connections := 0;
      Conexiones[i].tipo := '';
      Conexiones[i].ip := '';
      Conexiones[i].lastping := '';
      Conexiones[i].ClientConn := '0';
      Conexiones[i].Lastblock := '0';
      Conexiones[i].LastblockHash := '';
      Conexiones[i].Accounts := '0';
      Conexiones[i].AccountsHash := '';
      Conexiones[i].Pending := '0';
      Conexiones[i].ListenPort := '0';
      Conexiones[i].Version := '';
      result := true;
      break;
    end;
    i := i + 1;
  end;
end;

// RETURNS A FREE CONECTION SLOT OR 0 IF NONE
function GetFreeConexSlot(): int64;
var
  i: integer = 1;
begin
  for i := 1 to CONST_MAXConections do
    if Conexiones[i].tipo = '' then
    begin
      result := i;
      exit;
    end;
  result := 0;
end;

// RETURNS A FREE CLIENT OR 0 IF NONE
function GetFreeCliente(): int64;
var
  i: integer = 1;
begin
  for i := 1 to CONST_MaxOutgoingConections do
    if not ConexionesCliente[i].connected() then
    begin
      result := i;
      exit;
    end;
  result := 0;
end;

// CONNECT AS CLIENT
function ConnectClient(Address, Port: string): int64;
var
  freeconex, freecliente: int64;
  ConContext: TIdContext; // EMPTY
begin
  ConContext := Default(TIdContext);
  freeconex := GetFreeConexSlot();
  if freeconex = 0 then
  begin
    result := 0;
    exit;
  end;
  FreeCliente := GetFreeCliente();
  if FreeCliente = 0 then
  begin
    result := 0;
    exit;
  end;
  ConexionesCliente[FreeCliente].Host := Address;
  ConexionesCliente[FreeCliente].Port := StrToInt(Port);
  try
    ConexionesCliente[FreeCliente].ConnectTimeout := 200;
    ConexionesCliente[FreeCliente].Connect;
    SaveConection('server', Address, ConContext, IntToStr(FreeCliente));
    OutputText('Connected TO: ' + Address);
    ConexionesCliente[FreeCliente].IOHandler.writeln('{MIC MINICOIN ' + Address + ' END}');
    ConexionesCliente[FreeCliente].IOHandler.writeln(JoinString());
    if OptionsData.GetNodes then
      ConexionesCliente[FreeCliente].IOHandler.writeln('{MIC GETNODES END}');
    result := FreeConex;
  except
    on E: Exception do
    begin
      OutputText(Address + ': ' + E.Message);
      result := 0;
      exit;
    end;
  end;
end;

// RETURNS IF WE ARE CONNECTED TO A GIVEN IP
function AreWeConnectedTo(address: string): boolean;
var
  i: integer = 1;
begin
  for i := 1 to CONST_MAXConections do
    if Conexiones[i].ip = address then
    begin
      result := true;
      exit;
    end;
  result := false;
end;

// RETURNS THE SLOT OF THE GIVEN IP
function GetSlotFromIP(Ip: string): int64;
var
  i: integer;
begin
  for i := 1 to CONST_MAXConections do
    if conexiones[i].ip = ip then
    begin
      result := i;
      exit;
    end;
  result := 0;
end;

// CONNECT TO SERVERS
procedure ConnectToServers();
var
  i: int64 = 0;
begin
  while i < length(ArrayNodos) do
  begin
    if ((not AreWeConnectedTo(Arraynodos[i].ip)) and (GetActiveConex('server') < CONST_MaxOutgoingConections)) then
      ConnectClient(Arraynodos[i].ip, Arraynodos[i].port);
    i := i + 1;
  end;
  CONNECT_LastTime := StrToInt64(GetTimeStamp());
end;

// CONNECT TO A SPECIFIED NODE
procedure TryConnectToNode(NodeNumber: int64);
begin
  if ((not AreWeConnectedTo(Arraynodos[NodeNumber].ip)) and (GetActiveConex('server') < CONST_MaxOutgoingConections) and
    (Nodenumber < Length(ArrayNodos))) then
    ConnectClient(Arraynodos[NodeNumber].ip, Arraynodos[NodeNumber].port);
  CONNECT_LastTime := StrToInt64(GetTimeStamp());
  CONNECT_LastNode := NodeNumber;
end;

// CLOSE ALL OUTGOING (tipo:server) CONECTIONS
procedure CloseConectionsToServers();
var
  i: integer;
begin
  for i := 1 to CONST_MAXConections do
    if conexiones[i].tipo = 'server' then
    begin
      ConexionesCliente[i].Disconnect;
      OutputText('Disconnected outgoing: ' + conexiones[i].ip);
      ClearConection('server', conexiones[i].ip);
    end;
end;

// ENABLE SERVER
procedure TurnListenOn();
begin
  try
    Form1.IdTCPServer1.Bindings.Clear;
    Form1.IdTCPServer1.DefaultPort := StrToInt(OptionsData.ListeningPort);
    Form1.IdTCPServer1.Active := true;
    OutputText('Server ENABLED. Listening on port ' + OptionsData.ListeningPort);
  except
    on E: Exception do
      OutputText('Unable to start Server');
  end;
end;

// DISABLE SERVER (DISCONNECT ALL INCOMING FIRST)
procedure TurnListenOff();
var
  i: integer;
begin
  for i := 1 to CONST_MAXConections do
    if conexiones[i].tipo = 'client' then
      CloseConnectionSlot(i);
  Form1.IdTCPServer1.Active := false;
  OutputText('Incoming connections disabled');
end;

// CLOSE A GIVEN CONNECTION SLOT AND SENDS A MESSAGE
procedure CloseConnectionSlot(Slot: int64);
begin
  try
  if conexiones[Slot].tipo = 'client' then
  begin
    ReadsFromSlots[slot].Clear;
    Conexiones[Slot].context.Connection.IOHandler.InputBuffer.Clear;
    Conexiones[Slot].context.Connection.Disconnect;
    ClearConection('client', conexiones[Slot].ip);
  end;
  if conexiones[Slot].tipo = 'server' then
  begin
    ReadsFromSlots[slot].Clear;
    ConexionesCliente[Slot].IOHandler.InputBuffer.Clear;
    ConexionesCliente[Slot].Disconnect;
    ClearConection('server', conexiones[Slot].ip);
  end;
  except
    OutPutText('Error in CloseConnectionSlot');
  end;
end;


end. // END MASTER UNIT
