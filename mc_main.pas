unit MC_Main;
{
  MiniCoin Wallet and Miner Copyright (c) 2019 by Preben Bjorn Biermann Madsen
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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, ComCtrls, Grids, IdTCPServer, IdContext, IdTCPClient,
  Master, LCLType, CommandLineParser, Crypto, Protocol,
  Blocks, Clipbrd, dateutils, Timeunit,
  IdComponent, IdTCPConnection, IdBaseComponent, IdGlobal, IdCustomTCPServer,
  UECDSA;

{$I config.inc}
type

  { TForm1 }


  TForm1 = class(TForm)
    btSend: TButton;
    btAddNode: TButton;
    btDelNode: TButton;
    btDelBList: TButton;
    btDetails: TButton;
    btCopy: TButton;
    cbConnecting: TCheckBox;
    cbFullNode: TCheckBox;
    cbMining: TCheckBox;
    cbUpdateNodes: TCheckBox;
    CommandLine: TEdit;
    edAddNodeIP: TEdit;
    edAddNodePort: TEdit;
    edListenPort: TEdit;
    edRecv: TEdit;
    edAmount: TEdit;
    edMsg: TEdit;
    Image1: TImage;
    lbMin: TLabel;
    lbAdrs: TLabel;
    lbMyAdrs: TLabel;
    lbCorAmo: TLabel;
    lbBigBalance: TLabel;
    lbInf: TLabel;
    lbLastBlock: TLabel;
    lbLast20: TLabel;
    lbDiff: TLabel;
    lbListenPort: TLabel;
    LalbNods: TLabel;
    lbBlackL: TLabel;
    lbCons: TLabel;
    lbRec: TLabel;
    lbAmount: TLabel;
    lbMsg: TLabel;
    lbBigAccNum: TLabel;
    lbAccSum: TLabel;
    lbLastPing: TLabel;
    lbPending: TLabel;
    lbBalance: TLabel;
    lbTarget: TLabel;
    lbAccs: TLabel;
    lbListen: TLabel;
    lbMiner: TLabel;
    lbBlocks: TLabel;
    lbConnections: TLabel;
    lbNodes: TLabel;
    lbUserAcc: TLabel;
    MainMemo: TMemo;
    PageControl1: TPageControl;
    pnAppTop: TPanel;
    pnConsole: TPanel;
    pnNetwork: TPanel;
    pnTop: TPanel;
    pnWallet: TPanel;
    StatusBar1: TStatusBar;
    sgPending: TStringGrid;
    sgNodes: TStringGrid;
    sgBLNodes: TStringGrid;
    sgUserTrxs: TStringGrid;
    sgConns: TStringGrid;
    tbConsole: TTabSheet;
    tbNetwork: TTabSheet;
    tbWallet: TTabSheet;
    TimerLoop: TTimer;
    procedure btCopyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerLoopTimer(Sender: TObject);
    procedure CommandLineKeyup(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure edAmountOnChange(Sender: TObject);
    procedure btDelNodeOnClick(Sender: TObject);
    procedure btAddNodeOnClick(Sender: TObject);
    procedure btDelBListOnClick(Sender: TObject);
    procedure btSendOnClick(Sender: TObject);
    procedure btDetailsOnClick(Sender: TObject);
    procedure cbConnectingOnChange(Sender: TObject);
    procedure cbMiningOnChange(Sender: TObject);
    procedure sgPendingPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
    procedure sgUserTrxsPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
    procedure sgConnsPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
    procedure edListenPortOnEditingDone(Sender: TObject);
    procedure cbFullNodeOnChange(Sender: TObject);
    procedure cbUpdateNodesOnChange(Sender: TObject);
  private

  public
    IdTCPServer1: TIdTCPServer;
    procedure IdTCPServer1Connect(AContext: TIdContext);
    procedure IdTCPServer1Disconnect(AContext: TIdContext);
    procedure ReadClientLines(Number, Slot: Int64);
    procedure IdTCPServer1Execute(AContext: TIdContext);
    procedure IdTCPServer1Exception(AContext: TIdContext; AException: Exception);
    // Updates GUI
    procedure UpdateGui();
    procedure UpdatesgNodes();
    procedure UpdatesgBLNodes();
    procedure UpdatesgConns();
    procedure UpdateMyPendingTxs();
    procedure UpdateUserTrxs();
  end;

// IO related
   procedure ShowMsg(Message: string);
   function Int2CurrencyStr(Value: int64): string;
   procedure Outputtext(TextToShow: string; showhour: boolean = true);
   procedure ClearMemoLines();

var
  Form1: TForm1;
  ConexionesCliente: array [1..CONST_MaxOutgoingConections] of TIdTCPClient;
  MinerThread: TMinerThread;

implementation

{$R *.lfm}

// SHOW MESSAGE
Procedure ShowMsg(Message:String);
Begin
  if Message = '' then Message := 'Empty string!';
  ShowMessage(Message);
end;

// SHOWS THE BALANCE AS CURRENCY
function Int2CurrencyStr(Value: int64): string;
begin
  result := Format('%.2n', [Value * 0.01]);
end;

// OUTPUT TEXT TO THE MAINMEMO STRINGLIST
procedure Outputtext(TextToShow: string; showhour: boolean = true);
begin
  if showhour then
    texttoshow := timetostr(now) + ' ' + TextToShow;
  MainMemoStrings.Add(TextToShow);
end;

// CLEARS MEMO LINES
procedure ClearMemoLines();
begin
  form1.MainMemo.Lines.Clear;
  MainMemoStrings.Clear;
end;

{ TForm1 }
procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
begin
  Randomize;
  PendingTXs := TStringList.Create;
  LASTBLOCK_PendingTxs := TStringList.Create;
  LASTBLOCK_TrxsIDs := TStringList.Create;
  MainMemoStrings := TStringList.Create;
  OutGoingMessages := TStringList.Create;
  ProcessLines := TStringList.Create;
  MyPendingTxs := TStringList.Create;

  for i := 1 to CONST_MaxOutgoingConections do
    ConexionesCliente[i] := TIdTCPClient.Create(form1);
  for i := 1 to CONST_MAXConections do
    ReadsFromSlots[i] := TStringList.Create;
  for i := 1 to CONST_MaxOutgoingConections do
  begin
    ConexionesCliente[i] := TIdTCPClient.Create(form1);
    ReadsFromSlots[i] := TStringList.Create;
    ReadsFromSlots[CONST_MaxOutgoingConections+i] := TStringList.Create;
  end;
  //
  IdTCPServer1 := TIdTCPServer.Create(Form1);
  With IdTCPServer1 do
  begin
    Bindings.Add.IP := '0.0.0.0';
    Bindings.Add.Port := CONST_DefaultServerPort;
    DefaultPort:=CONST_DefaultServerPort;
    Active:=false;
    UseNagle:=true;
    TerminateWaitTime:=5000;
    OnExecute:=@IdTCPServer1Execute;
    OnConnect:=@IdTCPServer1Connect;
    OnDisconnect:=@IdTCPServer1Disconnect;
    OnException := @IdTCPServer1Exception;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
//*  KillThread(ThreadID);
  CloseMiningThread();
  FreeAndNil(IdTCPServer1);
  FreeAndNil(PendingTXs);
  FreeAndNil(LASTBLOCK_PendingTxs);
  FreeAndNil(LASTBLOCK_TrxsIDs);
  FreeAndNil(MainMemoStrings);
  FreeAndNil(OutGoingMessages);
  FreeAndNil(ProcessLines);
  FreeAndNil(MyPendingTxs);
end;

// FORM SHOW
procedure TForm1.FormShow(Sender: TObject);
begin
  if MAIN_FirstFormShow then
  begin
    MAIN_FirstFormShow := false;

    if (not ECDSA_Init) then
      halt;
    if not CheckKeys() then
      halt;
    MainMemo.Lines.Add('-----------------------------------------------------------------------------------');
    MainMemo.Lines.Add('MiniCoin Wallet and Miner (c) by PBM, 2019');
    MainMemo.Lines.Add('Version: ' + MAIN_Version);
    MainMemo.Lines.Add('-----------------------------------------------------------------------------------');
    MainMemo.Lines.Add('Type Help+Enter');
    Application.ProcessMessages;

    verifyfiles();
    GetMyLastUpdatedBlock(); // Set LOCAL_MyLastBlock
    GetLastAccountUpdated(); // Set LOCAL_MyLastAccount
    GetMyLastBLockHash(IntToStr(LOCAL_MyLastBlock));    // Set LOCAL_LastBlockHash
    GetMyAccSumHash();      // Set LOCAL_MyAccsumHash
    LoadNodesFromDisk();
    LoadBLNodesFromDisk();
    UpdateGui();
    lbMyAdrs.Caption := ArrayMyAddresses[0].Hash;
    if OptionsData.AutoConnect then
    begin
      cbConnecting.Checked := true;
      TurnListenOn();
      ConnectToServers();
    end;

    cbMining.Checked := OptionsData.Mining;
    cbFullNode.Checked := OptionsData.FullNode;
    cbUpdateNodes.Checked := OptionsData.GetNodes;

    if StrToInt(OptionsData.ListeningPort) > 0 then
      IdTCPServer1.DefaultPort := StrToInt(OptionsData.ListeningPort);

    BuiltMyTrxs();
    UpdateUserTrxs();

    edListenPort.Text := OptionsData.ListeningPort;
    MAIN_ProgramStarted := true;
    TimerLoop.Enabled := true;
  end;
end;

// ON FORM CLOSE
procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  MINER_IsMinerOn := false;
  OutputText('Closing...');
  CloseConectionsToServers();
  Sleep(150);
  TurnListenOff();
end;

procedure TForm1.btCopyClick(Sender: TObject);
begin
  Clipboard.AsText := lbMyAdrs.Caption;
end;

// EXECUTES MAIN TIMER LOOP  //* Perhaps divide this into two timers
procedure TForm1.TimerLoopTimer(Sender: TObject);
var
  i: integer;
begin
  TimerLoop.Enabled := false;

  {updates the GUI}
  UpdateGui();

  while MainMemo.Lines.Count > 100 do MainMemo.Lines.Delete(0);
  while MainMemoStrings.Count > 0 do
  begin
    Mainmemo.Lines.Add(MainMemoStrings[0]);
    MainMemoStrings.Delete(0);
    MainMemo.SelStart := length(MainMemo.Text);
  end;

  {save to disk updated files}
  if U_SaveNodeFile then
    SaveNodesToDisk();
  if U_SaveBlacklist then
    SaveBLNodesToDisk();
  if U_SaveWallet then
    SaveAddressesToDisk();
  if U_SaveOptions then
    SaveOptionsToDisk();
  if U_MylastBlock then
    GetMyLastUpdatedBlock();
  if U_MyLastAccount then
    GetLastAccountUpdated();
  if U_MyLastBLockHash then
    GetMyLastBlockHash(IntToStr(LOCAL_MyLastBlock));
  if U_MyAccsumHash then
    GetMyAccSumHash();
  if U_MyBalance then
  begin
    MAIN_AccountBalance := GetTotalAccountBalance();
    U_MyBalance := false;
  end;
  if U_MyAccountNumber then
  begin
    MAIN_AccountNumber := GetAccountNumberFromAddress(ArrayMyAddresses[0].Hash);
    U_MyAccountNumber := false;
  end;
  if U_RebuildMyTxs then
    BuiltMyTrxs();
  if NETWORK_SendPing then
  begin
    OutGoingMessages.Add(JoinString());
    NETWORK_SendPing := false;
  end;
  {Process the command lines}
  while ProcessLines.Count > 0 do
  begin
    ParseCommandLine(ProcessLines[0]);
    if ProcessLines.Count > 0 then
      ProcessLines.Delete(0);
  end;
  {detect if we are connected or disconnected}
  if ((GetTotalConex >= CONST_MinimunConnectionsToWork) and (not STATUS_Connected)) then
  begin
    OutPutText('CONNECTED');
    STATUS_Connected := true;
    StatusBar1.Panels[2].text := 'Connected';
  end
  else if ((GetTotalConex >= CONST_MinimunConnectionsToWork) and (STATUS_Connected)) then
  begin
    UpdateNetworkLastBlockData(); // SET NETWORK_LastBLock and NETWORK_LastBlockHash
    UpdateNetworkAccountSumData(); // SET NETWORK_LastAccount and NETWORK_AccsumHash
    UpdateNetworkPendingData(); // SET NETWORK_Pending
    StatusBar1.Panels[1].text := IntToStr(GetTotalConex);
  end
  else if ((GetTotalConex < CONST_MinimunConnectionsToWork) and (STATUS_Connected)) then
  begin
    OutPutText('DISCONNECTED');
    StatusBar1.Panels[2].text := 'DisConnected';
    STATUS_Connected := false;
    STATUS_Synzed := false;
    STATUS_Updated := false;
    STATUS_IncomingPings := 0;
    PendingTXs.Clear;
    NETWORK_PENDING := 0;
    NETWORK_LastBLock := 0;
    NETWORK_LastBlockHash := '';
    NETWORK_LastAccount := 0;
    NETWORK_AccsumHash := '';
    MINER_IsMinerOn := false;
    CloseMiningThread();
    StatusBar1.Panels[2].text := 'Disconnected';
  end;
  {detect if we are synzed or not}
  if ((STATUS_IncomingPings >= CONST_PinsToStart) and (not STATUS_Synzed)) then
  begin
    Outputtext('SYNCHRONIZED');
    STATUS_Synzed := true;
    StatusBar1.Panels[3].text := 'Syncronized';
  end;
  if STATUS_Synzed then
  begin
    SendOutGoingMessages();
    if LOCAL_MyLastBlock < NETWORK_LastBLock then
      if STATUS_LastBlockRequested < LOCAL_MyLastBlock + 1 then
      begin
        if OptionsData.FullNode = true then
          i := LOCAL_MyLastBlock
        else
          i := NETWORK_LastBLock - 20;
        if i < LOCAL_MyLastBlock then
          i := LOCAL_MyLastBlock;
        SendPTCMessage(UpdateNetworkLastBlockData(), '{MIC LASTBLOCK ' + IntToStr(i) + ' END}');
        STATUS_LastBlockRequested := LOCAL_MyLastBlock + 1;
      end;
    if LOCAL_MyAccsumHash <> NETWORK_AccsumHash then
      if ((not STATUS_LastAccountRequested) and (not STATUS_Updated) and (LOCAL_MyLastBlock < NETWORK_LastBLock)) then
      begin
        SendPTCMessage(UpdateNetworkAccountSumData(), '{MIC LASTACC ' + IntToStr(LOCAL_MyLastAccount) + ' END}');
        STATUS_LastAccountRequested := true;
        OutputText('Requesting accs data');
      end;
  end;
  if ((STATUS_Synzed) and (not STATUS_Updated) and (LOCAL_MylastBlock = NETWORK_LastBLock) and
    (LOCAL_MyAccsumHash = NETWORK_AccsumHash)) then
  begin
    Outputtext('UPDATED');
    STATUS_Updated := true;
    StatusBar1.Panels[3].text := 'Updated';
    LoadWalletData();
    BuildBlockSum();
    CheckIfMyAddressesNeedsRegisterFromDisk();
  end;
  if STATUS_Updated then
  begin
    if NETWORK_PENDING > PendingTXs.Count then
      SendPTCMessage(UpdateNetworkPendingData(), '{MIC GETPENDING END}');
    if MINER_BlockDiffSet <> BlockSumLastBlock() + 1 then
    begin
      VerifyUserAddressesForAcre();
      CloseMiningThread();
      MINER_IsMinerOn := false;
      MINER_BlockDiffSet := BlockSumLastBlock() + 1;
      MINER_MineDiff := GetBlockData(BlockSumLastBlock()).NxtBlkDiff;
      MINER_TargetChars := GetCharsFromMinerDiff(MINER_MineDiff);
      MINER_Steps := GetStepsFromMinerDiff(MINER_MineDiff);
      MINER_TargetHash := copy(LOCAL_LastBlockHash, 1, MINER_TargetChars); // FOR THE MINER
      MINER_CurrStepTarget := MINER_TargetHash;
      MINER_FoundedSteps := 0;
      MINER_ResultHash := '';
      MINER_HashCounter := 10000000;
      MINER_HashSeed := GetHashSeed(MINER_TargetChars);
    end;
  end;
  // MINER VERIFICATION
  if ((OptionsData.Mining) and (not MINER_IsMinerOn) and (STATUS_Updated) and (not MINER_BlockFound) and
    (LASTBLOCK_Duration > 5) and (MINER_TargetHash = copy(NETWORK_LastBlockHash, 1, MINER_TargetChars))) then
  begin
    OutputText('Mining. Target: ' + MINER_TargetHash + ' (Block: ' + IntToStr(NETWORK_LastBLock + 1) + ')');
    try
      MINER_IsMinerOn := true;
      MinerThread := TMinerThread.Create(True); // It doesn't start automatically
      MinerThread.Start;
    except
      OutPutText('Error in TimerLoop');
    end;
  end
  else if MINER_BlockFound then // BLOCK FOUND, BUILD BLOCK
    if LASTBLOCK_Duration > 5 then
    begin
      if VerifyMinerResult(MINER_ResultHash, MINER_MineDiff, MINER_TargetHash, MINER_BlockDiffSet) > 0 then
      begin
        OutputText('FALSE POSITIVE for ' + MINER_TargetHash + ' : ' + MINER_ResultHash);
        MINER_IsMinerOn := false;
        MINER_BlockFound := false;
        MINER_CurrStepTarget := MINER_TargetHash;
        MINER_FoundedSteps := 0;
        MINER_ResultHash := '';
        MINER_HashCounter := 10000000;
        MINER_HashSeed := GetHashSeed(MINER_TargetChars);
      end
      else
      begin
        OutputText('Block Found ' + IntToStr(NETWORK_LastBLock + 1) + ': ' + MINER_TargetHash + ' -> ' + MINER_ResultHash);
        BuildNewBlock(MINER_BlockDiffSet, GetTimestamp(), MAIN_AccountHash, '', '', '', MINER_MineDiff);
        MINER_IsMinerOn := false;
        MINER_BlockFound := false;
        MINER_CurrStepTarget := MINER_TargetHash;
        MINER_FoundedSteps := 0;
        MINER_ResultHash := '';
        MINER_HashCounter := 10000000;
        MINER_HashSeed := GetHashSeed(MINER_TargetChars);
      end;
    end
    else // Less than 5 second from last block
    begin
      OutputText('Too early to post a block');
      MINER_IsMinerOn := false;
      MINER_BlockFound := false;
      OptionsData.Mining := true;
    end;
  // RUN PERIODICALLY A CONNECTION TO SERVERS
  if strtoint64(GetTimeStamp()) > CONNECT_LastTime + 5000 then
  begin
    CONNECT_LastNode := CONNECT_LastNode + 1;
    if CONNECT_LastNode > length(ArrayNodos) - 1 then
      CONNECT_LastNode := 0;
    if ((length(ArrayNodos) > 0) and (cbConnecting.Checked) and (Optionsdata.Reconnect)) then
      TryConnectToNode(CONNECT_LastNode);
    CONNECT_LastTime := strtoint64(GetTimeStamp);
  end;
  // CHECK ALL CONNECTIONS
  for i := 1 to CONST_MAXConections do
  begin
    if conexiones[i].tipo = 'server' then
    begin
      ReadClientLines(strtoint64(conexiones[i].ClientConn), i);
      if strtoint64(GetTimeStamp()) > strtoint64(conexiones[i].lastping) + 5000 then
        SendPTCMessage(i, JoinString());
    end;
    if conexiones[i].tipo <> '' then
      if ((strtoint64(GetTimeStamp()) > strtoint64(conexiones[i].lastping) + 15000)) then
      begin
        Outputtext('Conection closed: ' + conexiones[i].ip + ' -> Time Out Auth');
        CloseConnectionSlot(i);
      end{close inactive connections after x time};
    if ReadsFromSlots[i].Count > 0 then
      ParseProtocolConnection(ReadsFromSlots[i], i);
  end;
  TimerLoop.Enabled := true;
end;

// SERVER: GET LINE
procedure TForm1.IdTCPServer1Execute(AContext: TIdContext);
var
  LLine: string;
  IPUser: string;
  AFileStream: TFileStream;
  BlockZipName: string;
begin
  try
  IPUser := AContext.Connection.Socket.Binding.PeerIP;
  LLine := AContext.Connection.IOHandler.readln(IndyTextEncoding_UTF8);
  if GetSlotFromIP(IPUser) = 0 then
    exit;
  if LLine = 'FILEACCSUM' then
  begin
    if FileExists(CONST_ArchivoAccData) then
      DeleteFile(CONST_ArchivoAccData);
    AFileStream := TFileStream.Create(CONST_ArchivoAccData, fmCreate);
    AContext.Connection.IOHandler.ReadStream(AFileStream);
    AFileStream.Free;
    U_MyLastAccount := true;
    U_MyAccsumHash := true;
    U_MyBalance := true;
    STATUS_LastAccountRequested := false;
  end
  else if LLine = 'BLOCKZIP' then
  begin
    BlockZipName := CONST_DirBlocks + 'blocks.zip';
    if FileExists(BlockZipName) then
      DeleteFile(BlockZipName);
    AFileStream := TFileStream.Create(BlockZipName, fmCreate);
    AContext.Connection.IOHandler.ReadStream(AFileStream);
    AFileStream.Free;
    U_MyLastBlock := true;
    U_MyLastBLockHash := true;
    UnzipBlockFile(CONST_DirBlocks + 'blocks.zip');
    BuildBlockSum();
    U_RebuildMyTxs := true;
  end
  else
    try
      ReadsFromSlots[GetSlotFromIP(IPUser)].Add(LLine);
    except
      On E: Exception do
        outputtext('Error receiving line: ' + LLine);
    end;
  except
    outputtext('Error in IdTCPServer1Execute');
  end;
end;

// SERVER: CLIENT JOINS
procedure TForm1.IdTCPServer1Connect(AContext: TIdContext);
var
  IPUser: string;
  LLine: string;
begin
  try
  IPUser := AContext.Connection.Socket.Binding.PeerIP;
  LLine := AContext.Connection.IOHandler.readln('', 200, -1, IndyTextEncoding_UTF8);
  if Copy(LLine, 1, 14) <> '{MIC MINICOIN ' then
  begin
    OutputText('INVALID CLIENT : ' + IPUser);
    AContext.Connection.Disconnect;
    Acontext.Connection.IOHandler.InputBuffer.Clear;
    if not BLNodeExists(IPUser, '') then
      AddNewBLNode(IPUser, '');
    exit;
  end
  else
  begin
  {* pbm (temp closed for local testing)
  MAIN_USER_IP := GetParameterFromCommandLine(LLine,2);
  if IPUser = MAIN_USER_IP then
  begin
    OutputText('INCOMING CLOSED: OWN CONNECTION');
    AContext.Connection.Disconnect;
    Acontext.Connection.IOHandler.InputBuffer.Clear;
    if not BLNodeExists(IPUser, '') then
      AddNewBLNode(IPUser, '');
    DeleteNodeAddress(IPUser);
    exit;
  end; }
  end;
  if BLNodeExists(IPUser, '') then
  begin
    OutputText('BLACKLISTED FROM: ' + IPUser);
    AContext.Connection.Disconnect;
    Acontext.Connection.IOHandler.InputBuffer.Clear;
    exit;
  end;
  if AreWeConnectedTo(IPUser) then
  begin
    OutputText('DUPLICATE REJECTED: ' + IPUser);
    AContext.Connection.Disconnect;
    Acontext.Connection.IOHandler.InputBuffer.Clear;
    exit;
  end;
  if SaveConection('client', IPUser, Acontext, '0') = 0 then // ZERO BECAUSE INCOMING
  begin                                                               // NOT USE CLIENT CHANNEL
    AContext.Connection.IOHandler.writeln(GetNodesString);
    AContext.Connection.Disconnect;
    OutputText('Unable to keep conection: ' + IPUser);
    Acontext.Connection.IOHandler.InputBuffer.Clear;
  end
  else
  begin
    OutputText('Connection FROM: ' + IPUser);
    if OptionsData.GetNodes then
      Acontext.Connection.IOHandler.writeln('{MIC GETNODES END}');
  end;
  except
    outputtext('Error in IdTCPServer1Connect');
  end;
end;

// SERVER: CLIENT LEAVE
procedure TForm1.IdTCPServer1Disconnect(AContext: TIdContext);
var
  IPUser: string;
begin
  try
  IPUser := AContext.Connection.Socket.Binding.PeerIP;
  Acontext.Connection.IOHandler.InputBuffer.Clear;
  ClearConection('client', ipuser);
  except
    outputtext('Error in IdTCPServer1DisConnect');
  end;
end;

// EXCEPTION ON SERVER
procedure TForm1.IdTCPServer1Exception(AContext: TIdContext; AException: Exception);
begin
  OutputText('Server Excepcion: ' + AException.Message);
end;

// CLIENTS: GET LINE
procedure TForm1.ReadClientLines(Number, Slot: int64);
var
  LLine: string;
  AFileStream: TFileStream;
  BlockZipName: string;
begin
  try
  if ConexionesCliente[Number].IOHandler.InputBufferIsEmpty then
  begin
    ConexionesCliente[Number].IOHandler.CheckForDataOnSource(10);
    if ConexionesCliente[Number].IOHandler.InputBufferIsEmpty then
      exit;
  end;
  while not ConexionesCliente[Number].IOHandler.InputBufferIsEmpty do
  begin
    LLine := ConexionesCliente[Number].IOHandler.readln(IndyTextEncoding_UTF8);
    if LLine = 'FILEACCSUM' then
    begin
      if FileExists(CONST_ArchivoAccData) then
        DeleteFile(CONST_ArchivoAccData);
      AFileStream := TFileStream.Create(CONST_ArchivoAccData, fmCreate);
      ConexionesCliente[Number].IOHandler.ReadStream(AFileStream);
      AFileStream.Free;
      U_MyLastAccount := true;
      U_MyAccsumHash := true;
      U_MyBalance := true;
      STATUS_LastAccountRequested := false;
    end
    else if LLine = 'BLOCKZIP' then
    begin
      BlockZipName := CONST_DirBlocks + 'blocks.zip';
      if FileExists(BlockZipName) then
        DeleteFile(BlockZipName);
      AFileStream := TFileStream.Create(BlockZipName, fmCreate);
      ConexionesCliente[Number].IOHandler.ReadStream(AFileStream);
      AFileStream.Free;
      U_MyLastBlock := true;
      U_MyLastBLockHash := true;
      UnzipBlockFile(CONST_DirBlocks + 'blocks.zip');
      BuildBlockSum();
      U_RebuildMyTxs := true;
    end
    else
      ReadsFromSlots[Slot].Add(LLine);
  end;
  except
    outputtext('Error in ReadClientLines');
  end;
end;

// CHECKS KEYPRESS ON COMMANDLINE
procedure Tform1.CommandLineKeyup(Sender: TObject; var Key: word; Shift: TShiftState);
var
  LineText: string;
begin
  LineText := commandline.Text;
  if Key = VK_RETURN then
  begin
    CommandLine.Text := '';
    LastCommandline := LineText;
    ProcessLines.add(LineText);
  end;
  if Key = VK_F3 then
  begin
    commandline.Text := LastCommandline;
    CommandLine.SelStart := Length(CommandLine.Text);
  end;
end;

// CHECK KEYPRESS ON AMMOUNT EDIT
procedure Tform1.edAmountOnChange(Sender: TObject);
var
  LineText: string;
  amount: real;
  Available, IntAmount, fee, total: int64;
begin
  Available := GetTotalAccountBalance() - GetTotalAccountPendingPayments();
  LineText := edAmount.Text;
  if linetext = '' then
  begin
    lbCorAmo.font.Color := clBlack;
    lbCorAmo.Caption := '0,00';
    exit;
  end;
  if not IsValidfloat(LineText) then
  begin
    lbCorAmo.Caption := 'Err';
    exit;
  end;
  Amount := StrToFloat(linetext) * 100;
  IntAmount := Round(Amount);
  fee := GetComisionValue(IntAmount);
  Total := IntAmount + fee;
  if total > Available then
    lbCorAmo.Font.Color := clRed
  else
    lbCorAmo.font.Color := clBlack;
  lbCorAmo.Caption := Int2CurrencyStr(IntAmount) + ' + ' + Int2CurrencyStr(fee) + ' (Fee) = ' + Int2CurrencyStr(Total);
end;

// CLICK ON CHECKBOX CONNECT
procedure TForm1.cbConnectingOnChange(Sender: TObject);
begin
  if not MAIN_ProgramStarted then exit;
  if cbConnecting.Checked then
  begin
    ProcessLines.Add('listenon');
    ProcessLines.Add('connect');
    ProcessLines.Add('AUTOCONN ON')
  end
  else
  begin
    ProcessLines.Add('disconnect');
    ProcessLines.Add('listenoff');
    ProcessLines.Add('AUTOCONN OFF');
  end;
end;

// CLICK ON CHECKBOX MINER
procedure TForm1.cbMiningOnChange(Sender: TObject);
begin
  if cbMining.Checked then
    ProcessLines.Add('mineron')
  else
    ProcessLines.Add('mineroff');
end;

// CLICK TO DELETE AN EXISTING NODE
procedure TForm1.btDelNodeOnClick(Sender: TObject);
begin
  if ((sgNodes.Row >= 0) and (length(arraynodos) > 0)) then
    ProcessLines.Add('deletenode ' + IntToStr(sgNodes.Row));
end;

// CLICK PROCESS ADD NEW NODE
procedure TForm1.btAddNodeOnClick(Sender: TObject);
begin
  ProcessLines.Add('addnode ' + edAddnodeIP.Text + ' ' + edAddNodePort.Text);
  edAddnodeIP.Text := '';
  edAddNodePort.Text := '';
end;

// CLICK TO DELETE AN EXISTING BLNODE
procedure TForm1.btDelBListOnClick(Sender: TObject);
begin
  if ((sgBLNodes.Row >= 0) and (length(ArrayBlacklisted) > 0)) then
    ProcessLines.Add('deletebl ' + IntToStr(sgBLNodes.Row));
end;

// CLICK TO SEND FUNDS
procedure TForm1.btSendOnClick(Sender: TObject);    //* test input better
begin
  ProcessLines.Add('SENDTO ' + edRecv.Text + ' ' + edAmount.Text + ' ' + edMsg.Text);
end;

// CLICK TO SHOW TRX DETAILS
procedure TForm1.btDetailsOnClick(Sender: TObject);
var
  DetString, trfrmsg: string;
  trxtipo, amount, blockN, sendera, receiver, trxhash, timestamp, mesg: string;
begin
  if sgUserTrxs.Row > 0 then
  begin
    blockN := sgUserTrxs.Cells[0, sgUserTrxs.Row];
    amount := sgUserTrxs.Cells[1, sgUserTrxs.Row];
    trxhash := sgUserTrxs.Cells[2, sgUserTrxs.Row];
    trxtipo := sgUserTrxs.Cells[3, sgUserTrxs.Row];
    sendera := sgUserTrxs.Cells[4, sgUserTrxs.Row];
    receiver := sgUserTrxs.Cells[5, sgUserTrxs.Row];
    timestamp := sgUserTrxs.Cells[6, sgUserTrxs.Row];
    mesg := sgUserTrxs.Cells[7, sgUserTrxs.Row];
    if trxtipo = 'MINE' then
      DetString := 'Type  : Block Mined' + SLINEBREAK + 'Block : ' + blockN + SLINEBREAK +
        'Reward: ' + amount + SLINEBREAK + 'Time  : ' + TimestampToDate(TimeStamp);
    if trxtipo = 'TRFR' then
    begin
      if IsAddressMine(sendera) > -1 then
        trfrmsg := 'Transfer sent'
      else
        trfrmsg := 'Transfer received';
      DetString := 'Type    : ' + trfrmsg + SLINEBREAK + 'Block   : ' + blockN + SLINEBREAK +
        'Sender  : ' + sendera + SLINEBREAK + 'Receiver: ' + receiver + SLINEBREAK +
        'Amount  : ' + amount + SLINEBREAK + 'Message : ' + mesg + SLINEBREAK +
        'Time    : ' + TimestampToDate(TimeStamp) + SLINEBREAK + 'TrxID   : ' + trxhash;
    end;
    ShowMsg(DetString);
  end;
end;

//DRAW GRIDPENDING
procedure TForm1.sgPendingPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
var
  ts: TTextStyle;
begin
  if ARow = 0 then
    sgPending.Canvas.Font.Style := [fsBold];
  if (aRow = 0) then
  begin
    ts := sgPending.Canvas.TextStyle;
    ts.Alignment := taCenter;
    sgPending.Canvas.TextStyle := ts;
  end;
  if (aRow > 0) then
  begin
    ts := sgPending.Canvas.TextStyle;
    ts.Alignment := taRightJustify;
    sgPending.Canvas.TextStyle := ts;
    if Copy(sgPending.Cells[0, aRow], 1, 1) = '-' then
      sgPending.Canvas.Font.Color := clRed
    else
      sgPending.Canvas.Font.Color := clGreen;
  end;
end;

//DRAW GRID USER TRXS
procedure TForm1.sgUserTrxsPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
var
  ts: TTextStyle;
begin
  if ARow = 0 then
    sgUserTrxs.Canvas.Font.Style := [fsBold];
  if ((aRow = 0) and (aCol = 0)) then
  begin
    ts := sgUserTrxs.Canvas.TextStyle;
    ts.Alignment := taCenter;
    sgUserTrxs.Canvas.TextStyle := ts;
  end;
  if ((aRow = 0) and (aCol = 1)) then
  begin
    ts := sgUserTrxs.Canvas.TextStyle;
    ts.Alignment := taRightJustify;
    sgUserTrxs.Canvas.TextStyle := ts;
  end;
  if ((aRow > 0) and (acol = 1)) then
  begin
    ts := sgUserTrxs.Canvas.TextStyle;
    ts.Alignment := taRightJustify;
    sgUserTrxs.Canvas.TextStyle := ts;
    if Copy(sgUserTrxs.Cells[1, aRow], 1, 1) = '-' then
      sgUserTrxs.Canvas.Font.Color := clRed
    else
      sgUserTrxs.Canvas.Font.Color := clGreen;
  end;
  if ((aRow > 0) and (acol = 0)) then
  begin
    ts := sgUserTrxs.Canvas.TextStyle;
    ts.Alignment := taCenter;
    sgUserTrxs.Canvas.TextStyle := ts;
  end;
end;

// DRAW GRID CONMECIONS
procedure Tform1.sgConnsPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
var
  ts: TTextStyle;
begin
  if ARow = 0 then
    sgConns.Canvas.Font.Style := [fsBold];
  ts := sgConns.Canvas.TextStyle;
  ts.Alignment := taCenter;
  sgConns.Canvas.TextStyle := ts;
end;

// CHANGE USER LISTENING PORT
procedure TForm1.edListenPortOnEditingDone(Sender: TObject);
begin
  if not MAIN_ProgramStarted then
    exit;
  ProcessLines.Add('SETPORT ' + edListenPort.Text);
end;

// CHANGE FULL NODE MODE
procedure TForm1.cbFullNodeOnChange(Sender: TObject);
begin
  if not MAIN_ProgramStarted then
    exit;
  if cbFullNode.Checked then
    ProcessLines.Add('FULLNODE ON')
  else
    ProcessLines.Add('FULLNODE OFF');
end;

// CHANGE UPDATE NODES FROM PEERS
procedure TForm1.cbUpdateNodesOnChange(Sender: TObject);
begin
  if not MAIN_ProgramStarted then
    exit;
  if cbUpdateNodes.Checked then
    ProcessLines.Add('UPDATENODES ON')
  else
    ProcessLines.Add('UPDATENODES OFF');
end;
{
// CHANEG SHOW MINNED AS TRX
procedure TForm1.cbMinnedOnChange(Sender: TObject);
begin
  if not MAIN_ProgramStarted then
    exit;
  if cbMinned.Checked then
    ProcessLines.Add('SHOWMINNED ON')
  else
    ProcessLines.Add('SHOWMINNED OFF');
end;
}
{*******************************************************************************
                                  UPDATES GUI
*******************************************************************************}
 // UPDATES STATUS GUI
procedure TForm1.UpdateGui();
var
  HasesDone: real;
  AccNumberMsg, MinerMsg, TargetMsg, AccSumMsg, LastPingMsg, LastBlockMsg: string;
  MinMsg, DifficultMsg, last20Msg: string;
begin
  if PageControl1.ActivePage = tbConsole then
  begin
    HasesDone := (MINER_HashCounter - MINER_LastHashCounter) / 200000;
    MINER_LastHashCounter := MINER_HashCounter;
    // lb account number
    if StrToInt(MAIN_AccountNumber) > -1 then
      AccNumberMsg := MAIN_AccountNumber
    else
      AccNumberMsg := 'Unknown';
    lbUserAcc.Caption := 'Account   : ' + AccNumberMsg;
    // lb balance
    lbBalance.Caption := 'Balance   : ' + Int2CurrencyStr(GetTotalAccountBalance() -
      GetTotalAccountPendingPayments()) + ' MIC';
    // lb listen
    lbListen.Caption := 'Listening : ' + Booltostr(form1.IdTCPServer1.Active, true) + '(' + OptionsData.ListeningPort + ')';
    // lb connections
    lbConnections.Caption := 'Conections: ' + IntToStr(GetActiveConex('client')) + '/' + IntToStr(GetActiveConex('server'));
    // lb reachable nodes
    lbNodes.Caption := 'ReachNodes: ' + IntToStr(GetReachableNodes);
    // lb block
    lbBlocks.Caption := 'Blocks    : ' + IntToStr(LOCAL_MyLastBlock) + '/' + IntToStr(NETWORK_LastBLock);
    // lb accounts
    lbAccs.Caption := 'Accounts  : ' + IntToStr(LOCAL_MyLastAccount) + '/' + IntToStr(NETWORK_LastAccount);
    // lb pending txs
    lbPending.Caption := 'Pending Tx: ' + IntToStr(PendingTXs.Count) + '/' + IntToStr(NETWORK_Pending);
    // lb difficult
    if STATUS_Updated then
      DifficultMsg := 'Difficult : ' + MINER_MineDiff + '/' + GetDiffForNextBlock(LOCAL_Mylastblock, LASTBLOCK_Duration)
    else
      DifficultMsg := 'Difficult : Unknown';
    lbDiff.Caption := DifficultMsg;
    // lb last20
    if STATUS_Updated then
      last20Msg := 'Last 20   : ' + IntToStr(GetLast20Average(LASTBLOCK_Duration)) + ' sec'
    else
      last20Msg := 'Last 20   : Unknown';
    lbLast20.Caption := last20Msg;
    // lb miner speed
    if HasesDone > 0.0 then
      MinerMsg := Formatfloat('##0.000', HasesDone) + ' MH/s (' + IntToStr(MINER_FoundedSteps) + '/' + IntToStr(MINER_Steps) + ')'
    else
      MinerMsg := 'Off';
    lbMiner.Caption := 'Hash : ' + MinerMsg;
    // lb target hash
    if STATUS_Connected then
      TargetMsg := copy(LOCAL_LastBlockHash, 1, MINER_TargetChars) + '/' + copy(NETWORK_LastBlockHash, 1, MINER_TargetChars)
    else
       TargetMsg := 'Offline';
    lbTarget.Caption := 'Target    : ' + TargetMsg;
    // lb accsumhash
    if STATUS_Connected then
    begin
      if LOCAL_MyAccsumHash = NETWORK_AccsumHash then
        AccSumMsg := 'Correct'
      else
        AccSumMsg := 'Wrong';
    end
    else
        AccSumMsg := 'Offline';
    lbAccsum.Caption := 'AccsumHash: ' + AccSumMsg;
    // lb last ping
    if STATUS_Connected then
      LastPingMsg := IntToStr(StrToInt64(copy(GetTimeStamp(), 1, 10)) - STATUS_LastPing) + ' sec'
    else
      LastPingMsg := 'Offline';
    lbLastPing.Caption := 'Last Ping : ' + LastPingMsg;
    // lb LAST BLOCK DURATION
    if STATUS_Updated then
      LASTBLOCK_Duration := (StrToInt64(GetTImeStamp()) - StrToInt64(GetBlockData(BlockSumLastBlock()).TimeEnd)) div 1000;
    if STATUS_UPDATED then
      LastBlockMsg := IntToStr(LASTBLOCK_Duration) + ' sec'
    else
      LastBlockMsg := 'Unknown';
    lbLastBlock.Caption := 'Last Block: ' + LastBlockMsg;
  end
  else if PageControl1.ActivePage = tbWallet then
  begin
    HasesDone := (MINER_HashCounter - MINER_LastHashCounter) / 200000;
    MINER_LastHashCounter := MINER_HashCounter;

    if StrToInt(MAIN_AccountNumber) > -1 then
      AccNumberMsg := MAIN_AccountNumber
    else
      AccNumberMsg := 'Unknown';
    lbBigAccNum.Caption := 'Account Number : ' + AccNumberMsg;
    // label balance
    lbBigBalance.Caption := Int2CurrencyStr(GetTotalAccountBalance() - GetTotalAccountPendingPayments()) +
      ' MIC';
    // Show user Trxs
    if SavedTrxs > 0 then
    begin
      UpdateUserTrxs();
      SavedTrxs := 0;
    end;

    UpdateMyPendingTxs();

    // lb miner speed
    if HasesDone > 0.0 then
      MinMsg := Formatfloat('##0.000', HasesDone) + ' MH/s (' + IntToStr(MINER_FoundedSteps) + '/' + IntToStr(MINER_Steps) + ')'
    else
      MinMsg := 'Off';
    lbMin.Caption := 'Hash : ' + MinMsg;
  end
  else if PageControl1.ActivePage = tbNetWork then
  begin
    //* perhaps find a better place to do this
    UpdatesgNodes();
    UpdatesgBLNodes();
    UpdatesgConns();
  end;
end;


// UPDATES THE GRID CONTAINING THE NODES
procedure TForm1.UpdatesgNodes();
var
  i: int64 = 0;
begin
  sgNodes.ColCount := 2;
  sgNodes.RowCount := length(arraynodos) + 1;
  if length(arraynodos) > 0 then
  begin
    sgNodes.ColWidths[0] := 100;
    sgNodes.ColWidths[1] := 50;
    while i < length(arraynodos) do
    begin
      sgNodes.Cells[0, i + 1] := arraynodos[i].ip;
      sgNodes.Cells[1, i + 1] := arraynodos[i].port;
      i := i + 1;
    end;
  end;
end;

// UPDATES THE sg CONTAINING THE BLACKLISTED NODES // FIX
procedure TForm1.UpdatesgBLNodes();
var
  i: int64 = 0;
begin
  sgBLNodes.ColCount := 1;
  sgBLNodes.RowCount := length(ArrayBlacklisted) + 1;
  if length(ArrayBlacklisted) > 0 then
  begin
    sgBLNodes.ColWidths[0] := 100;
    while i < length(ArrayBlacklisted) do
    begin
      sgBLNodes.Cells[0, i + 1] := ArrayBlacklisted[i].ip;
      i := i + 1;
    end;
  end;
end;

// UPDATES THE sg CONTAINING THE CONNECTION SLOTS INFO
procedure TForm1.UpdatesgConns();
var
  i: integer;
  ShowClientTipe, ShowLB, ShowLBHash, ShowAccSumHash, ShowVersion, ShowServer, ShowOffset: string;
begin
  sgConns.ColWidths[0] := 20;
  sgConns.ColWidths[1] := 100;
  sgConns.ColWidths[2] := 50;
  sgConns.ColWidths[3] := 30;
  sgConns.ColWidths[4] := 30;
  sgConns.ColWidths[5] := 30;
  sgConns.ColWidths[6] := 40;
  sgConns.ColWidths[7] := 30;
  sgConns.ColWidths[8] := 40;
  for i := 1 to CONST_MAXConections do
  begin
    if Conexiones[i].tipo = '' then
    begin
      ShowClientTipe := 'EMPTY';
      ShowLB := '';
      ShowLBHash := '';
      ShowAccSumHash := '';
      ShowVersion := '';
      ShowServer := '';
      ShowOffset := '';
    end
    else
    begin
      ShowClientTipe := Uppercase(Conexiones[i].tipo);
      if StrToIntDef(Conexiones[i].Lastblock, 0) = LOCAL_MyLastBlock then
        ShowLB := '✔'
      else
        ShowLB := '✘';
      if Conexiones[i].LastblockHash = LOCAL_LastBlockHash then
        ShowLBHash := '✔'
      else
        ShowLBHash := '✘';
      if Conexiones[i].AccountsHash = LOCAL_MyAccsumHash then
        ShowAccSumHash := '✔'
      else
        ShowAccSumHash := '✘';
      ShowVersion := Conexiones[i].Version;
      if Conexiones[i].listening then
        ShowServer := '✔'
      else
        ShowServer := '✘';
      ShowOffset := Conexiones[i].offset;
    end;
    sgConns.Cells[0, i] := IntToStr(i);
    sgConns.Cells[1, i] := Conexiones[i].ip;
    sgConns.Cells[2, i] := ShowClientTipe;
    sgConns.Cells[3, i] := ShowLB;
    sgConns.Cells[4, i] := ShowLBHash;
    sgConns.Cells[5, i] := ShowAccSumHash;
    sgConns.Cells[6, i] := ShowVersion;
    sgConns.Cells[7, i] := ShowServer;
    sgConns.Cells[8, i] := ShowOffset;
  end;
end;

// UPDATES GRID CONTAINING THE USER WALLET PENDING TXS
procedure TForm1.UpdateMyPendingTxs();
var
  i: integer;
  Tipo, Sender, Receiver, Monto: string;
  Ammount: int64;
begin
  MyPendingTxs.Clear;
  for i := 0 to PendingTxs.Count - 1 do
  begin
    Tipo := GetParameterFromCommandLine(PendingTxs[i], 1);
    if tipo = 'TRFR' then
    begin
      Sender := GetParameterFromCommandLine(PendingTxs[i], 3);
      Receiver := GetParameterFromCommandLine(PendingTxs[i], 4);
      Monto := GetParameterFromCommandLine(PendingTxs[i], 5);
      if IsAddressMine(Sender) >= 0 then
        MyPendingTxs.Add('-' + monto);
      if IsAddressMine(Receiver) >= 0 then
        MyPendingTxs.Add('+' + monto);
    end;
  end;
  sgPending.RowCount := 1;
  sgPending.ColCount := 1;
  sgPending.RowCount := MyPendingTxs.Count + 1;
  i := 0;
  if MyPendingTxs.Count > 0 then
  begin
    sgPending.ColWidths[0] := 164;
    while i < MyPendingTxs.Count do
    begin
      Ammount := StrToInt64(MyPendingTxs[i]);
      if Ammount > 0 then
        Ammount := Ammount - GetComisionValue(StrToInt64(MyPendingTxs[i]));
      sgPending.Cells[0, i + 1] := Int2CurrencyStr(Ammount);
      i := i + 1;
    end;
  end;
end;

// SHOW USER TRXS
procedure TForm1.UpdateUserTrxs();
var
  i: integer;
  Registros: integer;
  Transaccion: tranxdata;
  Ammount: int64;
begin
  sgUserTrxs.RowCount := 1;
  sgUserTrxs.ColCount := 8;
  assignfile(FilaMytxs, CONST_ArchivoMyTxs);
  reset(FilaMytxs);
  registros := filesize(FilaMytxs);
  for i := registros - 1 downto 0 do
  begin
    seek(FilaMytxs, i);
    read(FilaMytxs, Transaccion);
    if transaccion.TypeTx = 'TRFR' then
    begin
      if IsAddressMine(transaccion.Sender) >= 0 then
        transaccion.Ammount := '-' + transaccion.Ammount;
      if IsAddressMine(transaccion.Receiver) >= 0 then
        transaccion.Ammount := '+' + transaccion.Ammount;
      if ((IsAddressMine(transaccion.Sender) > -1) or (IsAddressMine(transaccion.Receiver) > -1)) then
      begin
        sgUserTrxs.RowCount := sgUserTrxs.RowCount + 1;
        sgUserTrxs.ColWidths[0] := 50;
        sgUserTrxs.ColWidths[1] := 50;
        sgUserTrxs.ColWidths[2] := 110;
        sgUserTrxs.ColWidths[3] := 1;
        Ammount := StrToInt64(transaccion.Ammount);
        if Ammount < 0 then
          Ammount := Ammount - GetComisionValue(abs(ammount));
        sgUserTrxs.Cells[0, sgUserTrxs.RowCount - 1] := transaccion.block;
        sgUserTrxs.Cells[1, sgUserTrxs.RowCount - 1] := Int2CurrencyStr(Ammount);
        sgUserTrxs.Cells[2, sgUserTrxs.RowCount - 1] := transaccion.hash;
        sgUserTrxs.Cells[3, sgUserTrxs.RowCount - 1] := transaccion.TypeTx;
        sgUserTrxs.Cells[4, sgUserTrxs.RowCount - 1] := transaccion.Sender;
        sgUserTrxs.Cells[5, sgUserTrxs.RowCount - 1] := transaccion.receiver;
        sgUserTrxs.Cells[6, sgUserTrxs.RowCount - 1] := transaccion.TimeStamp;
        sgUserTrxs.Cells[7, sgUserTrxs.RowCount - 1] := transaccion.Message;
      end;
    end;
    if (transaccion.TypeTx = 'MINE') then
    begin
      sgUserTrxs.RowCount := sgUserTrxs.RowCount + 1;
      sgUserTrxs.ColWidths[0] := 50;
      sgUserTrxs.ColWidths[1] := 160;
      Ammount := StrToInt64(transaccion.Ammount);
      sgUserTrxs.Cells[0, sgUserTrxs.RowCount - 1] := transaccion.block;
      sgUserTrxs.Cells[1, sgUserTrxs.RowCount - 1] := Int2CurrencyStr(Ammount);
      sgUserTrxs.Cells[2, sgUserTrxs.RowCount - 1] := transaccion.hash;
      sgUserTrxs.Cells[3, sgUserTrxs.RowCount - 1] := transaccion.TypeTx;
      sgUserTrxs.Cells[4, sgUserTrxs.RowCount - 1] := transaccion.Sender;
      sgUserTrxs.Cells[5, sgUserTrxs.RowCount - 1] := transaccion.receiver;
      sgUserTrxs.Cells[6, sgUserTrxs.RowCount - 1] := transaccion.TimeStamp;
      sgUserTrxs.Cells[7, sgUserTrxs.RowCount - 1] := transaccion.Message;
    end;
  end;
  closefile(FilaMytxs);
end;

end.  // END FORM
