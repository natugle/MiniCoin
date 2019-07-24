unit Blocks;
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
  Classes, SysUtils, Forms, MAster, Crypto, Commandlineparser, Dialogs,
  Fileutil;

{$I config.inc}
function BuildNewBlock(blNumber: int64; TimeStamp, Account, Solution, NewBlHash, TargetHash, difficulty: string): boolean;
procedure SaveNewBlockToDisk(BlockHeader: BlockHeaderData; ArrBlockTxs: array of TranxData; Solution, filename: string);
function GetDiffForNextBlock(block, ThisBlockTime: int64): string;
function DecreaseDiff(minediff: string; variation: int64): string;
function IncreaseDiff(minediff: string; variation: int64): string;
function GetLast20Average(ThisBlockTime: int64): int64;
function GetBlockReward(BlNumber: int64): int64;
procedure AddFundsToAddress(Address: string; Amount: int64; block: int64);
procedure SetAddressPubKey(Address, PubKey: string; block: int64);
function GetNegativeValue(number: int64): int64;
procedure CreateBlockZero();
function UndoneLastBlock(BlNumber: int64 = -1): boolean;

implementation

uses
  MC_Main, Protocol;

// BUILDS A NEW BLOCK
function BuildNewBlock(blNumber: int64; TimeStamp, Account, Solution, NewBlHash, TargetHash, difficulty: string): boolean;
var
  TrxData: TranxData;
  PendingType: string;
  textLine: string;
  StartBlockTime, ThisBlockHash: string;
  PendingCounter: integer;
  TotalOps: int64 = 0;
  MinerFee: int64 = 0;
  ThisTrxFee: int64 = 0;
  TLAddress, TLAmount: string;
  TransfersList: TStringList;
  AcresList: TStringList;
  IgnoredTrxs: TStringList;
  TrxTimeStamp: string;
  BlockHeader: BlockHeaderData;
  ArrBlockTxs: array of TranxData;
  filename: string;
begin
  result := false;
  if Solution = '' then
    Solution := MINER_ResultHash;
  if TargetHash = '' then
    TargetHash := MINER_TargetHash;
  if blNumber = 0 then
    StartBlockTime := '1531896783344'
  else
    StartBlockTime := GetBlockData(BlockSumLastBlock).TimeEnd;
  TotalOps := 0;
  TransfersList := TStringList.Create;
  IgnoredTrxs := TStringList.Create;
  AcresList := TStringList.Create;
  LASTBLOCK_PendingTxs.Clear;
  LASTBLOCK_TrxsIDs.Clear;
  filename := CONST_DIRBLOCKS + IntToStr(BlNumber) + '.blk';
  // Create array with Txs *****
  if PendingTXs.Count > 0 then
  begin
    SetLength(ArrBlockTxs, 0);
    for PendingCounter := 0 to PendingTXs.Count - 1 do
    begin
      TextLine := copy(PendingTXs[PendingCounter], 6, length(PendingTXs[PendingCounter]) - 10);
      {If trx is recent, do not include it in the block *****}
      TrxTimeStamp := GetParameterFromCommandLine(TextLine, 1);
      if StrToInt64(TrxTimeStamp) + 5000 > StrToInt64(TimeStamp) then // TRX is recent so not include in block
      begin
        IgnoredTrxs.Add('{MIC ' + TextLine + ' END}');
        continue;
      end;
      {-----}
      PendingType := GetCommandLineCommand(TextLine);
      if PendingType = 'ACRE' then
      begin
        TotalOps := TotalOps + 1;
        TrxData := Default(TranxData);
        TrxData.block := IntToStr(BlNumber);
        TrxData.TypeTx := 'ACRE';
        TrxData.TimeStamp := GetParameterFromCommandLine(TextLine, 1);
        TrxData.Sender := GetParameterFromCommandLine(TextLine, 2);
        TrxData.Receiver := 'null';
        TrxData.Ammount := GetParameterFromCommandLine(TextLine, 4);
        TrxData.Signature := GetParameterFromCommandLine(TextLine, 3);
        TrxData.hash := GetParameterFromCommandLine(TextLine, 5);
        SetLength(ArrBlockTxs, Length(ArrBlockTxs) + 1);
        ArrBlockTxs[Length(ArrBlockTxs) - 1] := TrxData;
        AcresList.Add('ACRE ' + GetParameterFromCommandLine(TextLine, 4) + ' '
          + GetParameterFromCommandLine(TextLine, 3));
      end;
      if PendingType = 'TRFR' then
      begin
        TotalOps := TotalOps + 1;
        TrxData := Default(TranxData);
        TrxData.block := IntToStr(BlNumber);
        TrxData.TypeTx := 'TRFR';
        TrxData.TimeStamp := GetParameterFromCommandLine(TextLine, 1);
        TrxData.Sender := GetParameterFromCommandLine(TextLine, 2);
        TrxData.Receiver := GetParameterFromCommandLine(TextLine, 3);
        TrxData.Ammount := GetParameterFromCommandLine(TextLine, 4);
        TrxData.Signature := GetParameterFromCommandLine(TextLine, 5);
        TrxData.hash := GetParameterFromCommandLine(TextLine, 6);
        TrxData.Message := GetParameterFromCommandLine(TextLine, 7);
          if StrToInt64(TrxData.Ammount) > CONST_MinFee then ThisTrxFee := GetComisionIncluded(StrToInt64(TrxData.Ammount))
            else ThisTrxFee := 0;
        TrxData.Ammount := IntToStr(StrToInt64(TrxData.Ammount) - ThisTrxFee);
        MinerFee := MinerFee + ThisTrxFee;
        TransfersList.Add('TRFR ' + TrxData.Receiver + ' ' + TrxData.Ammount);
        TransfersList.Add('TRFR ' + TrxData.Sender + ' ' + IntToStr(GetNegativeValue(
          StrToInt64(TrxData.Ammount) + ThisTrxFee)));
        SetLength(ArrBlockTxs, Length(ArrBlockTxs) + 1);
        ArrBlockTxs[Length(ArrBlockTxs) - 1] := TrxData;
        LASTBLOCK_TrxsIDs.Add(TrxData.hash);
      end;
    end;
  end;
  // End of the array with Txs -----
  // Set the block Header *****
  BlockHeader := Default(BlockHeaderData);
  BlockHeader.Number := blNumber;
  BlockHeader.TimeStart := StrToInt64(StartBlockTime);
  BlockHeader.TimeEnd := StrToInt64(TimeStamp);
  BlockHeader.TimeTot := (StrToInt64(TimeStamp) - StrToInt64(StartBlockTime)) div 1000;
  BlockHeader.TrxTot := TotalOps;
  BlockHeader.TargetHash := TargetHash;
  BlockHeader.Difficult := difficulty;
  BlockHeader.NxtBlkDiff := GetDiffForNextBlock(BlNumber, BlockHeader.TimeTot);
  BlockHeader.AccountMiner := Account;
  BlockHeader.MinerFee := MinerFee;
  BlockHeader.Reward := GetBlockReward(BlNumber);
  BlockHeader.SolutionLength := length(Solution);
  // End of the block header -----

  SaveNewBlockToDisk(BlockHeader, ArrBlockTxs, Solution, filename);

  ThisBlockHash := HashMD5File(CONST_DirBlocks + IntToStr(BlNumber) + '.blk');
  if NewBlHash = '' then
  begin
    OutputText('You found block:' + IntToStr(BlNumber) + ' - Hash:' + ThisBlockHash);
  end
  else if ((NewBlHash <> '') and (NewBlHash = ThisBlockHash)) then
    OutputText('Good Block Hash')
  else if ((NewBlHash <> '') and (NewBlHash <> ThisBlockHash)) then
  begin
    // the block hash is wrong so discard it
    OutputText('Wrong Block: ' + IntToStr(BlNumber));
    DeleteFile(filename);
    exit;
  end;

  outputtext(IntToStr(AcresList.Count) + ' Registers - ' + IntToStr(TransfersList.Count div 2) +
    ' Trxs - ' + IntToStr(IgnoredTrxs.Count) + ' Ignored');
  // SAVE VALUES TO REBUILD THE BLOCK IF NECESSARY
  CopyFile(CONST_ArchivoAccData, CONST_ArchivoAccData + '.bak', true);
  CopyFile(CONST_ArchivoMyTxs, CONST_ArchivoMyTxs + '.bak', true);
  LASTBLOCK_ArrBlockSum := copy(ArrBlockSummary);
  LASTBLOCK_ArrMyAddresses := copy(ArrayMyAddresses);
  LASTBLOCK_PendingTxs.AddStrings(PendingTxs);

  // PROCESS ACRES {modify accsum}
  while AcresList.Count > 0 do
  begin
    TLAddress := GetParameterFromCommandLine(AcresList[0], 1);
    TLAmount := GetParameterFromCommandLine(AcresList[0], 2);
    if GetAddressPubKey(TLAddress) = '' then
      SetAddressPubKey(TLAddress, TLAmount, blNumber);
    if IsAddressMine(TLAddress) > -1 then
      ArrayMyAddresses[IsAddressMine(TLAddress)].RegisterStatus := 2;
    if AcresList.Count > 0 then
      AcresList.Delete(0);
  end;

  // PROCESS TRANSFERS {modify accsum & ArrayMyAddresses}
  while TransfersList.Count > 0 do
  begin
    TLAddress := GetParameterFromCommandLine(TransfersList[0], 1);
    TLAmount := GetParameterFromCommandLine(TransfersList[0], 2);
    AddAddressIfNotExists(TLAddress);
    AddFundsToAddress(TLAddress, StrToInt64(TLAmount), BlNumber);
    if IsAddressMine(TLAddress) > -1 then
      ArrayMyAddresses[IsAddressMine(TLAddress)].Balance :=
        IntToStr(StrToInt64(ArrayMyAddresses[IsAddressMine(TLAddress)].Balance) + StrToInt64(TLAmount));
    if TransfersList.Count > 0 then
      TransfersList.Delete(0);
  end;
  // MINER PAYMENT {modify accsum & ArrayMyAddresses}
  AddAddressIfNotExists(Account);
  AddFundsToAddress(Account, GetBlockReward(BlNumber) + MinerFee, blNumber);
  if IsAddressMine(Account) > -1 then
    ArrayMyAddresses[IsAddressMine(Account)].Balance :=
      IntToStr(StrToInt64(ArrayMyAddresses[IsAddressMine(Account)].Balance) + GetBlockReward(BlNumber) + MinerFee);
  // SET UPDATES
  U_MylastBlock := true;
  U_MyLastBLockHash := true;
  U_MyLastAccount := true;
  U_MyAccsumHash := true;
  U_MyBalance := true;
  U_RebuildMyTxs := true;
  PendingTXs.Clear;
  while IgnoredTrxs.Count > 0 do
  begin
    PendingTXs.Add(IgnoredTrxs[0]);
    if IgnoredTrxs.Count > 0 then
      IgnoredTrxs.Delete(0);
  end;
  if BlNumber > 0 then
    OutGoingMessages.Add('{MIC NWBL ' + TimeStamp + ' ' + IntToStr(BlNumber) + ' ' + Account + ' ' +
      Solution + ' ' + Hashmd5file(CONST_DIRBLOCKS + IntToStr(BlNumber) + '.blk') + ' ' + TargetHash + ' ' +
      MINER_MineDiff + ' END}');
  // ADD BLOCKSUMMARY RECORD {modify blocksum}
  SetLength(ArrBlockSummary, length(ArrBlockSummary) + 1);
  ArrBlockSummary[length(ArrBlockSummary) - 1] := GetBlockDataFromDisk(BlNumber);
  AdjustBlockSum();
  // -----
  NETWORK_SendPing := true;
  TransfersList.Free;
  IgnoredTrxs.Free;
  AcresList.Free;
  if LASTBLOCK_UNDONE = blNumber then
    OutPutText('BLOCK ' + IntToStr(blNumber) + ' Rebuilded');
  result := true;
  MINER_IsMinerOn := false;
end;

// SAVE BLOCK TO DISK BINARY
procedure SaveNewBlockToDisk(BlockHeader: BlockHeaderData; ArrBlockTxs: array of TranxData; Solution, filename: string);
var
  MemStr: TMemoryStream;
  ArrRecords, SolLen: int64;
  counter: integer;
begin
  ArrRecords := BlockHeader.TrxTot;
  SolLen := BlockHeader.SolutionLength;
  MemStr := TMemoryStream.Create;
  try
    MemStr.write(BlockHeader, Sizeof(BlockHeader));
    for counter := 0 to ArrRecords - 1 do
      MemStr.write(ArrBlockTxs[counter], Sizeof(ArrBlockTxs[Counter]));
    MemStr.write(Solution[1], SolLen * sizeof(Solution[1]));
    MemStr.SaveToFile(FileName);
  finally
    MemStr.Free;
  end;
end;

// RETURNS THE DIFFICULT FOR NEXT BLOCK
function GetDiffForNextBlock(block, ThisBlockTime: int64): string;
var
  Last20Ave: int64;
  LastDiff: string;
begin
  if block < 20 then
    result := 'fe'
  else
  begin
    Last20Ave := GetLast20Average(ThisBlockTime);
    LastDiff := GetBlockData(BlockSumLastBlock()).NxtBlkDiff;
    if ThisBlockTime > CONST_ExpectedBlockDuration * 1.5 then
    begin
      result := DecreaseDiff(LastDiff, 2);
      exit;
    end;
    if ThisBlockTime < CONST_ExpectedBlockDuration * 0.5 then
    begin
      result := IncreaseDiff(LastDiff, 2);
      exit;
    end;
    if ThisBlockTime > CONST_ExpectedBlockDuration * 1.1 then
    begin
      result := DecreaseDiff(LastDiff, 1);
      exit;
    end;
    if ThisBlockTime < CONST_ExpectedBlockDuration * 0.9 then
    begin
      result := IncreaseDiff(LastDiff, 1);
      exit;
    end;
    if ((ThisBlockTime < CONST_ExpectedBlockDuration * 1.1) and (ThisBlockTime > CONST_ExpectedBlockDuration * 0.9)) then
    begin
      result := LastDiff;
      exit;
    end;
    if Last20Ave > CONST_ExpectedBlockDuration * 1.1 then // HIGH, DECREASE DIFFICULT
      result := DecreaseDiff(LastDiff, 1)
    else if Last20Ave < CONST_ExpectedBlockDuration * 0.9 then // LOW, INCREASE DIFFICULT
      result := IncreaseDiff(LastDiff, 1)
    else
      result := LastDiff;
  end;
end;

// DECREASES THE MINER DIFF
function DecreaseDiff(minediff: string; variation: int64): string;
var
  Lettra, Numero: int64;
  Resultado: string = '';
begin
  Lettra := GetCharsFromMinerDiff(minediff);
  Numero := GetStepsFromMinerDiff(minediff);
  Numero := Numero - variation;
  if Numero < 5 then
  begin
    Numero := 21 + numero;
    Lettra := lettra - 1;
  end;
  Resultado := Resultado + chr(Lettra + 96);
  Resultado := Resultado + chr(numero + 96);
  result := Resultado;
end;

// INCREASES THE MINER DIFF
function IncreaseDiff(minediff: string; variation: int64): string;
var
  Lettra, Numero: int64;
  Resultado: string = '';
begin
  Lettra := GetCharsFromMinerDiff(minediff);
  Numero := GetStepsFromMinerDiff(minediff);
  Numero := Numero + variation;
  if Numero > 25 then
  begin
    Numero := 4 + (numero - 25);
    Lettra := lettra + 1;
  end;
  Resultado := Resultado + chr(Lettra + 96);
  Resultado := Resultado + chr(numero + 96);
  result := Resultado;
end;

// RETURNS THE AVE DURATION OF THE LAST 20 BLOCKS
function GetLast20Average(ThisBlockTime: int64): int64;
var
  i: integer;
  Duration: int64 = 0;
  Divisor: int64;
begin
  if ThisBlockTime > 0 then
    Divisor := length(ArrBlockSummary) + 1
  else
    Divisor := length(ArrBlockSummary);
  for i := 0 to length(ArrBlockSummary) - 1 do
    Duration := Duration + StrToInt64(ArrBlockSummary[i].TimeTot);
  result := (Duration + ThisBlockTime) div (divisor);
end;

// RETURNS THE MINING REWARD FOR A BLOCK
function GetBlockReward(BlNumber: int64): int64;
begin
  result := 10000; //* could be changed
end;

// ADD FUNDS TO A SPECIFIED ADDRESS
procedure AddFundsToAddress(Address: string; Amount: int64; block: int64);
var
  i: integer = 0;
  Dataread, DataWrite: AccountData;
begin
  assignfile(FilaAccData, CONST_ArchivoAccData);
  reset(FilaAccData);
  for i := 0 to filesize(FilaAccData) - 1 do
  begin
    seek(FilaAccData, i);
    read(FilaAccData, Dataread);
    if DataRead.Hash = Address then
    begin
      Datawrite := Default(AccountData);
      Datawrite.Number := Dataread.Number;
      Datawrite.PublicKey := Dataread.PublicKey;
      Datawrite.Hash := DataRead.Hash;
      Datawrite.Balance := IntToStr(StrToInt64(dataread.Balance) + amount);
      Datawrite.Lastop := IntToStr(block);
      seek(FilaAccData, i);
      write(FilaAccData, datawrite);
    end;
  end;
  Closefile(FilaAccData);
end;

// SET ADDRESS PUBLIC KEY (REGISTER)
procedure SetAddressPubKey(Address, PubKey: string; block: int64);
var
  i: integer = 0;
  Dataread, DataWrite: AccountData;
begin
  assignfile(FilaAccData, CONST_ArchivoAccData);
  reset(FilaAccData);
  for i := 0 to filesize(FilaAccData) - 1 do
  begin
    seek(FilaAccData, i);
    read(FilaAccData, Dataread);
    if DataRead.Hash = Address then
    begin
      Datawrite := Default(AccountData);
      Datawrite.Number := Dataread.Number;
      Datawrite.PublicKey := PubKey;
      Datawrite.Hash := DataRead.Hash;
      Datawrite.Balance := DataRead.Balance;
      Datawrite.Lastop := IntToStr(block);
      seek(FilaAccData, i);
      write(FilaAccData, datawrite);
      Closefile(FilaAccData);
      exit;
    end;
  end;
  Closefile(FilaAccData);
end;

// RETURNS THE NEGATIVE VALUE OF AN int64
function GetNegativeValue(number: int64): int64;
begin
  if number > 0 then
    result := number - (Number * 2)
  else
    result := number;
end;

// CREATES THE BLOCK ZERO
procedure CreateBlockZero();
begin
  BuildNewBlock(0, '1531896783344', 'KCE1C52638B5A446AB8AE269AE52908E705', 'NONE', '', 'NONE', 'fe');
end;

// UNDONES THE LASTBLOCK
function UndoneLastBlock(BlNumber: int64 = -1): boolean;
begin
  result := false;
  if not STATUS_Updated then
  begin
    OutPutText('Can Not Undone Last Block if not Updated', false);
    exit;
  end;
  // RESTORE SAVED VALUES
  CopyFile(CONST_ArchivoAccData + '.bak', CONST_ArchivoAccData, true);
  CopyFile(CONST_ArchivoMyTxs + '.bak', CONST_ArchivoMyTxs, true);
  SetMyLastUpdatedBlockTrxs(blnumber - 1);
  ArrBlockSummary := copy(LASTBLOCK_ArrBlockSum);
  ArrayMyAddresses := copy(LASTBLOCK_ArrMyAddresses);
  LASTBLOCK_PendingTxs.AddStrings(PendingTxs);
  LASTBLOCK_PendingTxs.AddStrings(PendingTxs);
  PendingTxs.Clear;
  PendingTxs.AddStrings(LASTBLOCK_PendingTxs);
  Deletefile(CONST_DIRBLOCKS + IntToStr(BlNumber) + '.blk');

  LASTBLOCK_UNDONE := BlNumber;
  OutPutText('BLOCK ' + IntToStr(BlNumber) + ' Undone');
  result := true;
end;

end. // END UNIT
