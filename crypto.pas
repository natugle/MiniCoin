unit Crypto;
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
  Classes, SysUtils, MD5, StrUtils, DateUtils, fileutil, Dialogs, UStrSha256Hex,
  UECDSA;
{$I config.inc}

Type
  TMinerThread = class(TThread)
  private
  protected
    procedure Execute; override;
  public
    Constructor Create(CreateSuspended : boolean);
  end;

function HashMD5File(FileToHash: string): string;
function HashMD5String(StringToHash: string): string;
function HashSha256String(StringToHash: string): string;
function RunMiner(): PtrInt;
procedure CloseMiningThread();
function VerifyMinerResult(Solution, Difficulty, Target: string; block: int64): int64;
function CheckKeys(): boolean;
function CreateKeyPair(): boolean;
function GetPublicKey(): string;
function GetPrivateKey(): string;
function GetHashSeed(longitude: int64): string;
function GetAddressFromPublicKey(PubKey: string): string;
function IsValidAddress(Address: string): boolean;
function GetStringSigned(StringtoSign, PrivateKey: string): string;
function VerifySignedString(StringToVerify, Signatur, PublicKey: string): boolean;

implementation

uses
  MC_Main, Master, Protocol;

constructor TMinerThread.Create(CreateSuspended : boolean);
begin
    inherited Create(CreateSuspended); // because this is black box in OOP and can reset inherited to the opposite again...
    FreeOnTerminate := True;  // better code...
end;

procedure TMinerThread.Execute;
begin
  while (not Terminated) and (not MINER_BlockFound) do
  begin
    try   
      if Miner_isMinerON then RunMiner;
    except
      on E:Exception do
        OutPutText('Error in TMinerThread');
    end;
  end;
end;

// RETURN THE MD5 HASH OF A FILE
function HashMD5File(FileToHash: string): string;
begin
  result := UpperCase(MD5Print(MD5File(FileToHash)));
end;

// RETURNS THE MD5 HASH OF A STRING
function HashMD5String(StringToHash: string): string;
begin
  result := Uppercase(MD5Print(MD5String(StringToHash)));
end;

// RETURNS THE SHA256 OF A STRING
function HashSha256String(StringToHash: string): string;
begin
  result := Uppercase(StrSha256Hex(StringToHash));
end;

// RUNS POW
function RunMiner(): PtrInt;
var
  HashValue: string = '';
  Solution: string = '';
begin
  while MINER_FoundedSteps < MINER_Steps do
    while ((MINER_HashCounter < 99999999) and (OptionsData.Mining) and (STATUS_Updated) and
        (MINER_IsMinerOn) and (not MINER_BlockFound)) do
    begin
      Solution := MINER_HashSeed + IntToStr(MINER_HashCounter);
      HashValue := HashMD5String(Solution);
      if AnsiContainsStr(HashValue, MINER_CurrStepTarget) then // Step founded
      begin
        MINER_ResultHash := MINER_ResultHash + Solution;
        MINER_FoundedSteps := MINER_FoundedSteps + 1;
        if MINER_FoundedSteps = MINER_Steps then
          MINER_BlockFound := true
        else
        begin
          MINER_CurrStepTarget := Copy(HashMD5String(Solution + IntToStr(MINER_BlockDiffSet)), 1, MINER_TargetChars);
          // voy por aqui
          MINER_HashCounter := 10000000;
          MINER_HashSeed := GetHashSeed(MINER_TargetChars);
        end;
      end;
      MINER_HashCounter := MINER_HashCounter + 1;
      if MINER_HashCounter = 99999999 then
      begin
        MINER_HashCounter := 0;
        MINER_HashSeed := GetHashSeed(MINER_TargetChars);
      end;
    end;
  MINER_IsMinerOn := false;
end;

// CLOSE ALL OPENEND MINING THREADS
procedure CloseMiningThread();
begin
  try
    if Assigned(MinerThread) then
    begin
      MINER_IsMinerOn := false;
      MinerThread.Terminate;
    end;
  except
    OutPutText('Error in CloseMiningThread')
  end;
end;

// VERIFY THE BLOCK SOLUTION
function VerifyMinerResult(Solution, Difficulty, Target: string; block: int64): int64;
var
  chars, steps, solutionlength: int64;
  SLSolutions: TStringList;
  i: integer;
  FisrtChar: int64;
  CurrTarget: string;
begin
  result := 0;
  SLSolutions := TStringList.Create;
  chars := GetCharsFromMinerDiff(Difficulty);
  steps := GetStepsFromMinerDiff(Difficulty);
  solutionlength := chars + 8;
  for i := 0 to steps - 1 do
  begin
    FisrtChar := 1 + (i * solutionlength);
    SLSolutions.Add(copy(Solution, FisrtChar, solutionlength));
  end;
  Currtarget := Target;
  for i := 0 to SLSolutions.Count - 1 do
    if AnsiContainsStr(HashMD5String(SLSolutions[i]), currtarget) then  // correct step
      currtarget := copy(HashMD5String(SLSolutions[i] + IntToStr(block)), 1, chars)
    else
    begin
      OutputText('FAILED>>' + Currtarget + ': ' + SLSolutions[i] + '=>' + HashMD5String(SLSolutions[i]), false);
      SLSolutions.Free;
      exit;
    end;
  Outputtext('Solution Verified Block ' + IntToStr(block), false);
  SLSolutions.Free;
end;

// GET Users Password  - fixme - no need for saving keys several places
function CheckKeys(): boolean;
var
  KeyFile: TextFile;
  pk, upw, lt: string;
  lst: TStringList;
begin
  result := false;
  lst := TStringList.Create;
//* tmp for easy testing  FUserPassword := HashSha256String(PasswordBox('MiniCoin', 'Input password:'));
{$IFDEF PORT80}
   FUserPassword := HashSha256String('8080');
{$ELSE}
   FUserPassword := HashSha256String('8081');
{$ENDIF}
  if (not FileExists(CONST_FileKeyDat)) then
    CreateKeyPair()
  else
  begin
    AssignFile(KeyFile, CONST_FileKeyDat);
    try
      reset(Keyfile);
      while not EOF(KeyFile) do
      begin
        readln(KeyFile, lt);
        if AnsiContainsStr(lt, '-----') = false then
          lst.Add(lt);
      end;
      Closefile(Keyfile);
    except
      on E: EInOutError do
        OutPutText(CONST_FileKeyDat + ' file not found');
    end;
    upw := lst[0];
    if (upw <> HashSha256String(FUserPassword)) then
      exit;
    FPrivateKey := lst[1];
    pk := lst[2];
    FreeAndNil(lst);
    if ECDSA_SetKeysFromHex(XorDecode(FUserPassword, FPrivateKey)) then
      FPublicKey := ECDSA_GetPublicKey;
    if (pk <> FPublicKey) then
      exit;
  end;
  if ((FPrivateKey <> '') and (FPublicKey <> '') and (FUserPassword <> '')) then
    result := true;
end;

// CREATES KEY PAIR
function CreateKeyPair(): boolean;
var
  KeyFile: TextFile;
  pk, upwcode: string;
begin
  result := false;
  pk := '';
  upwcode := '';
  FPrivateKey := '';
  FPublicKey := '';
  if FUserPassword = '' then
    exit;
  if directoryexists('data') = false then
    CreateDir('data');
  // Generate new keys
  pk := ECDSA_GenratePrivateKey;
  FPrivateKey := XorEncode(FUserPassword, pk);
  if ECDSA_SetKeysFromHex(XorDecode(FUserPassword, FPrivateKey)) then
    FPublicKey := ECDSA_GetPublicKey
  else
    exit;
  upwcode := HashSha256String(FUserPassword);
  AssignFile(KeyFile, CONST_FileKeyDat);
  try
    rewrite(KeyFile);
    writeln(KeyFile, '-----BEGIN ENCRYPTED WALLET KEYS-----');
    writeln(KeyFile, upwcode);
    writeln(KeyFile, FPrivateKey);
    writeln(KeyFile, FPublicKey);
    writeln(KeyFile, '-----END ENCRYPTED WALLET KEYY-----');
    CloseFile(KeyFile);
  except
    on E: EInOutError do
      OutPutText(CONST_FileKeyDat + ' file not created');
  end;
  if ((FPrivateKey <> '') and (FPublicKey <> '') and (FUserPassword <> '')) then
    result := true;
end;

// RETURNS THE PUBLIC KEY WHEN CREATED
function GetPublicKey(): string;
begin
  result := FPublicKey;
end;

// RETURNS THE PRIVATE KEY WHEN CREATED
function GetPrivateKey(): string;
begin
  result := FPrivateKey;
end;

// RETURN THE HASH SEED FOR THE MINER
function GetHashSeed(longitude: int64): string;
var
  i: integer;
begin
  result := '';
  for i := 1 to longitude do
    result := result + chr(random(26) + 65);
end;

// RETURNS AN ADDRESS FROM A PUBLIC LEY
function GetAddressFromPublicKey(PubKey: string): string;
var
  PubSHAHashed, Hash1, Hash2, KeyChar1, KeyChar2: string;
begin
  PubSHAHashed := HashSha256String(PubKey);
  Hash1 := HashMD5String(PubSHAHashed);
  Hash2 := HashMD5String('K' + Hash1);
  KeyChar1 := Copy(Hash2, 16, 1);
  KeyChar2 := Copy(Hash2, 17, 1);
  result := 'K' + KeyChar1 + Hash1 + KeyChar2;
end;

// RETURNS IF AN ADDRESS IS VALID OR NOT
function IsValidAddress(Address: string): boolean;
var
  OrigHash, NewHash: string;
  KeyChar1, KeyChar2: string;
begin
  OrigHash := Copy(Address, 3, 32);
  NewHash := HashMD5String('K' + OrigHash);
  KeyChar1 := Copy(NewHash, 16, 1);
  KeyChar2 := Copy(NewHash, 17, 1);
  NewHash := 'K' + KeyChar1 + OrigHash + KeyChar2;
  if NewHash = Address then
    result := true
  else
    result := false;
end;

// GET THE HEXASTRING OF A SIGNED STRING
function GetStringSigned(StringtoSign, PrivateKey: string): string;
begin
  result := '';
  if (FUserPassword = '') then
    exit;
  if (not ECDSA_SetKeysFromHex(XorDecode(FUserPassword, PrivateKey))) then
    exit;
  result := ECDSA_Sign(StringtoSign);
end;

// VERIFY IF A SIGNED STRING IS VALID
function VerifySignedString(StringToVerify, Signatur, PublicKey: string): boolean;
begin
  result := false;
  if ECDSA_Verify(PublicKey, StringToVerify, Signatur) = true then
    result := true
  else
    outputtext('Error - Signature is not verified!!!!!!!!!');
end;

end.  // END UNIT
