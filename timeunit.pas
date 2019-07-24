unit TimeUnit;
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
  Classes, SysUtils, dateutils;

{$I config.inc}

procedure InitTime();
function GetTimestamp(): string;
function TimestampToDate(timestamp: string): string;

var
  TimeCorrection: int64 = 0;

implementation

// INITIALIZE TIME
procedure InitTime();
var
  Offset: int64;
begin
  //* 2209161600 Convert to UnixTime
  Offset := GetLocalTimeOffset*60;
  TimeCorrection := (2209161600 + (-1*Offset))*1000;
end;

// RETURNS EXTENDED UNIX TIMESTAMP
function GetTimestamp(): string;
begin
   result := IntToStr(Trunc(now*86400000)-TimeCorrection);
end;

// RETURNS A TIMESTAMP AS A HUMAN READABLE DATE
function TimestampToDate(timestamp: string): string;
begin
  result := DateTimeToStr(UnixToDateTime(StrToInt64(copy(timestamp, 1, 10))));
end;

end.
// END UNIT
