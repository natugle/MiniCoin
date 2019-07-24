unit UECDSA;

{ Copyright (c) 2018 by Preben Björn Biermann Madsen

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses
  Classes, SysUtils, UCrypto, UOpenSSL;

  function ECDSA_Init: boolean;
  function ECDSA_GenratePrivateKey: ansistring;
  function ECDSA_SetKeysFromHex(pk: ansistring): boolean;
  function ECDSA_GetPublicKey: ansistring;
  function ECDSA_Sign(Str: ansistring): ansistring;
  function ECDSA_Verify(PubKey, Str, Signa: ansistring): boolean;

  var
    FGeneratedPrivateKey: TECPrivateKey;

implementation

function ECDSA_Init: boolean;
begin
  if Not LoadSSLCrypt then raise Exception.Create('Cannot load '+SSL_C_LIB+#10+'To use this software make sure this file is available on you system or reinstall the application');
  TCrypto.InitCrypto;
  result := true;
end;

function ECDSA_GenratePrivateKey: ansistring;
var
  FGenKey: TECPrivateKey;
begin
  Result := '';
  FGenKey := TECPrivateKey.Create;
  FGenKey.GenerateRandomPrivateKey(714);  //* CT_NID_secp256k1 = 714;
  Result := TCrypto.PrivateKey2Hexa(FGenKey.PrivateKey);
  FreeAndNil(FGenKey);
end;

function ECDSA_SetKeysFromHex(pk: ansistring): boolean;
begin
  result := false;
  FGeneratedPrivateKey := TECPrivateKey.Create;
  FGeneratedPrivateKey.SetPrivateKeyFromHexa(714, pk);
  if (Assigned(FGeneratedPrivateKey)) then result := true;
end;

function ECDSA_GetPublicKey: ansistring;
var
  i: integer;
begin
  Result := '';
  if not (Assigned(FGeneratedPrivateKey)) then exit;
  i :=  FGeneratedPrivateKey.PublicKey.EC_OpenSSL_NID;
  Result := TCrypto.ToHexaString(FGeneratedPrivateKey.PublicKey.x) +
              TCrypto.ToHexaString(FGeneratedPrivateKey.PublicKey.y);
end;

function ECDSA_Sign(Str: ansistring): ansistring;
var
  hash: ansistring;
  sign: TECDSA_SIG;
begin
  result := '';
  if not (Assigned(FGeneratedPrivateKey)) then exit;
  hash := TCrypto.ToHexaString(TCrypto.DoRipeMD160(Str));
  sign := TCrypto.ECDSASign(FGeneratedPrivateKey.PrivateKey, hash);
  result := TCrypto.ToHexaString(sign.r) + TCrypto.ToHexaString(sign.s)
end;

function ECDSA_Verify(PubKey, Str, Signa: ansistring): boolean;
var
  hash: ansistring;
  sign: TECDSA_SIG;
  pkey:  TECDSA_Public;
begin
  result := false;
  hash := TCrypto.ToHexaString(TCrypto.DoRipeMD160(Str));
  Sign.r := TCrypto.HexaToRaw(copy(Signa, 1, 64)  );
  Sign.s := TCrypto.HexaToRaw(copy(Signa, 65, 64)  );
  pkey.EC_OpenSSL_NID := 714;
  pkey.x := TCrypto.HexaToRaw(copy(PubKey, 1, 64));
  pkey.y := TCrypto.HexaToRaw(copy(PubKey, 65, 64));
  if TCrypto.ECDSAVerify(pkey, hash , sign) then result := true;
end;

end.
