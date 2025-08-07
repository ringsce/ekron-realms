unit net_udp;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Sockets;

type
  TUDPConnection = class
  private
    FSocket: longint;
    FAddress: TInetSockAddr;
  public
    constructor Create(Port: Word);
    destructor Destroy; override;
    function SendTo(const Host: string; Port: Word; const Data: string): Boolean;
    function Receive(var Data: string; Timeout: Integer = 1000): Boolean;
  end;

implementation

constructor TUDPConnection.Create(Port: Word);
begin
  FSocket := fpSocket(AF_INET, SOCK_DGRAM, 0);
  if FSocket < 0 then
    raise Exception.Create('Failed to create socket.');

  FAddress.sin_family := AF_INET;
  FAddress.sin_addr.s_addr := INADDR_ANY;
  FAddress.sin_port := htons(Port);

  if fpBind(FSocket, @FAddress, SizeOf(FAddress)) < 0 then
    raise Exception.Create('Failed to bind socket.');
end;

destructor TUDPConnection.Destroy;
begin
  if FSocket >= 0 then
    fpShutdown(FSocket, 2); // 2 = SHUT_RDWR
  inherited Destroy;
end;

function TUDPConnection.SendTo(const Host: string; Port: Word; const Data: string): Boolean;
var
  Addr: TInetSockAddr;
begin
  Addr.sin_family := AF_INET;
  Addr.sin_addr := StrToNetAddr(Host); // Fixed type issue
  Addr.sin_port := htons(Port);

  Result := fpSendTo(FSocket, PChar(Data), Length(Data), 0, @Addr, SizeOf(Addr)) >= 0;
end;

function TUDPConnection.Receive(var Data: string; Timeout: Integer = 1000): Boolean;
var
  Addr: TInetSockAddr;
  AddrLen: TSockLen; // Fixed type issue
  Buffer: array[0..1023] of Char;
  BytesReceived: Integer;
begin
  fpSetSockOpt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @Timeout, SizeOf(Timeout));

  AddrLen := SizeOf(Addr);
  BytesReceived := fpRecvFrom(FSocket, @Buffer, SizeOf(Buffer), 0, @Addr, @AddrLen); // Fixed pointer issue

  if BytesReceived > 0 then
  begin
    SetString(Data, Buffer, BytesReceived);
    Result := True;
  end
  else
    Result := False;
end;

end.

