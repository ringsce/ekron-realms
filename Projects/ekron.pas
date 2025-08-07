unit ekron;

{$mode delphi}

interface

uses
  Classes, SysUtils, render.renderer, audio.sound;

procedure InitEngine;
procedure ShutdownEngine;
procedure RunFrame;

implementation

procedure InitEngine;
begin
  WriteLn('[Ekron] Initializing engine...');
  InitRenderer;
  InitSound;
end;

procedure ShutdownEngine;
begin
  WriteLn('[Ekron] Shutting down engine...');
  ShutdownSound;
  ShutdownRenderer;
end;

procedure RunFrame;
begin
  UpdateRenderer;
  UpdateSound;
end;

end.

