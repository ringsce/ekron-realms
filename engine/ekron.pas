unit ekron;

{$mode objfpc}{$H+}
{$linklib SDL2}
{$linklib SDL2_image}
{$linklib SDL2_mixer}
{$linklib SDL2_ttf}

interface

uses
  SysUtils, SDL2, SDL2_image, SDL2_mixer, SDL2_ttf,
  input, video, sound, game;

procedure Ekron_Run;

implementation

procedure InitSDL;
begin
  if SDL_Init(SDL_INIT_VIDEO or SDL_INIT_AUDIO or SDL_INIT_EVENTS) < 0 then
  begin
    Writeln('SDL_Init failed: ', SDL_GetError);
    Halt(1);
  end;

  if IMG_Init(IMG_INIT_PNG) = 0 then
  begin
    Writeln('SDL_image Init failed: ', IMG_GetError);
    Halt(1);
  end;

  if Mix_OpenAudio(44100, MIX_DEFAULT_FORMAT, 2, 2048) < 0 then
  begin
    Writeln('SDL_mixer Init failed: ', Mix_GetError);
    Halt(1);
  end;

  if TTF_Init() < 0 then
  begin
    Writeln('SDL_ttf Init failed: ', TTF_GetError);
    Halt(1);
  end;
end;

procedure ShutdownSDL;
begin
  Mix_CloseAudio;
  TTF_Quit;
  IMG_Quit;
  SDL_Quit;
end;

procedure Ekron_Run;
begin
  InitSDL;
  IN_Init;
  VID_Init;
  SND_Init;
  GAME_Init;

  GAME_Run;

  GAME_Shutdown;
  SND_Shutdown;
  VID_Shutdown;
  ShutdownSDL;
end;

end.

