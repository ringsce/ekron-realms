program realms;

{$IFDEF FPC}
  {$MODE ObjFPC}
{$ENDIF}

uses
  SysUtils,
  {$IFDEF WIN32}
  Windows,
  {$ENDIF }
  {$IFDEF LINUX}
  sys_linux in '..\linux\sys_linux.pas',
  {$ENDIF }
  {$IFDEF DARWIN}
  sys_mac in '..\macos\sys_mac.pas',
  {$ENDIF }
  {$IFDEF IOS}
  sys_ios in '..\ios\sys_ios.pas',
  {$ENDIF }
  {$IFDEF ANDROID}
  sys_android in '..\android\sys_android.pas',
  {$ENDIF }
  qfiles in '..\qcommon\qfiles.pas',
  crc in '..\qcommon\crc.pas',
  CPas in '..\qcommon\CPas.pas',
  cmd in '..\qcommon\cmd.pas',
  Common in '..\qcommon\Common.pas',
  adivtab in '..\client\adivtab.pas',
  asm_i386 in '..\client\asm_i386.pas',
  cl_main in '..\client\backup\cl_main.pas',
  keys in '..\client\backup\keys.pas',
  menu in '..\client\backup\menu.pas',
  Client in '..\client\Client.pas',
  cl_cin in '..\client\cl_cin.pas',
  cl_ents in '..\client\cl_ents.pas',
  cl_fx in '..\client\cl_fx.pas',
  cl_input in '..\client\cl_input.pas',
  cl_inv in '..\client\cl_inv.pas',
  GameUnit in '..\ctf\GameUnit.pas',
  g_ai in '..\ctf\g_ai.pas',
  g_chase in '..\ctf\g_chase.pas',
  g_cmds in '..\ctf\g_cmds.pas',
  g_combat in '..\ctf\g_combat.pas',
  g_func in '..\ctf\g_func.pas',
  g_items in '..\ctf\g_items.pas',
  g_local in '..\ctf\g_local.pas',
  g_main in '..\ctf\g_main.pas',
  g_misc in '..\ctf\g_misc.pas',
  g_phys in '..\ctf\g_phys.pas',
  g_save in '..\ctf\g_save.pas',
  g_svcmds in '..\ctf\g_svcmds.pas',
  g_target in '..\ctf\g_target.pas',
  g_trigger in '..\ctf\g_trigger.pas',
  g_utils in '..\ctf\g_utils.pas',
  g_weapon in '..\ctf\g_weapon.pas',
  m_move in '..\ctf\m_move.pas',
  m_player in '..\ctf\m_player.pas',
  p_client in '..\ctf\p_client.pas',
  p_hud in '..\ctf\p_hud.pas',
  p_menu in '..\ctf\p_menu.pas',
  p_view in '..\ctf\p_view.pas',
  p_weapon in '..\ctf\p_weapon.pas',
  q_shared in '..\ctf\q_shared.pas',
  m_flash in '..\game\backup\m_flash.pas',
  sys_null in '..\null\sys_null.pas',
  vid_null in '..\null\vid_null.pas',
  cd_win in '..\win32\cd_win.pas',
  ConProc in '..\win32\ConProc.pas',
  glw_imp in '..\win32\glw_imp.pas',
  glw_win in '..\win32\glw_win.pas',
  in_win in '..\win32\in_win.pas',
  net_wins in '..\win32\net_wins.pas',
  qgl_win in '..\win32\qgl_win.pas',
  q_shwin in '..\win32\q_shwin.pas',
  rw_ddraw in '..\win32\rw_ddraw.pas',
  rw_dib in '..\win32\rw_dib.pas',
  rw_Imp in '..\win32\rw_Imp.pas',
  rw_win in '..\win32\rw_win.pas',
  vid_menu in '..\win32\vid_menu.pas',
  g_local_32 in '..\ctf\g_local_32.pas',
  g_local_64 in '..\ctf\g_local_64.pas';

{$R *.res}

{$IFDEF WIN32}
{$INCLUDE ..\Jedi.inc}
{$ELSE}
{$IFDEF DARWIN}
{$INCLUDE ../macos/Jedi.inc}
{$ELSE}
{$IFDEF IOS}
{$INCLUDE ../ios/Jedi.inc}
{$ELSE}
{$IFDEF ANDROID}
{$INCLUDE ../android/Jedi.inc}
{$ELSE}
{$INCLUDE ../Jedi.inc}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

var
  Saved8087CW: Word;
  CommandLine: String;
  i: Integer;

begin
  {$IFDEF WIN32}
  Saved8087CW := Default8087CW;
  Set8087CW($133f); { Disable all FPU exceptions }
  {$ENDIF}

  CommandLine := '';
  for i := 1 to ParamCount do
    CommandLine := CommandLine + ' ' + ParamStr(i);
  CommandLine := Trim(CommandLine);

  {$IFDEF WIN32}
  WinMain(GetModuleHandle(nil), 0, PChar(CommandLine), SW_SHOWNORMAL);

  {$ELSE}
  {$IFDEF LINUX}
  Main(i, PChar(CommandLine));

  {$ELSE}
  {$IFDEF ANDROID}
  AndroidMain(i, PChar(CommandLine));

  {$ELSE}
  {$IFDEF DARWIN}
  Main(i, PChar(CommandLine)); // macOS-specific entry point

  {$ELSE}
  {$IFDEF IOS}
  IOSMain(i, PChar(CommandLine)); // iOS-specific entry point

  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}

  {$IFDEF WIN32}
  Set8087CW(Saved8087CW);
  {$ENDIF}
end.

