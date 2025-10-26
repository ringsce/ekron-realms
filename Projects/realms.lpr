program realms;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  SysUtils,
  {$IFDEF MSWINDOWS}
    {$IFDEF WIN64}
      sys_win   in '..\sys\win64\sys_win.pas',
      vid_dll   in '..\sys\win64\vid_dll.pas',
      snd_win   in '..\sys\win64\snd_win.pas',
      in_win    in '..\sys\win64\in_win.pas',
      q_shwin   in '..\sys\win64\q_shwin.pas',
      net_wins  in '..\sys\win64\net_wins.pas',
      cd_win    in '..\sys\win64\cd_win.pas',
      vid_menu  in '..\sys\win64\vid_menu.pas',
      ConProc   in '..\sys\win64\ConProc.pas',
    {$ELSE} // Default to Win32
      sys_win   in '..\sys\win32\sys_win.pas',
      vid_dll   in '..\sys\win32\vid_dll.pas',
      snd_win   in '..\sys\win32\snd_win.pas',
      in_win    in '..\sys\win32\in_win.pas',
      q_shwin   in '..\sys\win32\q_shwin.pas',
      net_wins  in '..\sys\win32\net_wins.pas',
      cd_win    in '..\sys\win32\cd_win.pas',
      vid_menu  in '..\sys\win32\vid_menu.pas',
      ConProc   in '..\sys\win32\ConProc.pas',
    {$ENDIF}
  {$ENDIF}
  {$IFDEF LINUX}
  sys_linux   in '../sys/linux/sys_linux.pas',
  vid_so      in '../sys/linux/vid_so.pas',
  snd_sdl     in '../sys/linux/snd_sdl.pas',
  in_linux    in '../sys/linux/in_linux.pas',
  q_shlinux   in '../sys/linux/q_shlinux.pas',
  net_udp     in '../sys/linux/net_udp.pas',
  cd_sdl      in '../sys/linux/cd_sdl.pas',
  vid_menu    in '../sys/linux/vid_menu.pas',
  glob        in '../sys/linux/glob.pas',
  rw_linux_h  in '../sys/linux/rw_linux_h.pas',
  {$ENDIF}
  {$IFDEF DARWIN}
  sys_mac   in  '../sys/mac/sys_mac.pas',
  vid_macos in  '../sys/mac/vid_macos.pas',
  snd_mac   in  '../sys/mac/snd_mac.pas',
  in_mac    in  '../sys/mac/in_mac.pas',
  q_shmac   in  '../sys/mac/q_shmac.pas',
  net_udp   in  '../sys/mac/net_udp.pas',
  cd_sdl    in  '../sys/mac/cd_sdl.pas',
  vid_menu  in  '../sys/mac/vid_menu.pas',
  glob      in  '../sys/mac/glob.pas',
  rw_mac_h  in  '../sys/mac/rw_mac_h.pas',
  MoltenVK  in  '../sys/mac/MoltenVK.pas', // Add MoltenVK unit
{$ENDIF}
{ Vulkan API support }
  Vulkan     in '../game/vulkan/vulkan.pas',
  VulkanUtils in '../game/vulkan/VulkanUtils.pas',
  VulkanRender in '../game/vulkan/VulkanRender.pas',
  kayte in '../engine/kayte/kayte.pas',
  kayteparser in ' ../engine/kayte/kayte_parser.pas',
  platformAsm in '../game/platformasm.pas',
  launcher in '../launcher.pas',

  { Other units }
  qfiles    in '../game/qcommon/qfiles.pas',
  crc       in '..\game\qcommon\crc.pas',
  CPas      in '..\game\qcommon\CPas.pas',
  cmd       in '..\game\qcommon\cmd.pas',
  Common    in '..\game\qcommon\Common.pas',
  CVar      in '..\game\qcommon\CVar.pas',
  //Files     in '..\game\qcommon\Files.pas',
  CModel    in '..\game\qcommon\CModel.pas',
  MD4       in '..\game\qcommon\MD4.pas',
  PMoveUnit in '..\game\qcommon\PMoveUnit.pas',
  net_chan  in '..\game\qcommon\net_chan.pas',
  gzio      in '..\game\qcommon\gzio.pas',
  Delphi_cdecl_printf in '../game/qcommon/Delphi_cdecl_printf.pas',
  q_shared  in '..\game\q_shared.pas',
  m_flash   in '..\game\m_flash.pas',
  GameUnit  in '..\game\GameUnit.pas',
  cl_main   in '..\engine\cl_main.pas',
  Client    in '..\engine\Client.pas',
  ref       in '..\engine\ref.pas',
  menu      in '..\engine\menu.pas',
  Sound_h   in '..\engine\Sound_h.pas',
  Console   in '..\engine\Console.pas',
  cl_scrn   in '..\engine\cl_scrn.pas',
  vid_h     in '..\engine\vid_h.pas',
  keys      in '..\engine\keys.pas',
  snd_loc   in '..\engine\snd_loc.pas',
  cl_input  in '..\engine\cl_input.pas',
  cl_cin    in '..\engine\cl_cin.pas',
  snd_dma   in '..\engine\snd_dma.pas',
  cl_ents   in '..\engine\cl_ents.pas',
  cl_pred   in '..\engine\cl_pred.pas',
  cl_view   in '..\engine\cl_view.pas',
  cl_parse  in '..\engine\cl_parse.pas',
  Qmenu     in '..\engine\Qmenu.pas',
  cl_tent   in '..\engine\cl_tent.pas',
  cl_fx     in '..\engine\cl_fx.pas',
  cl_newfx  in '..\engine\cl_newfx.pas',
  snd_mix   in '..\engine\snd_mix.pas',
  snd_mem   in '..\engine\snd_mem.pas',
  cl_inv    in '..\engine\cl_inv.pas',
  server    in '..\game\server\server.pas',
  sv_game   in '..\game\server\sv_game.pas',
  sv_init   in '..\game\server\sv_init.pas',
  sv_ccmds  in '..\game\server\sv_ccmds.pas',
  Sv_main   in '..\game\server\sv_main.pas',
  sv_send   in '..\game\server\sv_send.pas',
  sv_ents   in '..\game\server\sv_ents.pas',
  sv_user   in '..\game\server\sv_user.pas',
  sv_world  in '..\game\server\sv_world.pas',
  DelphiTypes  in '..\game\qcommon\DelphiTypes.pas',
  q_shared_add in '..\game\q_shared_add.pas',
  game_add     in '..\game\game_add.pas',
  RealmsStats in '../game/server/realmsstats.pas',
  input in '../engine/mac/input.pas';

{$R *.res}

var
  Saved8087CW: Word;
  CommandLine: String;
  i: Integer;
begin
  { Save the current FPU state and then disable FPU exceptions }
  Saved8087CW := Default8087CW;
  Set8087CW($133f); { Disable all fpu exceptions }

  { Build the command line }
  CommandLine := '';
  for i := 1 to ParamCount do
    CommandLine := CommandLine + ' ' + ParamStr(i);
  CommandLine := Trim(CommandLine);

  {$IFDEF MSWINDOWS}
  WinMain(hInstance, hPrevInst, PChar(CommandLine), CmdShow);
  {$ELSE}
  Main(ParamCount, PChar(CommandLine));
  {$ENDIF}

  { Initialize Vulkan with MoltenVK on Apple platforms }
  InitVulkan;

  { Reset the FPU to the previous state }
  Set8087CW(Saved8087CW);
end.
