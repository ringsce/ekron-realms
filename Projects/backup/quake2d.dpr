program quake2d;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  SysUtils,
  {$IFDEF MSWINDOWS}
  sys_win   in '..\win32\sys_win.pas',
  vid_dll   in '..\win32\vid_dll.pas',
  snd_win   in '..\win32\snd_win.pas',
  in_win    in '..\win32\in_win.pas',
  q_shwin   in '..\win32\q_shwin.pas',
  net_wins  in '..\win32\net_wins.pas',
  cd_win    in '..\win32\cd_win.pas',
  vid_menu  in '..\win32\vid_menu.pas',
  ConProc   in '..\win32\ConProc.pas',
  {$ENDIF}
  {$IFDEF LINUX}
  sys_linux   in '../linux/sys_linux.pas',
  vid_so      in '../linux/vid_so.pas',
  snd_sdl     in '../linux/snd_sdl.pas',
  in_linux    in '../linux/in_linux.pas',
  q_shlinux   in '../linux/q_shlinux.pas',
  net_udp     in '../linux/net_udp.pas',
  cd_sdl      in '../linux/cd_sdl.pas',
  vid_menu    in '../linux/vid_menu.pas',
  glob        in '../linux/glob.pas',
  rw_linux_h  in '../linux/rw_linux_h.pas',
  {$ENDIF}
  {$IFDEF DARWIN}
  sys_mac   in '../mac/sys_mac.pas',
  vid_macos in '../mac/vid_macos.pas',
  snd_mac   in '../mac/snd_mac.pas',
  in_mac    in '../mac/in_mac.pas',
  q_shmac   in '../mac/q_shmac.pas',
  net_udp   in '../mac/net_udp.pas',
  cd_sdl    in '../mac/cd_sdl.pas',
  vid_menu  in '../mac/vid_menu.pas',
  glob      in '../mac/glob.pas',
  rw_mac_h  in '../mac/rw_mac_h.pas',
  MoltenVK  in '../mac/MoltenVK.pas', // Add MoltenVK unit
{$ENDIF}
{ Vulkan API support }
  Vulkan     in '../vulkan/vulkan.pas',
  VulkanUtils in '../vulkan/VulkanUtils.pas',
  VulkanRender in '../vulkan/VulkanRender.pas',
  kayte in '../kayte/kayte.pas',
  kayteparser in ' ../kayte/kayteparser.pas',

  { Other units }
  qfiles    in '..\qcommon\qfiles.pas',
  crc       in '..\qcommon\crc.pas',
  CPas      in '..\qcommon\CPas.pas',
  cmd       in '..\qcommon\cmd.pas',
  Common    in '..\qcommon\Common.pas',
  CVar      in '..\qcommon\CVar.pas',
  Files     in '..\qcommon\Files.pas',
  CModel    in '..\qcommon\CModel.pas',
  MD4       in '..\qcommon\MD4.pas',
  PMoveUnit in '..\qcommon\PMoveUnit.pas',
  net_chan  in '..\qcommon\net_chan.pas',
  gzio      in '..\qcommon\gzio.pas',
  Delphi_cdecl_printf in '..\qcommon\Delphi_cdecl_printf.pas',
  q_shared  in '..\game\q_shared.pas',
  m_flash   in '..\game\m_flash.pas',
  GameUnit  in '..\game\GameUnit.pas',
  cl_main   in '..\client\cl_main.pas',
  Client    in '..\client\Client.pas',
  ref       in '..\client\ref.pas',
  menu      in '..\client\menu.pas',
  Sound_h   in '..\client\Sound_h.pas',
  Console   in '..\client\Console.pas',
  cl_scrn   in '..\client\cl_scrn.pas',
  vid_h     in '..\client\vid_h.pas',
  keys      in '..\client\keys.pas',
  snd_loc   in '..\client\snd_loc.pas',
  cl_input  in '..\client\cl_input.pas',
  cl_cin    in '..\client\cl_cin.pas',
  snd_dma   in '..\client\snd_dma.pas',
  cl_ents   in '..\client\cl_ents.pas',
  cl_pred   in '..\client\cl_pred.pas',
  cl_view   in '..\client\cl_view.pas',
  cl_parse  in '..\client\cl_parse.pas',
  Qmenu     in '..\client\Qmenu.pas',
  cl_tent   in '..\client\cl_tent.pas',
  cl_fx     in '..\client\cl_fx.pas',
  cl_newfx  in '..\client\cl_newfx.pas',
  snd_mix   in '..\client\snd_mix.pas',
  snd_mem   in '..\client\snd_mem.pas',
  cl_inv    in '..\client\cl_inv.pas',
  server    in '..\server\server.pas',
  sv_game   in '..\server\sv_game.pas',
  sv_init   in '..\server\sv_init.pas',
  sv_ccmds  in '..\server\sv_ccmds.pas',
  Sv_main   in '..\server\sv_main.pas',
  sv_send   in '..\server\sv_send.pas',
  sv_ents   in '..\server\sv_ents.pas',
  sv_user   in '..\server\sv_user.pas',
  sv_world  in '..\server\sv_world.pas',
  DelphiTypes  in '..\qcommon\DelphiTypes.pas',
  q_shared_add in '..\game\q_shared_add.pas',
  game_add     in '..\game\game_add.pas';

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
