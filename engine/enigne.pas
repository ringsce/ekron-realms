{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit enigne;

{$warn 5023 off : no warning about unused units}
interface

uses
  ekron, vid_h, Sound_h, snd_mix, snd_mem, snd_loc, snd_dma, ref, Qmenu, menu, 
  keys, input, Console, Client, cl_view, cl_tent, cl_scrn, cl_pred, cl_parse, 
  cl_newfx, cl_main, cl_inv, cl_input, cl_fx, cl_ents, cl_cin, asm_i386, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('enigne', @Register);
end.
