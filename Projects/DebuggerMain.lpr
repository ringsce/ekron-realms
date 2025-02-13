program DebuggerMainProgram;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, // this includes the LCL
  DebuggerMain; // Include the DebuggerMain unit

{$R *.res}

begin
  RequireDerivedFormResource := True; // Ensure the form resources are loaded
  Application.Scaled:=True;
 // Enable scaling for high-DPI displays
  {$IFDEF WINDOWS}
  Application.MainFormOnTaskbar := True; // Only enable on Windows
  {$ENDIF}
  Application.Initialize; // Initialize the application
  Application.CreateForm(TMainForm, MainForm); // Create the main form
  Application.Run; // Run the application
end.
