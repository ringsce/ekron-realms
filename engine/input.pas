unit input;

{$mode objfpc}{$H+}

interface

uses
  SDL2,     // For SDL_Event, SDL_GetMouseState, etc.
  SysUtils, // For basic system utilities if needed (e.g. debugging)
  Common,   // For Com_Printf (assuming it's in Common.pas)
  cvar,     // For Cvar_Get, Cvar_Set, etc.
  keys;     // For K_*, Key_Event, Key_SetCatcher, Key_ClearStates (already used)

// Forward declarations for input system functions
procedure IN_Init;
procedure IN_Shutdown;
procedure IN_Frame; // Called every frame to process input events
procedure IN_Activate(active: Boolean);
procedure IN_MouseMove(dx, dy: Integer); // For raw mouse movement
procedure IN_MouseEvent(button, state: Integer); // For mouse button events (e.g., SDL_BUTTON_LEFT, SDL_PRESSED/SDL_RELEASED)
procedure IN_JoyMove(axis, value: Integer); // For joystick axis events
procedure IN_JoyButtons(button, state: Integer); // For joystick button events

var
  // Input system console variables (CVars)
  in_mouse: cvar_p;
  in_joystick: cvar_p;
  // Add more as needed: cl_mouse_xscale, cl_mouse_yscale, sensitivity, etc.

implementation

// Global variables for internal input state (optional, depending on design)
var
  InputInitialized: Boolean = False;

procedure IN_Init;
begin
  if InputInitialized then Exit;

  //Com_Printf('------- Input Initialization -------'#10);
  // try on something else

  // Register input CVars
  in_mouse := Cvar_Get('in_mouse', '1', CVAR_ARCHIVE);
  in_joystick := Cvar_Get('in_joystick', '0', CVAR_ARCHIVE);
  // Cvar_Get('sensitivity', '3', CVAR_ARCHIVE); // Example
  // Cvar_Get('m_yaw', '0.022', CVAR_ARCHIVE); // Example
  // Cvar_Get('m_pitch', '0.022', CVAR_ARCHIVE); // Example

  // Initialize SDL input subsystems if not already done by SDL_Init(SDL_INIT_EVERYTHING)
  // SDL_InitSubSystem(SDL_INIT_JOYSTICK or SDL_INIT_GAMECONTROLLER or SDL_INIT_HAPTIC);

  InputInitialized := True;
  //Com_Printf('------------------------------------'#10); try on something else
end;

procedure IN_Shutdown;
begin
  if not InputInitialized then Exit;

  //Com_Printf('------- Input Shutdown -------'#10);
  // SDL_QuitSubSystem(SDL_INIT_JOYSTICK or SDL_INIT_GAMECONTROLLER or SDL_INIT_HAPTIC);
  InputInitialized := False;
  //Com_Printf('------------------------------------'#10);
end;

procedure IN_Frame;
// This procedure would typically be called once per game frame
// to process all accumulated input events from SDL.
// However, in Quake 2, input events are often processed as they arrive
// via SDL's event loop (e.g., in Quake2Main.lpr's main loop).
// This might be a placeholder or for polling-based input.
begin
  // Example: Check for mouse state if not using relative mode or event-driven
  // var
  //   x, y: Integer;
  // begin
  //   if in_mouse^.value <> 0 then
  //   begin
  //     SDL_GetMouseState(x, y);
  //     // Process x, y changes if not in relative mode
  //   end;
  // end;
end;

procedure IN_Activate(active: Boolean);
// This function activates or deactivates in-game input handling.
// Used by AppActivate in vid_macos.pas.
begin
  if active then
  begin
    //Com_Printf(PRINT_DEVELOPER, 'IN_Activate: Input Activated'#10);
    // Potentially re-enable specific input devices or polling here
    SDL_SetHint(SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS, '1'); // Example
  end
  else
  begin
    //Com_Printf(PRINT_DEVELOPER, 'IN_Activate: Input Deactivated'#10);
    // Potentially disable input or clear states
    Key_ClearStates; // Important to prevent "stuck" keys
  end;
end;

procedure IN_MouseMove(dx, dy: Integer);
// This would be called by your SDL event loop when SDL_MOUSEMOTION events occur.
// It translates raw mouse delta into game input.
begin
  // Example: Pass mouse movement to the client module
  // cl.m_yaw := cl.m_yaw + dx * cvar.sensitivity;
  // cl.m_pitch := cl.m_pitch + dy * cvar.sensitivity;
  // You'd need a cl_input.pas or similar to handle this.
end;

procedure IN_MouseEvent(button, state: Integer);
// This would be called by your SDL event loop when SDL_MOUSEBUTTONDOWN/UP events occur.
begin
  // Example: Translate SDL mouse button to Quake key and send event
  // case button of
  //   SDL_BUTTON_LEFT: Key_Event(K_MOUSE1, state = SDL_PRESSED);
  //   SDL_BUTTON_RIGHT: Key_Event(K_MOUSE2, state = SDL_PRESSED);
  //   SDL_BUTTON_MIDDLE: Key_Event(K_MOUSE3, state = SDL_PRESSED);
  // end;
end;

procedure IN_JoyMove(axis, value: Integer);
begin
  // Handle joystick axis movement
end;

procedure IN_JoyButtons(button, state: Integer);
begin
  // Handle joystick button presses/releases
end;

initialization
  // No specific initialization needed here yet beyond global variable setup
  // IN_Init will be called by the game's main loop
finalization
  // IN_Shutdown will be called by the game's main loop
end.
