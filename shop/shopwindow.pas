unit ShopWindow;

{$MODE OBJFPC}{$H+}
{$IFDEF FPC}
  {$IFDEF SDL3}
    {$DEFINE USE_SDL3}
  {$ELSE}
    {$DEFINE USE_SDL2}
  {$ENDIF}
{$ENDIF}

interface

uses
  {$IFDEF USE_SDL2}
  SDL2, GL, GLu;
  {$ENDIF}
  {$IFDEF USE_SDL3}
  SDL3, GL, GLu;
  {$ENDIF}

type
  TShopWindow = class
  private
    FWindow: PSDL_Window;
    FGLContext: TSDL_GLContext;
    FRunning: Boolean;
    FMouseX, FMouseY: Integer;
    FClicked: Boolean;
    FHoveredItem: Integer;
    FBuyButtonHovered: Boolean;
    procedure RenderText(const Text: String; X, Y: Integer);
    procedure RenderButton(X, Y, W, H: Integer; const LabelText: String; Hovered: Boolean);
    procedure RenderListView(X, Y, W, H: Integer; Items: array of String; HoverIndex: Integer);
    procedure HandleInput;
    procedure RenderScene;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

implementation

{ TShopWindow }

constructor TShopWindow.Create;
begin
  {$IFDEF USE_SDL2}
  if SDL_Init(SDL_INIT_VIDEO) < 0 then
    Halt(1);
  {$ENDIF}
  {$IFDEF USE_SDL3}
  if SDL_Init(SDL_INIT_VIDEO) <> 0 then
    Halt(1);
  {$ENDIF}

  FWindow := SDL_CreateWindow('Shop Window', SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
    800, 600, SDL_WINDOW_OPENGL);
  if FWindow = nil then
    Halt(1);

  FGLContext := SDL_GL_CreateContext(FWindow);
  if FGLContext = nil then
    Halt(1);

  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 2);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 1);

  glViewport(0, 0, 800, 600);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, 800, 600, 0, -1, 1); // 2D rendering
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  FRunning := True;
  FClicked := False;
  FHoveredItem := -1;
  FBuyButtonHovered := False;
end;

destructor TShopWindow.Destroy;
begin
  SDL_GL_DeleteContext(FGLContext);
  SDL_DestroyWindow(FWindow);
  SDL_Quit;
  inherited Destroy;
end;

procedure TShopWindow.RenderText(const Text: String; X, Y: Integer);
begin
  // Placeholder for text rendering
  Writeln('Render text: ', Text, ' at ', X, ', ', Y);
end;

procedure TShopWindow.RenderButton(X, Y, W, H: Integer; const LabelText: String; Hovered: Boolean);
begin
  if Hovered then
    glColor3f(0.8, 0.8, 0.2) // Highlighted color
  else
    glColor3f(0.5, 0.5, 0.5); // Normal button color

  glBegin(GL_QUADS);
    glVertex2f(X, Y);
    glVertex2f(X + W, Y);
    glVertex2f(X + W, Y + H);
    glVertex2f(X, Y + H);
  glEnd;

  glColor3f(1, 1, 1); // White for text
  RenderText(LabelText, X + 10, Y + 10);
end;

procedure TShopWindow.RenderListView(X, Y, W, H: Integer; Items: array of String; HoverIndex: Integer);
var
  I, ItemHeight: Integer;
begin
  ItemHeight := 30; // Height of each item
  for I := 0 to High(Items) do
  begin
    if I = HoverIndex then
      glColor3f(0.8, 0.8, 0.2) // Highlighted item
    else
      glColor3f(0.3, 0.3, 0.3); // Normal item color

    glBegin(GL_QUADS);
      glVertex2f(X, Y + I * ItemHeight);
      glVertex2f(X + W, Y + I * ItemHeight);
      glVertex2f(X + W, Y + (I + 1) * ItemHeight);
      glVertex2f(X, Y + (I + 1) * ItemHeight);
    glEnd;

    glColor3f(1, 1, 1); // White for text
    RenderText(Items[I], X + 10, Y + I * ItemHeight + 10);
  end;
end;

procedure TShopWindow.HandleInput;
var
  RelMouseX, RelMouseY: Integer;
begin
  {$IFDEF USE_SDL2}
  SDL_GetMouseState(@FMouseX, @FMouseY);
  {$ENDIF}
  {$IFDEF USE_SDL3}
  SDL_GetMouseState(nil, @FMouseX, @FMouseY);
  {$ENDIF}

  RelMouseX := FMouseX - 200; // Adjusted relative to list view position
  RelMouseY := FMouseY - 100;

  FHoveredItem := -1;
  FBuyButtonHovered := (FMouseX > 550) and (FMouseX < 650) and (FMouseY > 500) and (FMouseY < 550);

  if (RelMouseX > 0) and (RelMouseX < 300) and (RelMouseY > 0) and (RelMouseY < 300) then
    FHoveredItem := RelMouseY div 30; // Determine which item is hovered
end;

procedure TShopWindow.RenderScene;
const
  Items: array[0..4] of String = ('Sword', 'Shield', 'Potion', 'Armor', 'Helmet');
begin
  glClearColor(0.1, 0.1, 0.1, 1.0); // Background color
  glClear(GL_COLOR_BUFFER_BIT);

  // Render the shop list
  RenderListView(200, 100, 300, 300, Items, FHoveredItem);

  // Render the Buy button
  RenderButton(550, 500, 100, 50, 'Buy', FBuyButtonHovered);

  SDL_GL_SwapWindow(FWindow);
end;

procedure TShopWindow.Run;
var
  Event: TSDL_Event;
begin
  while FRunning do
  begin
    while SDL_PollEvent(@Event) <> 0 do
    begin
      case Event.type_ of
        SDL_QUITEV: FRunning := False;
        SDL_MOUSEBUTTONDOWN:
          if Event.button.button = SDL_BUTTON_LEFT then
            FClicked := True;
      end;
    end;

    HandleInput;

    if FClicked and FBuyButtonHovered then
    begin
      Writeln('Item purchased!');
      FClicked := False;
    end;

    RenderScene;
  end;
end;

end.

