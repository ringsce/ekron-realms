unit opengl2;

{$IFDEF FPC}
  {$MODE ObjFPC}
{$ENDIF}

interface

uses
  SysUtils, {$IFDEF WINDOWS}Windows{$ELSE}ctypes, dynlibs{$ENDIF};

const
  // OpenGL constants (subset for OpenGL 2.0)
  GL_TRIANGLES = $0004;
  GL_QUADS = $0007;
  GL_COMPILE_STATUS = $8B81;
  GL_LINK_STATUS = $8B82;
  GL_VERTEX_SHADER = $8B31;
  GL_FRAGMENT_SHADER = $8B30;
  GL_ARRAY_BUFFER = $8892;
  GL_STATIC_DRAW = $88E4;

type
  // OpenGL types
  GLenum = Cardinal;
  GLboolean = Byte;
  GLbitfield = Cardinal;
  GLvoid = Pointer;
  GLbyte = ShortInt;
  GLshort = SmallInt;
  GLint = LongInt;
  GLclampx = LongInt;
  GLubyte = Byte;
  GLushort = Word;
  GLuint = Cardinal;
  GLsizei = LongInt;
  GLfloat = Single;
  GLclampf = Single;
  GLdouble = Double;
  GLclampd = Double;

var
  glClearColor: procedure(red, green, blue, alpha: GLfloat); cdecl;
  glClear: procedure(mask: GLbitfield); cdecl;
  glGenBuffers: procedure(n: GLsizei; buffers: PGLuint); cdecl;
  glBindBuffer: procedure(target: GLenum; buffer: GLuint); cdecl;
  glBufferData: procedure(target: GLenum; size: GLsizeiptr; const data: Pointer; usage: GLenum); cdecl;
  glEnableVertexAttribArray: procedure(index: GLuint); cdecl;
  glVertexAttribPointer: procedure(index: GLuint; size: GLint; _type: GLenum; normalized: GLboolean; stride: GLsizei; const pointer: Pointer); cdecl;
  glCreateShader: function(shaderType: GLenum): GLuint; cdecl;
  glShaderSource: procedure(shader: GLuint; count: GLsizei; const string_: PPChar; const length: PGLint); cdecl;
  glCompileShader: procedure(shader: GLuint); cdecl;
  glGetShaderiv: procedure(shader: GLuint; pname: GLenum; params: PGLint); cdecl;
  glCreateProgram: function(): GLuint; cdecl;
  glAttachShader: procedure(program_: GLuint; shader: GLuint); cdecl;
  glLinkProgram: procedure(program_: GLuint); cdecl;
  glUseProgram: procedure(program_: GLuint); cdecl;

procedure LoadOpenGL;

implementation

{$IFDEF WINDOWS}
const
  OpenGLLib = 'opengl32.dll';
{$ELSE}
const
  OpenGLLib = 'libGL.so';
{$ENDIF}

procedure LoadOpenGL;
begin
  glClearColor := GetProcAddress(GetModuleHandle(OpenGLLib), 'glClearColor');
  glClear := GetProcAddress(GetModuleHandle(OpenGLLib), 'glClear');
  glGenBuffers := GetProcAddress(GetModuleHandle(OpenGLLib), 'glGenBuffers');
  glBindBuffer := GetProcAddress(GetModuleHandle(OpenGLLib), 'glBindBuffer');
  glBufferData := GetProcAddress(GetModuleHandle(OpenGLLib), 'glBufferData');
  glEnableVertexAttribArray := GetProcAddress(GetModuleHandle(OpenGLLib), 'glEnableVertexAttribArray');
  glVertexAttribPointer := GetProcAddress(GetModuleHandle(OpenGLLib), 'glVertexAttribPointer');
  glCreateShader := GetProcAddress(GetModuleHandle(OpenGLLib), 'glCreateShader');
  glShaderSource := GetProcAddress(GetModuleHandle(OpenGLLib), 'glShaderSource');
  glCompileShader := GetProcAddress(GetModuleHandle(OpenGLLib), 'glCompileShader');
  glGetShaderiv := GetProcAddress(GetModuleHandle(OpenGLLib), 'glGetShaderiv');
  glCreateProgram := GetProcAddress(GetModuleHandle(OpenGLLib), 'glCreateProgram');
  glAttachShader := GetProcAddress(GetModuleHandle(OpenGLLib), 'glAttachShader');
  glLinkProgram := GetProcAddress(GetModuleHandle(OpenGLLib), 'glLinkProgram');
  glUseProgram := GetProcAddress(GetModuleHandle(OpenGLLib), 'glUseProgram');

  if not Assigned(glClearColor) then
    raise Exception.Create('Failed to load OpenGL functions. Ensure proper OpenGL drivers are installed.');
end;

end.

