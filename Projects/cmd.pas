unit Cmd;

interface

procedure ExecuteCommand(const Command: string);

implementation

procedure ExecuteCommand(const Command: string);
begin
  // Implementation of command execution
  WriteLn('Executing command: ', Command);
end;

end.
