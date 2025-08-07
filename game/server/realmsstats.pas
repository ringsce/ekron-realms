unit RealmsStats;

{$mode objfpc}{$H+}

interface

type
  TPlayerStats = record
    Name: string;
    Kills: Integer;
    Deaths: Integer;
    Score: Integer;
    Level: Integer;
  end;

  TStatsArray = array of TPlayerStats;

procedure InitStats;
procedure AddPlayer(const PlayerName: string);
procedure RecordKill(const KillerName, VictimName: string);
procedure SetPlayerLevel(const PlayerName: string; Level: Integer);
procedure PrintStats;
function GetPlayerStats(const PlayerName: string): TPlayerStats;

var
  AllStats: TStatsArray;

implementation

uses
  SysUtils;

procedure InitStats;
begin
  SetLength(AllStats, 0);
end;

procedure AddPlayer(const PlayerName: string);
var
  i: Integer;
begin
  for i := 0 to High(AllStats) do
    if SameText(AllStats[i].Name, PlayerName) then
      Exit; // Already exists

  SetLength(AllStats, Length(AllStats) + 1);
  AllStats[High(AllStats)].Name := PlayerName;
  AllStats[High(AllStats)].Kills := 0;
  AllStats[High(AllStats)].Deaths := 0;
  AllStats[High(AllStats)].Score := 0;
  AllStats[High(AllStats)].Level := 1;
end;

procedure RecordKill(const KillerName, VictimName: string);
var
  i: Integer;
begin
  for i := 0 to High(AllStats) do
  begin
    if SameText(AllStats[i].Name, KillerName) then
    begin
      Inc(AllStats[i].Kills);
      Inc(AllStats[i].Score, 100);
    end
    else if SameText(AllStats[i].Name, VictimName) then
    begin
      Inc(AllStats[i].Deaths);
      Dec(AllStats[i].Score, 50);
    end;
  end;
end;

procedure SetPlayerLevel(const PlayerName: string; Level: Integer);
var
  i: Integer;
begin
  for i := 0 to High(AllStats) do
    if SameText(AllStats[i].Name, PlayerName) then
    begin
      AllStats[i].Level := Level;
      Exit;
    end;
end;

function GetPlayerStats(const PlayerName: string): TPlayerStats;
var
  i: Integer;
begin
  for i := 0 to High(AllStats) do
    if SameText(AllStats[i].Name, PlayerName) then
    begin
      Exit(AllStats[i]);
    end;
  // Return default if not found
  Result.Name := PlayerName;
  Result.Kills := 0;
  Result.Deaths := 0;
  Result.Score := 0;
  Result.Level := 0;
end;

procedure PrintStats;
var
  i: Integer;
begin
  Writeln('== Player Statistics ==');
  for i := 0 to High(AllStats) do
    with AllStats[i] do
      Writeln(Format('%s - Kills: %d, Deaths: %d, Score: %d, Level: %d',
        [Name, Kills, Deaths, Score, Level]));
end;

end.

