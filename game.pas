unit game;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TGameMap = array [0..7, 0..7] of integer;

  TGameState = (gsIdle, gsFirstClick, gsSecondClick);

  { TGame }

  TGame = class
  private
    _map: TGameMap;
    _state: TGameState;
    _firstX, _firstY: integer;
    _secondX, _secondY: integer;

    function LookRight(x, y: integer; map: TGameMap): integer;
    function LookDown(x, y: integer; map: TGameMap): integer;
    procedure Look(map: TGameMap; var rightMap, downMap: TGameMap);
    procedure DeleteItemRight(x, y, count: integer; var map: TGameMap);
    procedure DeleteItemDown(x, y, count: integer; var map: TGameMap);
    procedure DeleteItems(var map: TGameMap; rightMap, downMap: TGameMap);

    procedure SetFirstClick(x, y: integer);
    procedure SetSecondClick(x, y: integer);
    procedure SetIdle;
  public
    constructor Create;
    procedure Initialise;
    function Process: boolean;

    function GetMapItem(x, y: integer): integer;

    procedure SetClick(x, y: integer);

    property State: TGameState read _state write _state;
  end;

implementation

{ TGame }

constructor TGame.Create;
var
  x, y: integer;

begin
  _state := gsIdle;
  _firstX := -1;
  _firstY := -1;
  _secondX := -1;
  _secondY := -1;

  for y := 0 to 7 do
    for x := 0 to 7 do
      _map[x, y] := -1;
end;

procedure TGame.Initialise;
var
  x, y: integer;

begin
  _state := gsIdle;
  _firstX := -1;
  _firstY := -1;
  _secondX := -1;
  _secondY := -1;

  for y := 0 to 7 do
    for x := 0 to 7 do
      _map[x, y] := random(8);
end;

function TGame.Process: boolean;
var
  x, y: integer;
  rightMap, downMap: TGameMap;

begin
  for y := 0 to 7 do
    for x := 0 to 7 do
    begin
      rightMap[x, y] := 0;
      downMap[x, y] := 0;
    end;

  Look(_map, rightMap, downMap);
  DeleteItems(_map, rightMap, downMap);
  result := false;
end;

function TGame.GetMapItem(x, y: integer): integer;
begin
  result := _map[x, y];
end;

procedure TGame.SetClick(x, y: integer);
begin
  case _state of
    gsIdle:       SetFirstClick(x, y);
    gsFirstClick: SetSecondClick(x, y);
    gsSecondClick: ;
  end;
end;

function TGame.LookRight(x, y: integer; map: TGameMap): integer;
var
  starting, count, lookX: integer;

begin
  if x > 5 then exit;

  count := 1;
  starting := map[x, y];
  for lookX := x + 1 to 7 do
  begin
    if map[lookX, y] = starting then
      inc(count)
    else
      break;
  end;

  result := count;
end;

function TGame.LookDown(x, y: integer; map: TGameMap): integer;
var
  starting, count, lookY: integer;

begin
  if y > 5 then exit;

  count := 1;
  starting := map[x, y];
  for lookY := y + 1 to 7 do
  begin
    if map[x, lookY] = starting then
      inc(count)
    else
      break;
  end;

  result := count;
end;

procedure TGame.Look(map: TGameMap; var rightMap, downMap: TGameMap);
var
  x, y: integer;

begin
  for y := 0 to 7 do
    for x := 0 to 7 do
    begin
      rightMap[x, y] := LookRight(x, y, map);
      downMap[x, y] := LookDown(x, y, map);
    end;
end;

procedure TGame.DeleteItemRight(x, y, count: integer; var map: TGameMap);
var
  lookX: integer;

begin
  for lookX := x to x + count - 1 do
    if (lookX >= 0) and (lookX < 8) then
      map[lookX, y] := -1;
end;

procedure TGame.DeleteItemDown(x, y, count: integer; var map: TGameMap);
var
  lookY: integer;

begin
  for lookY := y to y + count - 1 do
    if (lookY >= 0) and (lookY < 8) then
      map[x, lookY] := -1;
end;

procedure TGame.DeleteItems(var map: TGameMap; rightMap, downMap: TGameMap);
var
  x, y: integer;

begin
  for y := 0 to 7 do
    for x := 0 to 7 do
    begin
      if rightMap[x, y] >= 3 then
        DeleteItemRight(x, y, rightMap[x, y], map);
      if downMap[x, y] >= 3 then
        DeleteItemDown(x, y, downMap[x, y], map);
    end;
end;

procedure TGame.SetFirstClick(x, y: integer);
begin
  _firstX := x;
  _firstY := y;
  _secondX := -1;
  _secondY := -1;
  _state := gsFirstClick;
end;

procedure TGame.SetSecondClick(x, y: integer);
begin
  _secondX := x;
  _secondY := y;
  _state := gsSecondClick;

  SetIdle;
end;

procedure TGame.SetIdle;
begin
  _firstX := -1;
  _firstY := -1;
  _secondX := -1;
  _secondY := -1;
  _state := gsIdle;
end;

initialization
  randomize;

end.

