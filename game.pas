unit game;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  Scores: array [0..8] of integer = (1, 2, 5, 10, 15, 20, 25, 50, 75);

type
  TGameMap = array [0..7, 0..7] of integer;

  TGameState = (gsIdle, gsFirstClick, gsSecondClick);

  { TGame }

  TGame = class
  private
    _score: integer;
    _map: TGameMap;
    _state: TGameState;
    _firstX, _firstY: integer;
    _secondX, _secondY: integer;
    _swapped: boolean;

    function LookRight(x, y: integer; map: TGameMap): integer;
    function LookDown(x, y: integer; map: TGameMap): integer;
    procedure Look(map: TGameMap; var rightMap, downMap: TGameMap);
    procedure DeleteItemRight(x, y, count: integer; var map: TGameMap);
    procedure DeleteItemDown(x, y, count: integer; var map: TGameMap);
    procedure DeleteItems(var map: TGameMap; rightMap, downMap: TGameMap);
    function DropDownColumn(x, y: integer; var map: TGameMap): boolean;
    function DropDown(var map: TGameMap): boolean;

    function FillAtTop(var map: TGameMap): boolean;
    procedure SetFirstClick(x, y: integer);
    procedure SetSecondClick(x, y: integer);
    procedure SetIdle;
    procedure Unswap(var map: TGameMap);
  public
    constructor Create;
    procedure Initialise;
    function Process: boolean;

    function GetMapItem(x, y: integer): integer;

    procedure SetClick(x, y: integer);

    property State: TGameState read _state write _state;
    property Score: integer read _score;
  end;

implementation

{ TGame }

const
  MaxJewelIndex = 9;

constructor TGame.Create;
var
  x, y: integer;

begin
  _state := gsIdle;
  _firstX := -1;
  _firstY := -1;
  _secondX := -1;
  _secondY := -1;
  _swapped := false;
  _score := 0;

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
      _map[x, y] := random(MaxJewelIndex);
end;

function TGame.Process: boolean;
var
  x, y: integer;
  rightMap, downMap: TGameMap;
  active: boolean;

begin
  for y := 0 to 7 do
    for x := 0 to 7 do
    begin
      rightMap[x, y] := 0;
      downMap[x, y] := 0;
    end;

  Look(_map, rightMap, downMap);
  DeleteItems(_map, rightMap, downMap);
  DropDown(_map);
  active := FillAtTop(_map);
  if _swapped then
  begin
    if not active then
      Unswap(_map);
    SetIdle;
  end;
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
  result := 0;
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
  result := 0;
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
  item, lookX: integer;

begin
  for lookX := x to x + count - 1 do
    if (lookX >= 0) and (lookX < 8) then
    begin
      item := map[lookX, y];
      if item <> -1 then
        inc(_score, Scores[item]);
      map[lookX, y] := -1;
    end;
end;

procedure TGame.DeleteItemDown(x, y, count: integer; var map: TGameMap);
var
  item, lookY: integer;

begin
  for lookY := y to y + count - 1 do
    if (lookY >= 0) and (lookY < 8) then
    begin
      item := map[x, lookY];
      if item <> -1 then
        inc(_score, Scores[item]);
      map[x, lookY] := -1;
    end;
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

function TGame.DropDownColumn(x, y: integer; var map: TGameMap): boolean;
begin
  result := false;

  if y < 1 then exit;

  if map[x, y] <> -1 then exit;

  map[x, y] := map[x, y - 1];
  map[x, y - 1] := -1;

  result := true;
end;

function TGame.DropDown(var map: TGameMap): boolean;
var
  x, y: integer;

begin
  result := false;
  for y := 7 downto 1 do
    for x := 0 to 7 do
    begin
      if DropDownColumn(x, y, map) then
        result := true;
    end;
end;

function TGame.FillAtTop(var map: TGameMap): boolean;
var
  x: integer;

begin
  result := false;
  for x := 0 to 7 do
  begin
    if map[x, 0] = -1 then
    begin
      map[x, 0] := random(MaxJewelIndex);
      result := true;
    end;
  end;
end;

procedure TGame.SetFirstClick(x, y: integer);
begin
  _firstX := x;
  _firstY := y;
  _secondX := -1;
  _secondY := -1;
  _swapped := false;
  _state := gsFirstClick;
end;

procedure TGame.SetSecondClick(x, y: integer);
var
  item1, item2: integer;

begin
  _secondX := x;
  _secondY := y;
  _state := gsSecondClick;

  item1 := _map[_firstX, _firstY];
  item2 := _map[_secondX, _secondY];

  _map[_firstX, _firstY] := item2;
  _map[_secondX, _secondY] := item1;

  _swapped := true;
end;

procedure TGame.SetIdle;
begin
  _firstX := -1;
  _firstY := -1;
  _secondX := -1;
  _secondY := -1;
  _state := gsIdle;
end;

procedure TGame.Unswap(var map: TGameMap);
var
  item1, item2: integer;

begin
  _swapped := false;

  if (_firstX = -1) or (_firstY = -1) or (_secondX = -1) or (_secondY = -1) then exit;

  item1 := map[_firstX, _firstY];
  item2 := map[_secondX, _secondY];

  map[_firstX, _firstY] := item2;
  map[_secondX, _secondY] := item1;
end;

initialization
  randomize;

end.

