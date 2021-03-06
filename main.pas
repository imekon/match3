unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, game;

type

  { TMainForm }

  TMainForm = class(TForm)
    BackgroundList: TImageList;
    ControlList: TImageList;
    Image1: TImage;
    JewelList: TImageList;
    TimeBar: TProgressBar;
    ScoreLabel: TLabel;
    PaintBox: TPaintBox;
    TimeLabel: TLabel;
    TickTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OnPaintBoxDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnPaintBoxDraw(Sender: TObject);
    procedure OnTickTimer(Sender: TObject);
  private
    _game: TGame;
    _cursorX, _cursorY: integer;

    procedure Process;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

const
  BackgroundSize = 65;
  MaxDimension = 7;

procedure TMainForm.OnPaintBoxDraw(Sender: TObject);
const
  offset = 8;

var
  x, y, item: integer;

begin
  for y := 0 to MaxDimension do
  begin
    for x := 0 to MaxDimension do
    begin
      //BackgroundList.Draw(PaintBox.Canvas, x * BackgroundSize, y * BackgroundSize, 0);
      item := _game.GetMapItem(x, y);
      if item <> -1 then
        JewelList.Draw(PaintBox.Canvas,
          x * BackgroundSize + offset, y * BackgroundSize + offset,
          item);
    end;
  end;

  if (_cursorX <> -1) and (_cursorY <> -1) then
    ControlList.Draw(PaintBox.Canvas,
      _cursorX * BackgroundSize, _cursorY * BackgroundSize,
      0);
end;

procedure TMainForm.OnTickTimer(Sender: TObject);
begin
  Process;
  ScoreLabel.Caption := 'Score: ' + IntToStr(_game.Score);
  PaintBox.Refresh;
end;

procedure TMainForm.Process;
var
  running: boolean;

begin
  running := true;
  while running do
    running := _game.Process;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  _game := TGame.Create;
  _game.Initialise;

  _cursorX := -1;
  _cursorY := -1;

  TickTimer.Enabled := true;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  TickTimer.Enabled := false;
  FreeAndNil(_game);
end;

procedure TMainForm.OnPaintBoxDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  _cursorX := X div BackgroundSize;
  _cursorY := Y div BackgroundSize;
  _game.SetClick(_cursorX, _cursorY);
  PaintBox.Refresh;
end;

end.

