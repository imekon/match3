unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, game;

type

  { TMainForm }

  TMainForm = class(TForm)
    BackgroundList: TImageList;
    JewelList: TImageList;
    PaintBox: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OnPaintBoxDraw(Sender: TObject);
  private
    _game: TGame;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.OnPaintBoxDraw(Sender: TObject);
const
  offset = 8;

var
  x, y: integer;

begin
  for y := 0 to 7 do
  begin
    for x := 0 to 7 do
    begin
      BackgroundList.Draw(PaintBox.Canvas, x * 63, y * 63, 0);
      JewelList.Draw(PaintBox.Canvas,
        x * 63 + offset, y * 63 + offset,
        _game.GetMapItem(x, y));
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  _game := TGame.Create;
  _game.Initialise;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  _game.Free;
end;

end.

