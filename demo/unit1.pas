unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Gauges;

type
  { TForm1 }

  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    TrackBar1: TTrackBar;
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    g: tgauge;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
const
  cMin = 15;
  cMax = 102;
begin
  g:= tgauge.create(Self);
  g.parent:= self;
  g.align:= alTop;
  g.BorderSpacing.Around:= 6;
  g.ForeColor:= clMoneyGreen;
  g.BackColor:= clYellow;

  g.MinValue:= cMin;
  g.MaxValue:= cMax;
  TrackBar1.Min:= cMin;
  TrackBar1.Max:= cMax;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  TrackBar1.Position:= 30;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  g.Progress:= TrackBar1.Position;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  if checkbox1.Checked then g.BorderStyle:= bssingle else g.BorderStyle:= bsnone;
end;

end.

