unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Gauges;

type
  { TForm1 }

  TForm1 = class(TForm)
    bHalfPie: TRadioButton;
    chkTextInvert: TCheckBox;
    chkBorder: TCheckBox;
    chkShowtext: TCheckBox;
    GroupBox1: TGroupBox;
    bText: TRadioButton;
    bHorz: TRadioButton;
    bVert: TRadioButton;
    bNeedle: TRadioButton;
    bPie: TRadioButton;
    TrackBar1: TTrackBar;
    procedure bHalfPieChange(Sender: TObject);
    procedure bNeedleChange(Sender: TObject);
    procedure bPieChange(Sender: TObject);
    procedure chkBorderChange(Sender: TObject);
    procedure chkShowtextChange(Sender: TObject);
    procedure chkTextInvertChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bTextChange(Sender: TObject);
    procedure bHorzChange(Sender: TObject);
    procedure bVertChange(Sender: TObject);
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
  cMax = 122;
begin
  g:= tgauge.create(Self);
  g.parent:= self;
  g.align:= alTop;
  g.height:= 100;
  g.BorderSpacing.Around:= 6;

  g.ForeColor:= clGreen;
  g.BackColor:= clYellow;

  g.MinValue:= cMin;
  g.MaxValue:= cMax;
  TrackBar1.Min:= cMin;
  TrackBar1.Max:= cMax;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  bHorz.Checked:= true;
  TrackBar1.Position:= 30;
end;

procedure TForm1.bTextChange(Sender: TObject);
begin
  g.kind:= gkText;
end;

procedure TForm1.bHorzChange(Sender: TObject);
begin
  g.kind:= gkHorizontalBar;
end;

procedure TForm1.bVertChange(Sender: TObject);
begin
  g.kind:= gkVerticalBar;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  g.Progress:= TrackBar1.Position;
end;

procedure TForm1.chkBorderChange(Sender: TObject);
begin
  if chkBorder.Checked then g.BorderStyle:= bssingle else g.BorderStyle:= bsnone;
end;

procedure TForm1.bNeedleChange(Sender: TObject);
begin
  g.kind:= gkNeedle;
end;

procedure TForm1.bHalfPieChange(Sender: TObject);
begin
  g.kind:= gkHalfPie;
end;

procedure TForm1.bPieChange(Sender: TObject);
begin
  g.kind:= gkPie;
end;

procedure TForm1.chkShowtextChange(Sender: TObject);
begin
  g.ShowText:= chkShowtext.Checked;
end;

procedure TForm1.chkTextInvertChange(Sender: TObject);
begin
  g.ShowTextInverted:= chkTextInvert.Checked;
end;

end.

