{
Copyright (C) Alexey Torgashin, uvviewsoft.com
Written for Lazarus LCL
License: MPL 2.0 or LGPL or any license which LCL can use
}

unit Gauges;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls;

type
  TGaugeKind = (gkText, gkHorizontalBar, gkVerticalBar);

const
  cInitGaugeValue = 20;
  cInitGaugeKind = gkHorizontalBar;

type
  { TGauge }

  TGauge = class(TGraphicControl)
  private
    FBitmap: TBitmap;
    FBorderStyle: TBorderStyle;
    FKind: TGaugeKind;
    FColorBack,
    FColorFore,
    FColorBorder: TColor;
    FMinValue,
    FMaxValue,
    FProgress: integer;
    FShowText: boolean;
    procedure DoPaintTo(C: TCanvas; r: TRect);
    function GetPercentDone: integer;
    procedure SetColorBorder(AValue: TColor);
    procedure SetBorderStyle(AValue: TBorderStyle);
    procedure SetColorBack(AValue: TColor);
    procedure SetColorFore(AValue: TColor);
    procedure SetKind(AValue: TGaugeKind);
    procedure SetMaxValue(AValue: integer);
    procedure SetMinValue(AValue: integer);
    procedure SetProgress(AValue: integer);
    procedure SetShowText(AValue: boolean);
  protected
    procedure Paint; override;
    procedure DoOnResize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddProgress(AValue: integer);
    property PercentDone: integer read GetPercentDone;
  published
    property Align;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property BorderSpacing;
    property Font;
    property Kind: TGaugeKind read FKind write SetKind default cInitGaugeKind;
    property Progress: integer read FProgress write SetProgress default cInitGaugeValue;
    property MinValue: integer read FMinValue write SetMinValue default 0;
    property MaxValue: integer read FMaxValue write SetMaxValue default 100;
    property BackColor: TColor read FColorBack write SetColorBack default clWhite;
    property ForeColor: TColor read FColorFore write SetColorFore default clNavy;
    property BorderColor: TColor read FColorBorder write SetColorBorder default clBlack;
    property ShowText: boolean read FShowText write SetShowText default true;
  end;

implementation

uses
  Math, Types, LCLType, LCLIntf;

{ TGauge }

procedure TGauge.DoPaintTo(C: TCanvas; r: TRect);
var
  NSize: integer;
  StrSize: TSize;
  Str: string;
begin
  //paint backgrd
  C.Pen.Color:= FColorBack;
  C.Brush.Color:= FColorBack;
  C.FillRect(r);

  //paint progress color9
  case FKind of
    gkText:
      begin
        //none
      end;
    gkHorizontalBar:
      begin
        C.Brush.Color:= FColorFore;
        NSize:= Round((r.Right-r.Left) * (FProgress-FMinValue) / (FMaxValue-FMinValue));
        C.FillRect(r.Left, r.Top, r.Left+NSize, r.Bottom);
      end;
    gkVerticalBar:
      begin
        C.Brush.Color:= FColorFore;
        NSize:= Round((r.Bottom-r.Top) * (FProgress-FMinValue) / (FMaxValue-FMinValue));
        C.FillRect(r.Left, r.Bottom-NSize, r.Right, r.Bottom);
      end;
  end;

  //paint text
  if FShowText then
  begin
    Str:= IntToStr(PercentDone)+' %';
    StrSize:= C.TextExtent(Str);
    C.Font.Assign(Self.Font);
    C.Brush.Style:= bsClear;
    C.TextOut(
      (r.Left+r.Right-StrSize.cx) div 2,
      (r.Top+r.Bottom-StrSize.cy) div 2,
      Str);
    C.Brush.Style:= bsSolid;
  end;

  //paint border
  if FBorderStyle<>bsNone then
  begin
    C.Pen.Color:= FColorBorder;
    C.Brush.Style:= bsClear;
    C.Rectangle(r);
    C.Brush.Style:= bsSolid;
  end;
end;

function TGauge.GetPercentDone: integer;
begin
  Result:= Round(100 * (FProgress-FMinValue) / (FMaxValue-FMinValue));
end;

procedure TGauge.SetColorBorder(AValue: TColor);
begin
  if FColorBorder=AValue then Exit;
  FColorBorder:=AValue;
  Update;
end;

procedure TGauge.SetBorderStyle(AValue: TBorderStyle);
begin
  if FBorderStyle=AValue then Exit;
  FBorderStyle:=AValue;
  Update;
end;

procedure TGauge.SetColorBack(AValue: TColor);
begin
  if FColorBack=AValue then Exit;
  FColorBack:=AValue;
  Update;
end;

procedure TGauge.SetColorFore(AValue: TColor);
begin
  if FColorFore=AValue then Exit;
  FColorFore:=AValue;
  Update;
end;

procedure TGauge.SetKind(AValue: TGaugeKind);
begin
  if FKind=AValue then Exit;
  FKind:=AValue;
  Update;
end;

procedure TGauge.SetMaxValue(AValue: integer);
begin
  if FMaxValue=AValue then Exit;
  FMaxValue:=Max(FMinValue+1, AValue);
  FProgress:=Min(FProgress, FMaxValue);
  Update;
end;

procedure TGauge.SetMinValue(AValue: integer);
begin
  if FMinValue=AValue then Exit;
  FMinValue:=Min(FMaxValue-1, AValue);
  FProgress:=Max(FProgress, FMinValue);
  Update;
end;

procedure TGauge.SetProgress(AValue: integer);
begin
  if FProgress=AValue then Exit;
  FProgress:=Max(FMinValue, Min(FMaxValue, AValue));
  Update;
end;

procedure TGauge.SetShowText(AValue: boolean);
begin
  if FShowText=AValue then Exit;
  FShowText:=AValue;
  Update;
end;

procedure TGauge.Paint;
var
  R: TRect;
begin
  inherited;

  R:= ClientRect;
  FBitmap.Canvas.Font.Assign(Self.Font);
  DoPaintTo(FBitmap.Canvas, R);
  Canvas.CopyRect(R, FBitmap.Canvas, R);
end;


constructor TGauge.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle:= ControlStyle
    +[csOpaque, csNoFocus]
    -[csDoubleClicks, csTripleClicks];

  Width:= 150;
  Height:= 50;

  FBitmap:= TBitmap.Create;
  FBitmap.SetSize(500, 80);

  FKind:= cInitGaugeKind;
  FBorderStyle:= bsSingle;

  FColorBack:= clWhite;
  FColorFore:= clNavy;
  FColorBorder:= clBlack;

  FMinValue:= 0;
  FMaxValue:= 100;
  FProgress:= cInitGaugeValue;
  FShowText:= true;
end;

destructor TGauge.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TGauge.AddProgress(AValue: integer);
begin
  Progress:= Progress+AValue;
end;

procedure TGauge.DoOnResize;
const
  cResizeBitmapStep = 100;
var
  SizeX, SizeY: integer;
begin
  inherited;

  if Assigned(FBitmap) then
  begin
    SizeX:= (Width div cResizeBitmapStep + 1)*cResizeBitmapStep;
    SizeY:= (Height div cResizeBitmapStep + 1)*cResizeBitmapStep;
    if (SizeX>FBitmap.Width) or (SizeY>FBitmap.Height) then
    begin
      FBitmap.SetSize(SizeX, SizeY);
      FBitmap.FreeImage; //recommended, else seen black bitmap on bigsize
    end;
  end;

  Update;
end;


initialization

end.

