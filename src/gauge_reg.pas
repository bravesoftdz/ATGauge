unit gauge_reg;

interface

uses
  SysUtils, Classes, Controls, LResources, gauges;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Misc', [TGauge]);
end;

initialization
  //{$I res/icons.lrs}

end.
