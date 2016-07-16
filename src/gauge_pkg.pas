{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit gauge_pkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  Gauges, gauge_reg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('gauge_reg', @gauge_reg.Register);
end;

initialization
  RegisterPackage('gauge_pkg', @Register);
end.
