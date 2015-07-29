{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit gauge_pkg;

interface

uses
  gauges, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('gauge_pkg', @Register);
end.
