unit GridButton;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.StdCtrls;

type
  TGridButton = class(TButton)
  private
    FX, FY : Integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TGridButton]);
end;

end.
