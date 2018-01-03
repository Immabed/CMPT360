program Lab5;

uses
  Vcl.Forms,
  Minesweeper in 'Minesweeper.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
