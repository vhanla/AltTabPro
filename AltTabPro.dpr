program AltTabPro;

uses
  Vcl.Forms,
  main in 'main.pas' {frmAltTabPro},
  skins in 'skins.pas';

{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := False;
  Application.ShowMainForm := False;
  Application.CreateForm(TfrmAltTabPro, frmAltTabPro);
  Application.Run;
end.
