program AltTabPro;

uses
  Vcl.Forms,
  main in 'main.pas' {frmAltTabPro},
  skins in 'skins.pas',
  Winapi.Windows, Winapi.PsAPI;

{$R *.res}

var
  HActiveWindow, HForegroundThread, HAppThread: DWORD;
  FClientId: DWORD;

begin
  HActiveWindow := GetForegroundWindow();
  HForegroundThread := GetWindowThreadProcessId(HActiveWindow, @FClientId);
  AllowSetForegroundWindow(FClientId);
  HAppThread := GetCurrentThreadId;

  // Check if the window with title "AltTabPro - Window" exists
  if FindWindow(nil, 'AltTabPro - Window') <> 0 then
  begin
    if not SetForegroundWindow(FindWindow(nil, 'AltTabPro - Window')) then
      SwitchToThisWindow(GetDesktopWindow, True);

    if HForegroundThread <> HAppThread then
    begin
      AttachThreadInput(HForegroundThread, HAppThread, True);
      BringWindowToTop(FindWindow(nil, 'AltTabPro - Window'));
      Winapi.Windows.SetFocus(FindWindow(nil, 'AltTabPro - Window'));
      AttachThreadInput(HForegroundThread, HAppThread, False);
    end;

    Exit; // Exit the program since the window is already activated
  end;

  Application.Initialize;
//  Application.MainFormOnTaskbar := False;
  Application.ShowMainForm := False;
  Application.CreateForm(TfrmAltTabPro, frmAltTabPro);
  Application.Run;
end.
