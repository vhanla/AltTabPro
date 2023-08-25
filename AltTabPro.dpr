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

  // Check if the window with title "AltTabPro - Window" exists
  var wnd := FindWindow(nil, 'AltTabPro - Window');
  if wnd <> 0 then
  begin
    HActiveWindow := GetForegroundWindow();
    HForegroundThread := GetWindowThreadProcessId(HActiveWindow, @FClientId);
    AllowSetForegroundWindow(FClientId);
    HAppThread := GetCurrentThreadId;

    if not SetForegroundWindow(wnd) then
      SwitchToThisWindow(GetDesktopWindow, True);

    if HForegroundThread <> HAppThread then
    begin
      AttachThreadInput(HForegroundThread, HAppThread, True);
      BringWindowToTop(wnd);
      Winapi.Windows.SetFocus(wnd);
      AttachThreadInput(HForegroundThread, HAppThread, False);
    end;

    Exit; // Exit the program since the window is already activated
  end
  else
  begin

    Application.Initialize;
  //  Application.MainFormOnTaskbar := False;
    Application.ShowMainForm := False;
    Application.CreateForm(TfrmAltTabPro, frmAltTabPro);
    Application.Run;
  end;
end.
