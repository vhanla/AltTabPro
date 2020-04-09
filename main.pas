unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, UCL.Form, DwmApi,
  UCL.ScrollBox, Vcl.ExtCtrls, UCL.Panel;

const
  KeyEvent = WM_USER + 1;
  THUMBHEIGHT = 132;

type
  TAccentPolicy = packed record
    AccentState: Integer;
    AccentFlags: Integer;
    GradientColor: Integer;
    AnimationId: Integer;
  end;

  TWindowCompositionAttributeData = packed record
    Attribute: Cardinal;
    Data: Pointer;
    SizeOfData: Integer;
  end;

  TfrmAltTabPro = class(TUForm)
    ListBox1: TListBox;
    UPanel1: TUPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    ThumbDesktop, ThumbTaskbar, ThumbWindow: HTHUMBNAIL;
    procedure KeyEventHandler(var Msg: TMessage); message KeyEvent;
    procedure EnableBlur;
    procedure ListApps;
    procedure DrawDesktop;
    procedure DrawTaskbar;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
  end;

var
  frmAltTabPro: TfrmAltTabPro;
  appHandlers: TStringList;

  SetWindowCompositionAttribute:function (hWnd: HWND; var data: TWindowCompositionAttributeData):integer; stdcall;

  procedure SwitchToThisWindow(h1: hWnd; x: bool); stdcall;
  external user32 Name 'SwitchToThisWindow';

  function GetTickCount64:ULONGLONG;stdcall;
  external 'kernel32.dll' name 'GetTickCount64';
  procedure StartHook; stdcall;
    external 'HotkeyHook.dll' name 'STARTHOOK';
  procedure StopHook; stdcall;
    external 'HotkeyHook.dll' name 'STOPHOOK';
  function GetShellWindow:HWND;stdcall;
  external user32 Name 'GetShellWindow';
implementation

{$R *.dfm}

procedure TfrmAltTabPro.CreateParams(var Params: TCreateParams);
begin
  inherited;

  Params.WinClassName := 'AltTabProHwnd';
end;

procedure TfrmAltTabPro.DrawDesktop;
var
  ThumbProps: DWM_THUMBNAIL_PROPERTIES;
  ThumbSize: SIZE;
  LHDesktop: HWND;
begin
  LHDesktop := GetShellWindow;

  if ThumbDesktop <> 0 then
    DwmUnregisterThumbnail(ThumbDesktop);

  if Succeeded(DwmRegisterThumbnail(Handle, LHDesktop, @ThumbDesktop)) then
  begin
    DwmQueryThumbnailSourceSize(ThumbDesktop, @ThumbSize);
    if (ThumbSize.cx <> 0) and (ThumbSize.cy <> 0) then
    begin
      ThumbProps.dwFlags := DWM_TNP_SOURCECLIENTAREAONLY
                            or DWM_TNP_VISIBLE
                            or DWM_TNP_OPACITY
                            or DWM_TNP_RECTDESTINATION;
      ThumbProps.fSourceClientAreaOnly := False;
      ThumbProps.fVisible := True;
      ThumbProps.opacity := 255;
      ThumbProps.rcDestination := Rect(
        UPanel1.Left,
        UPanel1.Top,
        UPanel1.Left+UPanel1.Width,
        UPanel1.Top+UPanel1.Height
      );
      DwmUpdateThumbnailProperties(ThumbDesktop, ThumbProps);
    end;
  end;
end;

procedure TfrmAltTabPro.DrawTaskbar;
var
  ThumbProps: DWM_THUMBNAIL_PROPERTIES;
  ThumbSize: SIZE;
  LHDesktop: HWND;
begin
  LHDesktop := FindWindow('Shell_TrayWnd', nil);

  if ThumbTaskbar <> 0 then
    DwmUnregisterThumbnail(ThumbTaskbar);

  if Succeeded(DwmRegisterThumbnail(Handle, LHDesktop, @ThumbTaskbar)) then
  begin
    DwmQueryThumbnailSourceSize(ThumbTaskbar, @ThumbSize);
    if (ThumbSize.cx <> 0) and (ThumbSize.cy <> 0) then
    begin
      ThumbProps.dwFlags := DWM_TNP_SOURCECLIENTAREAONLY
                            or DWM_TNP_VISIBLE
                            or DWM_TNP_OPACITY
                            or DWM_TNP_RECTDESTINATION;
      ThumbProps.fSourceClientAreaOnly := False;
      ThumbProps.fVisible := True;
      ThumbProps.opacity := 255;
      ThumbProps.rcDestination := Rect(
        UPanel1.Left,
        UPanel1.Top+UPanel1.Height-15,
        UPanel1.Left+UPanel1.Width,
        UPanel1.Top+UPanel1.Height
      );
      DwmUpdateThumbnailProperties(ThumbTaskbar, ThumbProps);
    end;
  end;
end;

procedure TfrmAltTabPro.EnableBlur;
const
  WCA_ACCENT_POLICY = 19;
  ACCENT_ENABLE_BLURBEHIND = 3;
  ACCENT_ENABLE_ACRYLICBLURBEHIND = 4;
var
  dwm10: THandle;
  data: TWindowCompositionAttributeData;
  accent: TAccentPolicy;
begin
  dwm10 := LoadLibrary(user32);
  try
    @SetWindowCompositionAttribute := GetProcAddress(dwm10, 'SetWindowCompositionAttribute');
    if @SetWindowCompositionAttribute <> nil then
    begin
      accent.AccentState := ACCENT_ENABLE_ACRYLICBLURBEHIND;
      accent.GradientColor := accent.GradientColor;
      data.Attribute := WCA_ACCENT_POLICY;
      data.Data := @accent;
      SetWindowCompositionAttribute(Handle, data);
    end;
  finally
    FreeLibrary(dwm10);
  end;
end;

procedure TfrmAltTabPro.FormCreate(Sender: TObject);
begin
  StartHook;

  BorderStyle := bsSingle;
  BorderIcons := [];
  GlassFrame.Enabled := False;
//  BorderStyle := bsNone;
//  Color := clBlack;
  DoubleBuffered := True;

//  EnableBlur;

  appHandlers := TStringList.Create;
  ListApps;
end;

procedure TfrmAltTabPro.FormDestroy(Sender: TObject);
begin
  StopHook;
  appHandlers.Free;
end;

procedure TfrmAltTabPro.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = VK_MENU then
  begin
    //Hide;
    ShowWindow(Handle, SW_HIDE);
    if (ListBox1.Items.Count > 0)
    and (ListBox1.ItemIndex < ListBox1.Items.Count)
    and (ListBox1.ItemIndex >= 0)
    then
    begin
      SwitchToThisWindow(StrToInt(appHandlers[ListBox1.ItemIndex]), True);
    end;
    ListApps;
  end;
end;

procedure TfrmAltTabPro.FormShow(Sender: TObject);
begin
  ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TfrmAltTabPro.KeyEventHandler(var Msg: TMessage);
var
  KeyName: String;
  Res: Integer;
  command: String;
  ThumbProps: DWM_THUMBNAIL_PROPERTIES;
  ThumbSize: SIZE;
  LHWindow: Cardinal;
begin
  if not Visible then
  begin
    ListApps;
    Show;
  end;
  //SetLength(KeyName, 32);
  //Res := GetKeyNameText(Msg.LParam, @KeyName[1], Length(KeyName));
  //KeyName := copy(KeyName, 1, Res);

  command := PChar(Msg.LParam);

  if (ListBox1.Items.Count > 0) and (ListBox1.ItemIndex < ListBox1.Items.Count) then
  begin

    if command = 'prev' then
    begin
      //Memo1.Lines.Add('prev');
      if ListBox1.ItemIndex = 0 then
        ListBox1.ItemIndex := ListBox1.Items.Count - 1
      else
        ListBox1.ItemIndex := ListBox1.ItemIndex - 1;
    end
    else if command = 'next' then
    begin
      //Memo1.Lines.Add('next');
      if ListBox1.ItemIndex = ListBox1.Items.Count - 1 then
        ListBox1.ItemIndex := 0
      else
        ListBox1.ItemIndex := ListBox1.ItemIndex + 1;
    end;

    LHWindow := StrToInt(appHandlers[ListBox1.ItemIndex]);
    UPanel1.Caption := appHandlers[ListBox1.ItemIndex];

  end;
  // draw desktop
  DrawDesktop;
  // draw taskbar
  DrawTaskbar;
  // draw thumbnail
  if ThumbWindow <> 0 then
    DwmUnregisterThumbnail(ThumbWindow);

  if Succeeded(DwmRegisterThumbnail(Handle, LHWindow, @ThumbWindow)) then
  begin
    DwmQueryThumbnailSourceSize(ThumbWindow, @ThumbSize);
    if (ThumbSize.cx <> 0) and (ThumbSize.cy <> 0) then
    begin
      ThumbProps.dwFlags := DWM_TNP_SOURCECLIENTAREAONLY
                            or DWM_TNP_VISIBLE
                            or DWM_TNP_OPACITY
                            or DWM_TNP_RECTDESTINATION;
      ThumbProps.fSourceClientAreaOnly := False;
      ThumbProps.fVisible := True;
      ThumbProps.opacity := 255;
      ThumbProps.rcDestination := Rect(
        UPanel1.Left+25,
        UPanel1.Top+25,
        UPanel1.Left+UPanel1.Width-50,
        UPanel1.Top+UPanel1.Height-50
      );
      DwmUpdateThumbnailProperties(ThumbWindow, ThumbProps);
    end;
  end;
end;

procedure TfrmAltTabPro.ListApps;
const
  DWMWA_CLOAKED = 14; // Windows 8 or superior only
  DWM_NOT_CLOAKED = 0; // i.e. Visible for real
  DWM_CLOAKED_APP = 1;
  DWM_CLOAKED_SHELL = 2;
  DWM_CLOAKED_INHERITED = 4;
  DWM_NORMAL_APP_NOT_CLOAKED = 8; // invented number, might have issues on newest versions of window 10 2020 or earlier not tested
var
  LHDesktop: HWND;
  LHWindow: HWND;
  LHParent: HWND;
  LExStyle: DWORD;
  AppClassName: array[0..255] of char;
  Cloaked: Cardinal;
  title: array [0..255] of char;
begin
  appHandlers.BeginUpdate;
  ListBox1.Items.BeginUpdate;

  appHandlers.Clear;
  ListBox1.Items.Clear;

  LHDesktop:=GetDesktopWindow;
  LHWindow:=GetWindow(LHDesktop,GW_CHILD);

  while LHWindow <> 0 do
  begin
    GetClassName(LHWindow, AppClassName, 255);
    LHParent:=GetWindowLong(LHWindow,GWL_HWNDPARENT);
    LExStyle:=GetWindowLong(LHWindow,GWL_EXSTYLE);

    // only works on windows superior to windows 7
    if AppClassName = 'ApplicationFrameWindow' then
      DwmGetWindowAttribute(LHWindow, DWMWA_CLOAKED, @cloaked, sizeof(Cardinal))
    else
      cloaked := DWM_NORMAL_APP_NOT_CLOAKED;

    if IsWindowVisible(LHWindow)
    and (GetProp(LHWindow, 'ITaskList_Deleted') = 0)
    and (AppClassName <> 'Windows.UI.Core.CoreWindow')
    and ( (cloaked = DWM_NOT_CLOAKED) or (cloaked = DWM_NORMAL_APP_NOT_CLOAKED))
    and ((LHParent=0)or(LHParent=LHDesktop))
    and (Application.Handle<>LHWindow)
    and ((LExStyle and WS_EX_TOOLWINDOW = 0)or(LExStyle and WS_EX_APPWINDOW <> 0))
    then
    begin
      GetWindowText(LHWindow,title, 256);
      appHandlers.Add(IntToStr(LHWindow));
      ListBox1.Items.Add(title);
    end;
    LHWindow:=GetWindow(LHWindow, GW_HWNDNEXT);
  end;

  ListBox1.Items.EndUpdate;
  appHandlers.EndUpdate;
end;

end.
