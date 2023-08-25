unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DwmApi,
  UWP.ScrollBox, Vcl.ExtCtrls, UWP.Panel, System.ImageList, Vcl.ImgList,
  Vcl.WinXCtrls, JvComponentBase, JvFormAutoSize, JvAppStorage, JvFormPlacement,
  JvAppIniStorage, JvExExtCtrls, JvSplitter, JvFormTransparent;

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

  TfrmAltTabPro = class(TForm)
    ListBox1: TListBox;
    UPanel1: TUWPPanel;
    ImageList1: TImageList;
    SearchBox1: TSearchBox;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListBox1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
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
    procedure WM_NCCalcSize(var Msg: TWMNCCalcSize); message WM_NCCALCSIZE;
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
  function  RegisterShellHookWindow( hWnd : HWND ) : BOOL;    stdcall;
    external user32 name 'RegisterShellHookWindow';
  function  DeregisterShellHookWindow( hWnd : HWND) : BOOL;  stdcall;
    external user32 name 'DeregisterShellHookWindow';


implementation

uses
  Winapi.ShellAPI;

{$R *.dfm}

procedure TfrmAltTabPro.CreateParams(var Params: TCreateParams);
begin
  inherited;

  Params.WinClassName := 'AltTabProHwnd';

  Params.ExStyle := Params.ExStyle and not WS_EX_APPWINDOW;
  Params.WndParent := Application.Handle;
//  Params.Style := WS_POPUP or WS_VISIBLE;
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
  SetWindowLong(Application.Handle, GWL_EXSTYLE,
    (GetWindowLong(Application.Handle, GWL_EXSTYLE)
    OR WS_EX_TOOLWINDOW) AND NOT WS_EX_APPWINDOW);

  RegisterShellHookWindow(Handle);
  StartHook;

//  BorderStyle := bsSingle;
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
  DeregisterShellHookWindow(Handle);
end;

procedure TfrmAltTabPro.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
{  if key = VK_MENU then
  begin
    if (ListBox1.Items.Count > 0)
    and (ListBox1.ItemIndex < ListBox1.Items.Count)
    and (ListBox1.ItemIndex >= 0)
    then
    begin
      //SetForegroundWindow(StrToInt(appHandlers[ListBox1.ItemIndex]));
      SwitchToThisWindow(StrToInt(appHandlers[ListBox1.ItemIndex]), True);
      Hide;
      //ShowWindow(Handle, SW_HIDE);
    end;
    ListApps;
  end;}
end;

procedure TfrmAltTabPro.FormShow(Sender: TObject);
begin
  SetWindowLong(Application.Handle, GWL_EXSTYLE,
    (GetWindowLong(Application.Handle, GWL_EXSTYLE)
      OR WS_EX_TOOLWINDOW) AND NOT WS_EX_APPWINDOW);
  ShowWindow(Application.Handle, SW_HIDE);
  if ListBox1.Items.Count > 0 then
    ListBox1.ItemIndex := 0;
end;

procedure TfrmAltTabPro.KeyEventHandler(var Msg: TMessage);
var
  KeyName: String;
  Res: Integer;
  command: String;
  ThumbProps: DWM_THUMBNAIL_PROPERTIES;
  ThumbSize: SIZE;
  LHWindow: Cardinal;
  LHRect: TRect;
  IsShown: Boolean;
begin
  command := PChar(Msg.LParam);

  IsShown := False;

  if command = 'refresh' then
    if Visible then
      SearchBox1.Visible := False
    else
      Exit;

  if not Visible and (command <> 'released') then
  begin
    ListApps;
    Show;
    IsShown := True;
    SetWindowLong(Application.Handle, GWL_EXSTYLE,
        (GetWindowLong(Application.Handle, GWL_EXSTYLE)
          OR WS_EX_TOOLWINDOW) AND NOT WS_EX_APPWINDOW);
      ShowWindow(Application.Handle, SW_HIDE);
  end;
  ShellExecute(GetDesktopWindow, 'OPEN', PChar(ExtractFilePath(ParamStr(0))+'forceswitch.exe'), nil, nil, SW_SHOWNA);
  //SetLength(KeyName, 32);
  //Res := GetKeyNameText(Msg.LParam, @KeyName[1], Length(KeyName));
  //KeyName := copy(KeyName, 1, Res);


  if (ListBox1.Items.Count > 0)
//  and (ListBox1.ItemIndex >= 0)
  and (ListBox1.ItemIndex < ListBox1.Items.Count)
  then
  begin
    if ListBox1.ItemIndex < 0 then
      ListBox1.ItemIndex := 0;

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
    end
    else if command = 'released' then
    begin
      if Visible and not SearchBox1.Visible then
      begin
        Hide;
        Sleep(10);
        var app := StrToInt(appHandlers[ListBox1.ItemIndex]);
        if IsWindow(app) then
        begin
          SwitchToThisWindow(app, True);
          SetForegroundWindow(app);
        end;
      end;
    end
    else if command = 'escaped' then
    begin
      ListBox1.ItemIndex := 0;
      Hide;
    end
    else if command = 'command' then
    begin
      SearchBox1.Visible := True;
      SearchBox1.SetFocus;
    end;

    if IsShown then
    begin
      IsShown := False;
      if (ListBox1.ItemIndex = 0) and (ListBox1.Items.Count > 1) then
        ListBox1.ItemIndex := 1;
    end;

    LHWindow := StrToInt(appHandlers[ListBox1.ItemIndex]);
    UPanel1.Caption := appHandlers[ListBox1.ItemIndex];

    GetWindowRect(LHWindow, LHRect);
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

type
  PChildWindowInfo = ^TChildWindowInfo;
  TChildWindowInfo = record
    ownerPid: UINT;
    childPid: UINT;
end;

function EnumChildWindowsProc(Wnd: HWND; ChildInfo: PChildWindowInfo): BOOL; export; stdcall;
var
  aPID: DWORD;
begin
  Result := False;
  aPID := 0;
  GetWindowThreadProcessId(Wnd,aPID);
  if aPID <> ChildInfo.ownerPid then
  begin
    ChildInfo.childPid := aPID;
    Result := True;
  end;
end;

procedure TfrmAltTabPro.ListApps;
type
  TQueryFullProcessImageName = function(hProcess: THandle; dwFlags: DWORD; lpExeName: PChar; nSize: PDWORD): BOOL; stdcall;
const
  WS_EX_NOREDIRECTIONBITMAP = $200000;
  DWMWA_CLOAKED = 14; // Windows 8 or superior only
  DWM_NOT_CLOAKED = 0; // i.e. Visible for real
  DWM_CLOAKED_APP = 1;
  DWM_CLOAKED_SHELL = 2;
  DWM_CLOAKED_INHERITED = 4;
  DWM_NORMAL_APP_NOT_CLOAKED = 8; // invented number, might have issues on newest versions of window 10 2020 or earlier not tested
  STATUS_BUFFER_TOO_SMALL = LongInt($C0000023);
var
  hMod: THandle;
  NtUserBuildHwndList: function(aDesk: HDESK; aHwndNext: HWND; EnumChildren: BOOL;
    RemoveImmersive: BOOL; ThreadID: DWORD; aMax: UINT; aList: Pointer; var Cnt: UINT): LongInt; stdcall;
  lv_list: array of HWND;
  lv_cnt: UINT;
  lv_max: UINT;
  LHDesktop: HWND;
  LHWindow: HWND;
  LHParent: HWND;
  LExStyle: DWORD;
  AppClassName: array[0..255] of char;
  Cloaked: Cardinal;
  res: HRESULT;
  I: Integer;
  titlelen: Integer;
  title: array [0..255] of char;
  IsInCurrentDesktop: Boolean;
  DesktopId: string;

  fIcon: HICON;
  aIcon: TIcon;
  lpsfi: SHFILEINFO;
  fIconIndex: WORD;
  WinFileName: String;
  PID: DWORD;
  hProcess, hProcess2: THandle;
  FileName, FileName2: array[0..MAX_PATH -1] of Char;
  QueryFullProcessImageName: TQueryFullProcessImageName;
  nSize: Cardinal;
  FilePath: array[0..MAX_PATH] of WideChar;
  windowInfo: TChildWindowInfo;
  GuiInfo: TGUIThreadInfo;
begin
  appHandlers.BeginUpdate;
  ListBox1.Items.BeginUpdate;

  appHandlers.Clear;
  ListBox1.Items.Clear;
  ImageList1.Clear;

  hMod := LoadLibrary('win32u.dll');
  if hMod <> 0 then
  begin
    NtUserBuildHwndList := GetProcAddress(hMod, 'NtUserBuildHwndList');
    if (@NtUserBuildHwndList <> nil) then
    begin
      lv_max := 512;
      while True do
      begin
        SetLength(lv_list, lv_max);
        res :=  NtUserBuildHwndList(0, 0, BOOL(False), BOOL(false), 0, lv_max, Pointer(lv_list), lv_cnt);
        if res = NOERROR then
          Break;
        SetLength(lv_list, 0);

        lv_list := nil;

        if (res <> STATUS_BUFFER_TOO_SMALL) or (lv_cnt <= lv_max) then
          Break;

        lv_max := lv_cnt + 16;
      end;

      if lv_list <> nil then
      begin
        for I := 0 to lv_cnt - 1 do
        begin
            LHWindow := lv_list[I];
            if IsWindow(LHWindow) then
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
              and (AppClassName <> 'ApplicationManager_ImmersiveShellWindow')
              //and ( (cloaked = DWM_NOT_CLOAKED) or (cloaked = DWM_NORMAL_APP_NOT_CLOAKED))
              and ((LHParent=0)or(LHParent=LHDesktop))
              and (Application.Handle<>LHWindow)
              and ((LExStyle and WS_EX_TOOLWINDOW = 0)or(LExStyle and WS_EX_APPWINDOW <> 0))
              //and (LExStyle <> (WS_EX_NOREDIRECTIONBITMAP + WS_EX_TOPMOST))
              then
              begin
                titlelen := GetWindowTextLength(LHWindow);
                title := '';
                if titlelen > 0 then
                begin
                  GetWindowText(LHWindow,title, 256);
                end;

                var validApp := False;
                if ((cloaked = DWM_NOT_CLOAKED) or (cloaked = DWM_NORMAL_APP_NOT_CLOAKED))
                or(LExStyle = (WS_EX_NOREDIRECTIONBITMAP + WS_EX_TOPMOST)) then
                begin
                    appHandlers.Add(IntToStr(LHWindow));
                    ListBox1.Items.Add(title);
                    validApp := True;
                end;

                // get icon
                GetWindowThreadProcessId(LHWindow, PID);
                hProcess := OpenProcess(PROCESS_ALL_ACCESS, False, PID);
                if (hProcess <> 0) and validApp then
                try
                  nSize := MAX_PATH;
                  ZeroMemory(@FileName, MAX_PATH);
                  // WinVista +
                  @QueryFullProcessImageName := GetProcAddress(GetModuleHandle(kernel32), 'QueryFullProcessImageNameW');
                  if Assigned(QueryFullProcessImageName) then
                  begin
                    if QueryFullProcessImageName(hProcess, 0, FileName, @nSize) then
                    begin
                      SetString(WinFileName, PChar(@FileName[0]), nSize);
                      StrPLCopy(FilePath, WinFileName, High(FilePath));
                      if WinFileName.Contains('32\ApplicationFrameHost.exe') then
                      begin
                        // let's find the right UWP executable
                        //fAppItem.FilePath := 'UWP APP';
                        windowInfo.ownerPid := PID;
                        windowInfo.childPid := PID;
                        GetGUIThreadInfo(PID,GuiInfo);
                        EnumChildWindows(LHWindow,@EnumChildWindowsProc,LParam(@windowInfo));
                        Sleep(1);
                        // if found, let's find its executable path again
                        if windowInfo.ownerPid <> windowInfo.childPid then
                        begin
                          hProcess2 := OpenProcess(PROCESS_ALL_ACCESS, False, windowInfo.childPid);
                          if hProcess2 <> 0 then
                          try
                            if QueryFullProcessImageName(hProcess2, 0, FileName2, @nSize) then
                            begin
                              SetString(WinFileName, PChar(@FileName2[0]), nSize);
                              StrPLCopy(FilePath, WinFileName, High(FilePath));
                            end;
                          finally
                            CloseHandle(hProcess2);
                          end;
                        end;
                      end;
                      //fAppItem.Executable := ExtractFileName(FileName);
                    end;
                  end;

                  ZeroMemory(@lpsfi, SizeOf(SHFILEINFO));
                  SHGetFileInfo(PChar(WinFileName),FILE_ATTRIBUTE_NORMAL,
                    lpsfi, SizeOf(SHFILEINFO), SHGFI_ICON or SHGFI_LARGEICON);

                  aIcon := TIcon.Create;
                  try
                    aIcon.Handle := GetClassLong(LHWindow, GCL_HICON);
                    if aIcon.Handle = 0 then
                      aIcon.Handle := GetClassLong(LHWindow, GCL_HICONSM);
                        if aIcon.Handle = 0 then
                          aIcon.Handle := lpsfi.hIcon;
                            if aIcon.Handle = 0 then
                              aIcon.Handle := ExtractAssociatedIcon(HInstance, PChar(WinFileName),fIconIndex);

                    if aIcon.Handle <> 0 then
                    begin
                      ImageList1.AddIcon(aIcon);
                    end
                    else
                    begin
                      var bmp := TBitmap.Create;
                      try
                        bmp.SetSize(ImageList1.Width, ImageList1.Height);
                        bmp.Canvas.FillRect(Rect(0, 0, bmp.Width, bmp.Height));
                        ImageList1.AddMasked(bmp, bmp.TransparentColor);
                      finally
                        bmp.Free;
                      end;
                    end;

                  finally
                    aIcon.Free;
                  end;
                finally
                  CloseHandle(hProcess);
                end;

              end;
            end;
        end;
        SetLength(lv_list, 0);
      end;
    end;

    FreeLibrary(hMod);
  end;

  ListBox1.Items.EndUpdate;
  appHandlers.EndUpdate;
end;

procedure TfrmAltTabPro.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with (Control as TListBox).Canvas do
  begin
    if odSelected in State then
      Brush.Color := $00FFD2A6
    else
      Brush.Color := clWhite;
    FillRect(Rect);

    if ImageList1.Count > ListBox1.ItemIndex then
    begin
      ImageList1.Draw(ListBox1.Canvas, Rect.Left + 2, Rect.Top + 4, Index);
      TextOut(Rect.Left + 36, Rect.Top+6, ListBox1.Items[Index]);
    end
    else
      TextOut(Rect.Left, Rect.Top+6, ListBox1.Items[Index]);

    {TextOut(Rect.Left, Rect.Top, (Control as TListBox).Items[Index]);
    if odFocused In State then begin
      Brush.Color := ListBox1.Color;
      DrawFocusRect(Rect);}
  end;
end;

procedure TfrmAltTabPro.ListBox1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
{  if (GetAsyncKeyState(VK_LMENU) and $8000 <> 0)
  or (GetAsyncKeyState(VK_RMENU) and $8000 <> 0)
  then
  begin

  end
  else
    Hide;}
end;

procedure TfrmAltTabPro.WM_NCCalcSize(var Msg: TWMNCCalcSize);
var
  LCaptionBarHeight: Integer;
begin
  inherited;

  if BorderStyle = bsNone then Exit;

  LCaptionBarHeight := GetSystemMetrics(SM_CYCAPTION);

  if WindowState = wsNormal then
    Inc(LCaptionBarHeight, GetSystemMetrics(SM_CYSIZEFRAME) +
                GetSystemMetrics(SM_CXPADDEDBORDER));

  Dec(Msg.CalcSize_Params.rgrc[0].Top, LCaptionBarHeight );

end;

end.
