library HotkeyHook;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  System.SysUtils,
  System.Classes,
  Windows,
  Messages;

const
  KeyEvent = WM_USER + 1;
  LLKHF_ALTDOWN = $20;
  LLKHF_UP = $80;

{ Define a record for recording and passing information process wide }
type
  PKBDLLHOOKSTRUCT = ^TKBDLLHOOKSTRUCT;
  TKBDLLHOOKSTRUCT = record
    vkCode: Cardinal;
    scanCode: Cardinal;
    flags: Cardinal;
    time: Cardinal;
    dwExtrainfo: Cardinal;
  end;

  PHookRec = ^THookRec;
  THookRec = packed record
    HookHandle: HHOOK;
    AppHandle: HWND;
    CtrlWinHandle: HWND;
    KeyCount: DWORD;
  end;

  TSystemKeyCombination = (skLWin,
  skRWin,
  skCtrlEsc,
  skAltTab,
  skAltEsc,
  skCtrlShiftEsc,
  skAltF4);
  TSystemKeyCombinations = set of TSystemKeyCombination;

{$R *.res}

var
  hObjHandle: THandle; { Variable for the file mappgin object }
  lpHookRec: PHookRec;
  InvalidCombinations: TSystemKeyCombinations;
  AltPressed: BOOL;
  CtrlPressed: BOOL;
  ShiftPressed: BOOL;

  procedure SwitchToThisWindow(h1: hWnd; x: bool); stdcall;
  external user32 Name 'SwitchToThisWindow';

{ Pointer to our hook record }
procedure MapFileMemory (dwAllocSize: DWORD);
begin
  { Create a process wide memory mapped variable }
  hObjHandle := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, dwAllocSize, 'AltTabProHook');
  if hObjHandle = 0 then
  begin
    raise Exception.Create('Hook couldn''t create file map object.');
    Exit;
  end;

  { Get a pointer to our process wide memory mapped file }
  lpHookRec := MapViewOfFile(hObjHandle, FILE_MAP_WRITE, 0, 0, dwAllocSize);
  if lpHookRec = nil then
  begin
    CloseHandle(hObjHandle);
    raise Exception.Create('Hook couldn''t map file.');
    Exit;
  end;
end;

procedure UnmapFileMemory;
begin
  { Delete our process wide memory mapped variable }
  if lpHookRec <> nil then
  begin
    UnmapViewOfFile(lpHookRec);
    lpHookRec := nil;
  end;

  if hObjHandle > 0 then
  begin
    CloseHandle(hObjHandle);
    hObjHandle := 0;
  end;
end;

function GetHookRecPointer:Pointer; stdcall;
begin
  { Return a pointer to our process wide memory mapped variable }
  Result := lpHookRec;
end;

function KeyboardProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  KeyUp: BOOL;
//  AltPressed: BOOL;
//  CtrlPressed: BOOL;
//  ShiftPressed: BOOL;
  KeyName: string;
  Res: Integer;
  ParentHandle: HWND;
  hs: PKBDLLHOOKSTRUCT;
  command: string;
begin
  Result := 0;

  case nCode of
    HC_ACTION:
    begin
      hs := PKBDLLHOOKSTRUCT(lParam);
//      CtrlPressed := GetAsyncKeyState(VK_CONTROL) and $8000 <> 0;
//      ShiftPressed := GetAsyncKeyState(VK_SHIFT) and $8000 <> 0;
//      AltPressed := GetAsyncKeyState(VK_MENU) and $8000 <> 0;

      if (wParam = WM_KEYDOWN) or (wParam = WM_SYSKEYDOWN) then
      begin
        if not ShiftPressed then
          ShiftPressed := (hs^.vkCode = VK_SHIFT) or (hs^.vkCode = VK_LSHIFT) or (hs^.vkCode = VK_RSHIFT);
        if not CtrlPressed then
          CtrlPressed := (hs^.vkCode = VK_CONTROL) or (hs^.vkCode = VK_LCONTROL) or (hs^.vkCode = VK_RCONTROL);
//        if not AltPressed then
          if (hs^.vkCode = VK_MENU) or (hs^.vkCode = VK_LMENU) or (hs^.vkCode = VK_RMENU) then
          begin
            AltPressed := True;
            ParentHandle := FindWindow('AltTabProHwnd', nil);
            command := 'refresh';
            if ParentHandle > 0 then
              SendMessageTimeout(ParentHandle, KeyEvent, wParam, Windows.LPARAM(PChar(command)), SMTO_NORMAL, 500, nil);
          end;
      end;

      if (wParam = WM_KEYUP) or (wParam = WM_SYSKEYUP) then
      begin
        if ShiftPressed then
          ShiftPressed := not((hs^.vkCode = VK_SHIFT) or (hs^.vkCode = VK_LSHIFT) or (hs^.vkCode = VK_RSHIFT));
        if CtrlPressed then
          CtrlPressed := not((hs^.vkCode = VK_CONTROL) or (hs^.vkCode = VK_LCONTROL) or (hs^.vkCode = VK_RCONTROL));

        if AltPressed
        and ((hs^.vkCode = VK_MENU) or (hs^.vkCode = VK_LMENU) or (hs^.vkCode = VK_RMENU)) then
        begin
          AltPressed := False;
          //alt was released
          ParentHandle := FindWindow('AltTabProHwnd', nil);
          command := 'released';
          if ParentHandle > 0 then
            SendMessageTimeout(ParentHandle, KeyEvent, wParam, Windows.LPARAM(PChar(command)), SMTO_NORMAL, 500, nil);
        end;

        if AltPressed
        and ((hs^.vkCode = VK_ESCAPE) or (hs^.vkCode = VK_CAPITAL)) then
        begin
//          AltPressed := False;
          ParentHandle := FindWindow('AltTabProHwnd', nil);
          command := 'escaped';
          if ParentHandle > 0 then
            SendMessageTimeout(ParentHandle, KeyEvent, wParam, Windows.LPARAM(PChar(command)), SMTO_NORMAL, 500, nil);
        end;

        if AltPressed
        and (hs^.vkCode = VK_SPACE) then
        begin
          ParentHandle := FindWindow('AltTabProHwnd', nil);
          command := 'command';
          if ParentHandle > 0 then
            SendMessageTimeout(ParentHandle, KeyEvent, wParam, Windows.LPARAM(PChar(command)), SMTO_NORMAL, 500, nil);
        end;

        if AltPressed
        and (hs^.vkCode = VK_TAB) then// and ((hs^.flags and LLKHF_ALTDOWN)<>0) then
        begin
          ParentHandle := FindWindow('AltTabProHwnd', nil);
          command := 'next';
          if ShiftPressed then
            command := 'prev';
          if (ParentHandle > 0) then//and IsWindowVisible(ParentHandle) then
          begin
            SendMessageTimeout(ParentHandle, KeyEvent, wParam, Windows.LPARAM(PChar(command)), SMTO_NORMAL, 500, nil);
            if IsWindowVisible(ParentHandle) then
            begin
              SwitchToThisWindow(ParentHandle, True);
              SetForegroundWindow(ParentHandle);
              var PID: DWORD;
              GetWindowThreadProcessId(ParentHandle, PID);
              AllowSetForegroundWindow(PID);
            end;
          end;

        end;

      end;

//      if (wParam = WM_KEYUP) or (wParam = WM_SYSKEYUP) then
//      if ((hs^.vkCode = VK_TAB) and ((hs^.flags and LLKHF_ALTDOWN)<>0))
//      or ((hs^.vkCode = VK_TAB) and ShiftPressed and ((hs^.flags and LLKHF_ALTDOWN)<>0))
      if ((hs^.vkCode = VK_TAB) and AltPressed)
      or ((hs^.vkCode = VK_ESCAPE) and AltPressed)
      or ((hs^.vkCode = VK_TAB) and AltPressed and ShiftPressed)
      then
      begin
        Exit(1);
        ParentHandle := FindWindow('AltTabProHwnd', nil);
        if ParentHandle > 0 then
        begin
          //SendMessageTimeout(ParentHandle, KeyEvent, wParam, lParam, SMTO_NORMAL, 500, nil);

          {if ShiftPressed then command := 'prev' else command := 'next';

          if GetForegroundWindow <> ParentHandle then
          begin
            ShowWindow(ParentHandle, SW_SHOWNORMAL);
            SetForegroundWindow(ParentHandle);
          end;

          if ((hs^.flags and LLKHF_UP) <> 0) then
          SendMessageTimeout(ParentHandle, KeyEvent, wParam, Windows.LPARAM(PChar(command)), SMTO_NORMAL, 500, nil);
           }
          // block alt-tab from showing
          Exit(1);
        end;
      end;

//      if (hs^.vkCode = VK_TAB) and ((hs^.flags and LLKHF_UP) <> 0) then
//      begin
//        ShowWindow(ParentHandle, SW_HIDE);
//      end;


        (*SetLength(KeyName, 32);
        Res := GetKeyNameText(lParam, @KeyName[1], Length(KeyName));
        ParentHandle := FindWindow('AltTabProHwnd', nil);
        if ParentHandle <> 0 then
        begin
          KeyUp := (lParam and (1 shl 31)) <> 0;

          { Alt Key }
          AltPressed := False;
          if (lParam and (1 shl 29)) <> 0 then
            AltPressed := True;

          { CtrlKey }
          CtrlPressed := False;
          if ((GetAsyncKeyState(VK_CONTROL) and (1 shl 15)) <> 0) then
            CtrlPressed := True;

          { Shift key }
          ShiftPressed := False;
          if ((GetAsyncKeyState(VK_SHIFT) and (1 shl 15)) <> 0) then
            ShiftPressed := True;

          { If KeyUp then increment the key count }
          if (KeyUp <> False) then
            Inc(lpHookRec^.KeyCount);

          case wParam of
            VK_TAB:
            begin

            end;
          end;

          SendMessageTimeout(ParentHandle, KeyEvent, wParam, lParam, SMTO_NORMAL, 500, nil);
        end;*)

        { Allow the keystroke }
      //  Result := 0;
      Result := CallNextHookEx(lpHookRec^.HookHandle, nCode, wParam, lParam);
    end;

    {HC_NOREMOVE:
    begin
      { This is a keystroke message, but the keystroke message has not been
      removed from the message queue, since an application has called
      PeekMessage() specifying PM_NOREMOVE
      Result := 0;
      Exit;
    end;}
  end;
{  if nCode < 0 then
    Result := CallNextHookEx(lpHookRec^.HookHandle, nCode, wParam, lParam);}
  Result := CallNextHookEx(lpHookRec^.HookHandle, nCode, wParam, lParam);
end;

procedure StartHook; stdcall;
begin
  { If we have a process wide memory variable and the hook has not already bee set }
  if ((lpHookRec <> nil) and (lpHookRec^.HookHandle = 0)) then
  begin
    { Set the hook and remember our hook handle }
//    var explorer := FindWindowEx(0, 0, 'Progman', nil);
//    if explorer > 0 then
    begin
//      var PID: DWORD;
//      GetWindowThreadProcessId(explorer, PID);
      lpHookRec^.HookHandle := SetWindowsHookEx(WH_KEYBOARD_LL, @KeyboardProc, HInstance, 0);//PID);
    end;
  end;
end;

procedure StopHook; stdcall;
begin
  { If we have a process wide memory variable and the hook has already been ser }
  if ((lpHookRec <> nil) and (lpHookRec^.HookHandle <> 0)) then
  begin
    { Remove our hook and clear our hook handle }
    if (UnhookWindowsHookEx(lpHookRec^.HookHandle) <> False) then
    begin
      lpHookRec^.HookHandle := 0;
    end;
  end;
end;

procedure DllEntryPoint(dwReason: DWORD);
begin
  case dwReason of
    DLL_PROCESS_ATTACH:
    begin
      { If we are getting mapped into a process, then get a pointer
        to our process wide memory mapped variable }
      hObjHandle := 0;
      lpHookRec := nil;
      MapFileMemory(SizeOf(lpHookRec^));
    end;
    DLL_PROCESS_DETACH:
    begin
      { If we are getting unmapped from a proces then, remove the
        pointer to our process wide memory mapped variable }
      UnmapFileMemory;
    end;
  end;
end;

Exports
  KeyboardProc Name 'KEYBOARDPROC',
  GetHookRecPointer name 'GETHOOKRECPOINTER',
  StartHook name 'STARTHOOK',
  StopHook name 'STOPHOOK';

begin
  DllProc := @DllEntryPoint;
  DllEntryPoint(DLL_PROCESS_ATTACH);
end.
