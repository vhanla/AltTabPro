object frmAltTabPro: TfrmAltTabPro
  Left = 0
  Top = 0
  Caption = 'AltTabPro - Window'
  ClientHeight = 354
  ClientWidth = 756
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 201
    Top = 27
    Height = 327
    ExplicitLeft = 384
    ExplicitTop = 144
    ExplicitHeight = 100
  end
  object ListBox1: TListBox
    Left = 0
    Top = 27
    Width = 201
    Height = 327
    Style = lbOwnerDrawVariable
    Align = alLeft
    ItemHeight = 32
    TabOrder = 0
    OnDrawItem = ListBox1DrawItem
    OnKeyUp = ListBox1KeyUp
    ExplicitHeight = 318
  end
  object UPanel1: TUWPPanel
    Left = 204
    Top = 27
    Width = 552
    Height = 327
    Align = alClient
    Color = 15132390
    TabOrder = 1
    CustomBackColor.Enabled = False
    CustomBackColor.Color = 15132390
    CustomBackColor.LightColor = 15132390
    CustomBackColor.DarkColor = 2039583
    ExplicitWidth = 546
    ExplicitHeight = 318
  end
  object SearchBox1: TSearchBox
    Left = 0
    Top = 0
    Width = 756
    Height = 27
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Visible = False
    ExplicitWidth = 750
  end
  object ImageList1: TImageList
    Height = 24
    Width = 24
    Left = 336
    Top = 128
  end
end
