{ This file is part of the CodePreview (for Lazarus/FPC) package

  Copyright (C) 1999-2017 - Carl Caulkett - carl.caulkett@gmail.com

  MODIFIED LGPL Licence - this is the same licence as that used by the Free Pascal Compiler (FPC)
  A copy of the full licence can be found in the file Licence.md in the same folder as this file.

  This library is free software; you can redistribute it and/or modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version
  with the following modification:

  As a special exception, the copyright holders of this library give you permission to link this library with independent
  modules to produce an executable, regardless of the license terms of these independent modules, and to copy and distribute the
  resulting executable under terms of your choice, provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a module which is not derived from or based on this
  library. If you modify this library, you may extend this exception to your version of the library, but you are not obligated
  to do so. If you do not wish to do so, delete this exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to the Free
  Software Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}


unit main;

{$mode objfpc}{$H+}

{$DEFINE XCODE}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynCompletion, Forms, Controls,
  Graphics, Dialogs, FileCtrl, LazUtf8, StdCtrls, ExtCtrls, Buttons, Menus,
  EditBtn, ComCtrls, caUtils, JsonConf, LazFileUtils, caDbg, cajsonconfig,
  preferencesunit, casynconfig, TypInfo, casynhighlightergolang, Process,
  LazLogger, cahint;

type

  { TCodePreviewForm }

  TCodePreviewForm = class(TForm)
    GolangEdit: TSynEdit;
    FilesList: TFileListBox;
    FoldersList: TListBox;
    GolangEditPanel: TPanel;
    ToolbarSpacer: TLabel;
    PreferencesButton: TToolButton;
    RunButton: TToolButton;
    RunMemo: TMemo;
    RunTestButton: TToolButton;
    ToolBar: TToolBar;
    ToolbarImages: TImageList;
    LeftPanel: TPanel;
    MainMenu: TMainMenu;
    CodePreviewMenuItem: TMenuItem;
    AboutCodePreviewMenuItem: TMenuItem;
    SeparatorMenuItem: TMenuItem;
    StatusBar: TStatusBar;
    ToolbarPanel: TPanel;
    CancelButton: TToolButton;
    ToolButton3: TToolButton;
    WindowMenuItem: TMenuItem;
    MinimiseMenuItem: TMenuItem;
    SeparatorMenuItem2: TMenuItem;
    MainWindowMenuItem: TMenuItem;
    PreferencesMenuItem2: TMenuItem;
    PreferencesMenuItem: TMenuItem;
    ParentButtonPanel: TPanel;
    ParentDirButton: TSpeedButton;
    HorzSplitter: TSplitter;
    VertSplitter: TSplitter;
    Syntax: TcaSynGolangSyn;
    procedure FilesListClick(Sender: TObject);
    procedure FoldersListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GolangEditPanelPaint(Sender: TObject);
    procedure MainWindowMenuItemClick(Sender: TObject);
    procedure MinimiseMenuItemClick(Sender: TObject);
    procedure PreferencesMenuItem2Click(Sender: TObject);
    procedure PreferencesMenuItemClick(Sender: TObject);
    procedure ParentDirButtonClick(Sender: TObject);
    procedure RunButtonClick(Sender: TObject);
    procedure RunTestButtonClick(Sender: TObject);
    procedure SyntaxGetConfigPath(Sender: TObject; var AConfigPath: string);
    procedure PreferencesButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure ToolbarPanelPaint(Sender: TObject);
  private
    FDirectories: TStrings;
    FRoot: string;
    FGoRoot: String;
    FPreferencesForm: TPreferencesForm;
    function GetConfigPath: string;
    procedure AppShowHintEvent(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    procedure ColorGridChangedEvent(Sender: TObject);
    procedure FilesFontChangedEvent(Sender: TObject; AFont: TFont);
    procedure FoldersFontChangedEvent(Sender: TObject; AFont: TFont);
    procedure GolangFontChangedEvent(Sender: TObject; AFont: TFont);
    procedure LaunchPreferencesForm;
    procedure LoadDirectories;
    procedure LoadConfig;
    procedure LoadColorConfig;
    procedure SaveConfig;
    procedure UpdateCaption;
    procedure SearcherFoundDirectory(FileIterator: TFileIterator);
    procedure UpdateToolbarButtonState;
    procedure RunGolang(const AFileName: string);
    procedure UpdateDirectory(const ADirectory: string);
  end;

var
  CodePreviewForm: TCodePreviewForm;

implementation

{$R *.lfm}

{ TCodePreviewForm }

procedure TCodePreviewForm.FormCreate(Sender: TObject);
begin
  CodePreviewMenuItem.Caption := #$EF#$A3#$BF;
  FPreferencesForm := TPreferencesForm.Create(Application);
  Application.OnShowHint := @AppShowHintEvent;
  Application.ShowHint := True;
  DbgMethod(dmForm);
end;

procedure TCodePreviewForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveConfig;
  FDirectories.Free;
end;

procedure TCodePreviewForm.FilesListClick(Sender: TObject);
begin
  GolangEdit.Lines.LoadFromFile(FilesList.FileName);
  UpdateToolbarButtonState;
  UpdateCaption;
end;

procedure TCodePreviewForm.FoldersListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  Index := FoldersList.ItemIndex;
  FRoot := FDirectories[Index];
  FoldersList.Invalidate;
  Application.ProcessMessages;
  Sleep(600);
  UpdateDirectory(FRoot);
end;

procedure TCodePreviewForm.FormShow(Sender: TObject);
begin
  LoadConfig;
  RunMemo.Font := GolangEdit.Font;
  RunMemo.Font.Color := Syntax.IdentifierAttri.Foreground;
  RunMemo.Color := Syntax.SpaceAttri.Background;
  RunMemo.Align := alClient;
  RunMemo.Visible := False;
  GolangEditPanel.Color := Syntax.SpaceAttri.Background;
  FDirectories := TStringList.Create;
  UpdateDirectory(FRoot);
  LoadColorConfig;
end;

procedure TCodePreviewForm.GolangEditPanelPaint(Sender: TObject);
begin
  GolangEditPanel.Canvas.FloodFill(1, 1, clBtnFace, TFillStyle.fsSurface);
end;

procedure TCodePreviewForm.MinimiseMenuItemClick(Sender: TObject);
begin
  Application.Minimize;
end;

procedure TCodePreviewForm.PreferencesMenuItem2Click(Sender: TObject);
begin
  LaunchPreferencesForm;
end;

procedure TCodePreviewForm.MainWindowMenuItemClick(Sender: TObject);
begin
  Show;
end;

procedure TCodePreviewForm.PreferencesMenuItemClick(Sender: TObject);
begin
  LaunchPreferencesForm;
end;

procedure TCodePreviewForm.ParentDirButtonClick(Sender: TObject);
begin
  FRoot := ExtractFilePath(ExcludeTrailingPathDelimiter(FRoot));
  if FRoot[Length(FRoot)] = '/' then
    Delete(FRoot, Length(FRoot), 1);
  UpdateDirectory(FRoot);
end;

procedure TCodePreviewForm.RunButtonClick(Sender: TObject);
begin
  RunGolang(FilesList.FileName);
end;

procedure TCodePreviewForm.RunTestButtonClick(Sender: TObject);
begin
  RunGolang('');
end;

procedure TCodePreviewForm.SyntaxGetConfigPath(Sender: TObject; var AConfigPath: string);
begin
  AConfigPath := GetConfigPath;
end;

procedure TCodePreviewForm.PreferencesButtonClick(Sender: TObject);
begin
  LaunchPreferencesForm;
end;

procedure TCodePreviewForm.CancelButtonClick(Sender: TObject);
begin
  RunMemo.Visible := False;
  GolangEdit.Visible := True;
  UpdateToolbarButtonState;
end;

procedure TCodePreviewForm.ToolbarPanelPaint(Sender: TObject);
begin
  ToolbarPanel.Canvas.FloodFill(1, 1, clBtnFace, TFillStyle.fsSurface);
end;

function TCodePreviewForm.GetConfigPath: string;
begin
  {$IFDEF XCODE}
  Result := '/users/carlca/Code/fpc/CodePreview/';
  {$ELSE}
  Result := Utils.AppPath;
  {$ENDIF}
end;

procedure TCodePreviewForm.FilesFontChangedEvent(Sender: TObject; AFont: TFont);
begin
  FilesList.Font.Assign(AFont);
end;

procedure TCodePreviewForm.ColorGridChangedEvent(Sender: TObject);
begin
  Syntax.SaveConfig(GetConfigPath + 'colorconfig.json');
  Syntax.LoadConfig(GetConfigPath + 'colorconfig.json');
end;

procedure TCodePreviewForm.AppShowHintEvent(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
begin
  HintInfo.HintWindowClass := TcaHint;
  HintInfo.HintColor := clSkyBlue;
  CanShow := True;
end;

procedure TCodePreviewForm.FoldersFontChangedEvent(Sender: TObject; AFont: TFont);
begin
  FoldersList.Font.Assign(AFont);
end;

procedure TCodePreviewForm.GolangFontChangedEvent(Sender: TObject; AFont: TFont);
begin
  GolangEdit.Font.Assign(AFont);
end;

procedure TCodePreviewForm.LaunchPreferencesForm;
begin
  FPreferencesForm.ColorConfig := Syntax.ColorConfig;
  FPreferencesForm.OnFilesFontChanged := @FilesFontChangedEvent;
  FPreferencesForm.OnFoldersFontChanged := @FoldersFontChangedEvent;
  FPreferencesForm.OnGolangFontChanged := @GolangFontChangedEvent;
  FPreferencesForm.SetFonts(FilesList.Font, FoldersList.Font, GolangEdit.Font);
  FPreferencesForm.OnColorGridChanged := @ColorGridChangedEvent;
  FPreferencesForm.Show;
end;

procedure TCodePreviewForm.LoadColorConfig;
begin
  Syntax.LoadConfig(GetConfigPath + 'colorconfig.json');
end;

procedure TCodePreviewForm.LoadConfig;
var
  C: TcaJsonConfig;
begin
  C := TcaJsonConfig.Create(nil);
  try
    C.FileName := GetConfigPath + 'config.json';
    // mainForm
    C.Group := 'mainForm';
    Self.Left := C.IntProp['left'];
    Self.Top := C.IntProp['top'];
    Self.Width := C.IntProp['width'];
    Self.Height := C.IntProp['height'];
    // paths
    C.Group := 'paths';
    FRoot := C.StrProp['root'];
    if Length(FRoot) = 0 then
      FRoot := C.StrProp['gopath'];
    FGoRoot := C.StrProp['goroot'] + '/bin/go';
    // filesList
    C.Group := 'filesList';
    FilesList.Height := C.IntProp['height'];
    FilesList.Font.Name := C.StrProp['fontName'];
    FilesList.Font.Size := C.IntProp['fontSize'];
    // leftPanel
    C.Group := 'leftPanel';
    LeftPanel.Width := C.IntProp['width'];
    // foldersList
    C.Group := 'foldersList';
    FoldersList.Font.Name := C.StrProp['fontName'];
    FoldersList.Font.Size := C.IntProp['fontSize'];
    // golangEdit
    C.Group := 'golangEdit';
    GolangEdit.Font.Name := C.StrProp['fontName'];
    GolangEdit.Font.Size := C.IntProp['fontSize'];
    UpdateCaption;
  finally
    C.Free;
  end;
end;

procedure TCodePreviewForm.SaveConfig;
var
  C: TcaJsonConfig;
begin
  C := TcaJsonConfig.Create(nil);
  try
    C.FileName := GetConfigPath + 'config.json';
    // mainForm
    C.Group := 'mainForm';
    C.IntProp['left'] := Self.Left;
    C.IntProp['top'] := Self.Top;
    C.IntProp['width'] := Self.Width;
    C.IntProp['height'] := Self.Height;
    // paths
    C.Group := 'paths';
    C.StrProp['root'] := FRoot;
    // filesList
    C.Group := 'filesList';
    C.IntProp['height'] := FilesList.Height;
    C.StrProp['fontName'] := FilesList.Font.Name;
    C.IntProp['fontSize'] := FilesList.Font.Size;
    // leftPanel
    C.Group := 'leftPanel';
    C.IntProp['width'] := LeftPanel.Width;
    // foldersList
    C.Group := 'foldersList';
    C.StrProp['fontName'] := FoldersList.Font.Name;
    C.IntProp['fontSize'] := FoldersList.Font.Size;
    // golangEdit
    C.Group := 'golangEdit';
    C.StrProp['fontName'] := GolangEdit.Font.Name;
    C.IntProp['fontSize'] := GolangEdit.Font.Size;
  finally
    C.Free;
  end;
end;

procedure TCodePreviewForm.LoadDirectories;
var
  Searcher: TFileSearcher;
  Index: Integer;
  FileName: string;
begin
  FDirectories.Clear;
  FoldersList.Clear;
  Searcher := TFileSearcher.Create;
  try
    Searcher.OnDirectoryFound := @SearcherFoundDirectory;
    Searcher.Search(UTF8Encode(FRoot), '', False);
    for Index := 0 to FDirectories.Count - 1 do
      begin
        FileName := FDirectories[Index];
        Delete(FileName, 1, Length(FRoot) + 1);
        FoldersList.AddItem(FileName, TObject(Index));
      end;
  finally
    Searcher.Free;
  end;
end;

procedure TCodePreviewForm.SearcherFoundDirectory(FileIterator: TFileIterator);
var
  FileName: string;
begin
  FileName := FileIterator.FileName;
  Delete(FileName, 1, Length(FRoot) + 1);
  if FileName[1] <> '.' then
    FDirectories.Add(FileIterator.FileName);
end;

procedure TCodePreviewForm.UpdateToolbarButtonState;
var
  IsTest: Boolean;
  FileName: string;
begin
  IsTest := False;
  for FileName in FilesList.Items do
    begin
      if StringUtils.EndsWith(LowerCase(ExtractFileNameOnly(FileName)), '_test') then
        begin
          IsTest := True;
          Break;
        end;
    end;
  RunTestButton.Enabled := IsTest;
  RunButton.Enabled := FilesList.ItemIndex >= 0;
  CancelButton.Enabled := RunMemo.Visible;
end;

procedure TCodePreviewForm.RunGolang(const AFileName: string);
const
  BUF_SIZE = 2048;
var
  AProcess: TProcess;
  OutputStream: TStream;
  BytesRead: Longint;
  Buffer: array[1..BUF_SIZE] of byte;
begin
  GolangEdit.Visible := False;
  RunMemo.Visible := True;
  UpdateToolbarButtonState;
  AProcess := TProcess.Create(nil);
  // Need a way of letting the underlying terminal know which directory we are in...
  try
    AProcess.Executable := FGoRoot;
    if AFileName = '' then
      begin
        AProcess.Parameters.Add('test');
        AProcess.CurrentDirectory := FRoot;
      end
    else
      begin
        AProcess.Parameters.Add('run');
        AProcess.Parameters.Add(AFileName);
      end;
    AProcess.Options := [poUsePipes];
    AProcess.Execute;
    OutputStream := TMemoryStream.Create;
    try
      repeat
        BytesRead := AProcess.Output.Read(Buffer, BUF_SIZE);
        OutputStream.Write(Buffer, BytesRead)
      until BytesRead = 0;
      OutputStream.Position := 0;
      RunMemo.Lines.LoadFromStream(OutputStream);
    finally
      OutputStream.Free;
    end;
  finally
    DebugLn('AProcess ExitStatus: ', IntToStr(AProcess.ExitStatus));
    DebugLn('  AProcess ExitCode: ', IntToStr(AProcess.ExitCode));
    AProcess.Free;
  end;
end;

procedure TCodePreviewForm.UpdateDirectory(const ADirectory: string);
begin
  FilesList.Directory := FRoot;
  UpdateCaption;
  LoadDirectories;
  UpdateToolbarButtonState;
end;

procedure TCodePreviewForm.UpdateCaption;
var
  S: string;
begin
  S := 'CodePreview - ' + FRoot;
  if FilesList.FileName <> '' then
    S := 'CodePreview - ' + FilesList.FileName;
  Caption := S;
end;

end.

