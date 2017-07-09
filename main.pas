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

{.$DEFINE DBG}
{$DEFINE XCODE}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit,
  SynCompletion, Forms, Controls, Graphics, Dialogs,
  FileCtrl, LazUtf8, StdCtrls, ExtCtrls, Buttons, Menus, EditBtn, caUtils,
  JsonConf, LazFileUtils, caDbg, cajsonconfig, preferencesunit, casynconfig,
  TypInfo, casynhighlightergolang;

type

  { TCodePreviewForm }

  TCodePreviewForm = class(TForm)
    Edit: TSynEdit;
    Files: TFileListBox;
    DirectoryList: TListBox;
    LeftPanel: TPanel;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PreferencesMenuItem: TMenuItem;
    Panel3: TPanel;
    ParentDirButton: TSpeedButton;
    HorzSplitter: TSplitter;
    VertSplitter: TSplitter;
    Syntax: TcaSynGolangSyn;
    procedure DirectoryListClick(Sender: TObject);
    procedure FilesClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PreferencesMenuItemClick(Sender: TObject);
    procedure ParentDirButtonClick(Sender: TObject);
    procedure SyntaxGetConfigPath(Sender: TObject; var AConfigPath: string);
  private
    FDirectories: TStrings;
    FRoot: string;
    function GetConfigPath: string;
    procedure LoadConfig;
    procedure LoadColorConfig;
    procedure SaveConfig;
    procedure LoadDirectories;
    procedure UpdateCaption;
    procedure SearcherFoundDirectory(FileIterator: TFileIterator);
  end;

var
  CodePreviewForm: TCodePreviewForm;

implementation

{$R *.lfm}

{ TCodePreviewForm }

procedure TCodePreviewForm.FormCreate(Sender: TObject);
begin
  MenuItem1.Caption := #$EF#$A3#$BF;
end;

procedure TCodePreviewForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveConfig;
  FDirectories.Free;
end;

procedure TCodePreviewForm.DirectoryListClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := DirectoryList.ItemIndex;
  FRoot := FDirectories[Index];
  Files.Directory := FRoot;
  UpdateCaption;
  LoadDirectories;
end;

procedure TCodePreviewForm.FilesClick(Sender: TObject);
begin
  Edit.Lines.LoadFromFile(Files.FileName);
  UpdateCaption;
end;

procedure TCodePreviewForm.FormShow(Sender: TObject);
begin
  LoadConfig;
  FDirectories := TStringList.Create;
  Files.Directory := FRoot;
  LoadDirectories;
  LoadColorConfig;
end;

procedure TCodePreviewForm.PreferencesMenuItemClick(Sender: TObject);
var
  PreferencesForm: TPreferencesForm;
begin
  PreferencesForm := TPreferencesForm.Create(nil);
  try
    PreferencesForm.ColorConfig := Syntax.ColorConfig;
    PreferencesForm.ShowModal;
    if PreferencesForm.Modified then
      begin
        Syntax.ColorConfig := PreferencesForm.ColorConfig;
        Syntax.SaveConfig(GetConfigPath + 'colorconfig.json');
      end;
  finally
    PreferencesForm.Free;
  end;
end;

procedure TCodePreviewForm.ParentDirButtonClick(Sender: TObject);
begin
  FRoot := ExtractFilePath(ExcludeTrailingPathDelimiter(FRoot));
  if FRoot[Length(FRoot)] = '/' then
    Delete(FRoot, Length(FRoot), 1);
  Files.Directory := FRoot;
  UpdateCaption;
  LoadDirectories;
end;

procedure TCodePreviewForm.SyntaxGetConfigPath(Sender: TObject; var AConfigPath: string);
begin
  AConfigPath := GetConfigPath;
end;

function TCodePreviewForm.GetConfigPath: string;
begin
  {$IFDEF XCODE}
  Result := '/users/carlca/Code/fpc/CodePreview/';
  {$ELSE}
  Result := Utils.AppPath;
  {$ENDIF}
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
    // files
    C.Group := 'files';
    Files.Height := C.IntProp['height'];
    // leftPanel
    C.Group := 'leftPanel';
    LeftPanel.Width := C.IntProp['width'];
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
    // files
    C.Group := 'files';
    C.IntProp['height'] := Files.Height;
    // leftPanel
    C.Group := 'leftPanel';
    C.IntProp['width'] := LeftPanel.Width;
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
  DirectoryList.Clear;
  Searcher := TFileSearcher.Create;
  try
    Searcher.OnDirectoryFound := @SearcherFoundDirectory;
    Searcher.Search(UTF8Encode(FRoot), '', False);
    for Index := 0 to FDirectories.Count - 1 do
      begin
        FileName := FDirectories[Index];
        Delete(FileName, 1, Length(FRoot) + 1);
        DirectoryList.AddItem(FileName, TObject(Index));
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

procedure TCodePreviewForm.UpdateCaption;
var
  S: string;
begin
  S := 'CodePreview - ' + FRoot;
  if Files.FileName <> '' then
    S := 'CodePreview - ' + Files.FileName;
  Caption := S;
end;

end.

