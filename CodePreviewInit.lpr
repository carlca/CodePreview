program CodePreviewInit;

{$mode objfpc}{$H+}

{$DEFINE XCODE}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  cajsonconfig,
  LazUtf8,
  SysUtils
  { you can add units after this };

function GetConfigPath: string;
begin
  {$IFDEF XCODE}
  Result := '/users/carlca/Code/fpc/CodePreview/config.json';
  {$ELSE}
  Result := Utils.AppPath + 'config.json';
  {$ENDIF}
end;

procedure CreateConfig;
var
  C: TcaJsonConfig;
begin
  C := TcaJsonConfig.Create(nil);
  try
    C.FileName := GetConfigPath;
    // mainForm
    C.Group := 'mainForm';
    C.IntProp['left'] := 500;
    C.IntProp['top'] := 50;
    C.IntProp['width'] := 1000;
    C.IntProp['height'] := 600;
    // paths
    C.Group := 'paths';
    C.StrProp['gopath'] := GetEnvironmentVariableUtf8('GOPATH');
    C.StrProp['goroot'] := GetEnvironmentVariableUtf8('GOROOT');
    C.StrProp['root'] := '';
    // filesList
    C.Group := 'filesList';
    C.IntProp['height'] := 250;
    C.StrProp['fontName'] := 'Lucida Grande';
    C.IntProp['fontSize'] := 10;
    // leftPanel
    C.Group := 'leftPanel';
    C.IntProp['width'] := 350;
    // foldersList
    C.Group := 'foldersList';
    C.StrProp['fontName'] := 'Lucida Grande';
    C.IntProp['fontSize'] := 10;
    // golangEdit
    C.Group := 'golangEdit';
    C.StrProp['fontName'] := 'Courier New';
    C.IntProp['fontSize'] := 10;
  finally
    C.Free;
  end;
end;

begin
  CreateConfig;
end.

