unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.FileCtrl, System.IniFiles, ShellApi, System.Hash, System.JSON, System.Net.HttpClient,
  System.Threading, System.Generics.Collections, System.UITypes, System.IOUtils, System.Types,
  System.Net.URLClient, System.Zip, System.StrUtils, System.DateUtils, PsAPI,
  WebView2, Winapi.ActiveX, Vcl.Edge, Vcl.OleCtrls, SHDocVw, System.Win.Registry, WinInet;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    Panel2: TPanel;
    WebBrowser1: TWebBrowser;
    Panel3: TPanel;
    ProgressBar1: TProgressBar;
    statusLabel: TLabel;
    Panel4: TPanel;
    checkButton: TButton;
    stopButton: TButton;
    updateButton: TButton;
    playButton: TButton;
    autoUpdateCheckBox: TCheckBox;
    updateIntervalLabel: TLabel;
    updateIntervalEdit: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure playButtonClick(Sender: TObject);
    procedure checkButtonClick(Sender: TObject);
    procedure updateButtonClick(Sender: TObject);
    procedure stopButtonClick(Sender: TObject);
    procedure autoUpdateCheckBoxClick(Sender: TObject);
    procedure updateIntervalEditChange(Sender: TObject);
  private
    FGamePath: string;
    FFileHashes: TDictionary<string, string>;
    FScanning: Boolean;
    FServerBaseURL: string;
    FClientVersion: string;
    FServerVersion: string;
    FUpdateInProgress: Boolean;
    FPatchList: TStringList;
    FFirstRun: Boolean;
    procedure CheckGameLocation;
    function ValidateGamePath(const Path: string): Boolean;
    procedure SelectGameFolder;
    procedure SaveSettings;
    procedure LoadSettings;
    procedure UpdateUI;
    function CalculateFileHash(const FilePath: string): string;
    procedure LoadServerHashes;
    procedure CompareFiles;
    function DownloadFile(const RemotePath, LocalPath: string): Boolean;
    function CreateDirectoryForFile(const FilePath: string): Boolean;
    function CheckForUpdates: Boolean;
    function GetPatchList: Boolean;
    function DownloadAndApplyPatches: Boolean;
    procedure ApplyPatch(const PatchFile: string);
    function GetCurrentClientVersion: string;
    function GetServerVersion: string;
    function ExtractVersionFromUrl(const Url: string): string;
    procedure CheckUpdatesOnStartup;
    procedure ForceCheckUpdates;
    function IsProcessRunning(PID: DWORD): Boolean;
    function GetProcessName(hProcess: THandle): string;
    //function CheckDependencies: Boolean;
    function RunAsAdmin(const ExePath, Params: string): Boolean;
    function TryNormalLaunch(const ExePath: string): Boolean;
  public
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  SETTINGS_FILE = 'launcher.ini';
  BIN32_PATH = 'bin32\aion.bin';
  BIN64_PATH = 'bin64\aion.bin';
  SERVER_HASHES_URL = 'http://api.aion-community.ru/client/file_hashes.json';
  SERVER_BASE_URL = 'http://files.aion-community.ru/client/';
  VERSION_FILE = 'version.ini';
  SERVER_VERSION_URL = 'http://api.aion-community.ru/client/version.json';
  PATCH_LIST_URL = 'http://api.aion-community.ru/client/patches.json?current_version=';
  DEFAULT_CLIENT_VERSION = '1.0.0';
  REQUIRED_CHECK_DAYS = 14; // Интервал автоматической проверки в днях


//
//function IsEdgeChromiumInstalled: Boolean;
//var
//  Reg: TRegistry;
//begin
//  Result := False;
//  Reg := TRegistry.Create;
//  try
//    Reg.RootKey := HKEY_LOCAL_MACHINE;
//    if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\EdgeUpdate\Clients\{56EB18F8-B008-4CBD-B6D2-8C97FE7E9062}') then
//    begin
//      Result := Reg.ValueExists('pv'); // Проверяем наличие версии Edge
//     Reg.CloseKey;
//    end;
//  finally
//    Reg.Free;
//  end;
//end;

procedure ClearIECacheForURL(const URL: string);
begin
  DeleteUrlCacheEntry(PChar(URL));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FFileHashes := TDictionary<string, string>.Create;
  FPatchList := TStringList.Create;
  FScanning := False;
  FUpdateInProgress := False;
  FFirstRun := True;
  FServerBaseURL := SERVER_BASE_URL;
  Memo1.Clear;
  LoadSettings;
  CheckGameLocation;
  UpdateUI;
  stopButton.Enabled := False;
  FClientVersion := GetCurrentClientVersion;

  //if IsEdgeChromiumInstalled then
  //  EdgeBrowser1.Navigate('https://google.com')
  //else
  //  ShowMessage('Edge не установлен. Браузер может отображаться не верно.');

  ClearIECacheForURL('http://api.aion-community.ru/launcher/updater.html');
  WebBrowser1.Navigate('http://api.aion-community.ru/launcher/updater.html'); // Перезагрузит страницу без кеша
  //WebBrowser1.Navigate('http://api.aion-community.ru/launcher/updater.html');

end;


procedure TForm1.FormShow(Sender: TObject);
begin
  if FFirstRun then
  begin
    FFirstRun := False;
    ForceCheckUpdates;
  end
  else
  begin
    CheckUpdatesOnStartup;
  end;
end;

procedure TForm1.ForceCheckUpdates;
begin
  Memo1.Lines.Add('Выполняется проверка обновлений...');
  Application.ProcessMessages;

  if CheckForUpdates then
  begin
    if MessageDlg('Доступно обновление до версии ' + FServerVersion +
                 '. Установить сейчас?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      updateButton.Click;
    end;
  end
  else
  begin
    Memo1.Lines.Add('Ваша версия клиента актуальна.');
    playButton.Enabled := True;
    statusLabel.Caption := 'Статус: Можно играть';
  end;

  with TIniFile.Create(ExtractFilePath(Application.ExeName) + SETTINGS_FILE) do
  try
    WriteDateTime('Settings', 'LastUpdateCheck', Now);
  finally
    Free;
  end;
end;

procedure TForm1.CheckUpdatesOnStartup;
var
  IniFile: TIniFile;
  LastCheck: TDateTime;
  CheckIntervalHours: Integer;
  HoursSinceLastCheck: Integer;
begin
  try
    IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + SETTINGS_FILE);
    try
      LastCheck := IniFile.ReadDateTime('Settings', 'LastUpdateCheck', 0);
      HoursSinceLastCheck := HoursBetween(Now, LastCheck);

      // Обязательная проверка, если прошло более 14 дней
      if (HoursSinceLastCheck >= REQUIRED_CHECK_DAYS) then
      begin
        Memo1.Lines.Add('Обязательная проверка клиента (раз в 14 дней)...');
        Application.ProcessMessages;

        if CheckForUpdates then
        begin
          if MessageDlg('Доступно обновление до версии ' + FServerVersion +
                       '. Установить сейчас?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
          begin
            updateButton.Click;
          end;
        end;

        IniFile.WriteDateTime('Settings', 'LastUpdateCheck', Now);
        Exit;
      end;

      // Обычная проверка по настройкам пользователя
      if not IniFile.ReadBool('Settings', 'AutoCheckUpdates', True) then
      begin
        Memo1.Lines.Add('Автопроверка обновлений отключена в настройках');
        Exit;
      end;

      CheckIntervalHours := IniFile.ReadInteger('Settings', 'UpdateCheckInterval', 24);

      if (LastCheck = 0) or (HoursSinceLastCheck >= CheckIntervalHours) then
      begin
        Memo1.Lines.Add('Проверка обновлений...');
        Application.ProcessMessages;

        if CheckForUpdates then
        begin
          if MessageDlg('Доступно обновление до версии ' + FServerVersion +
                       '. Установить сейчас?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
          begin
            updateButton.Click;
          end;
        end;

        IniFile.WriteDateTime('Settings', 'LastUpdateCheck', Now);
      end
      else
      begin
        Memo1.Lines.Add('Следующая проверка через ' +
          IntToStr(CheckIntervalHours - HoursSinceLastCheck) + ' ч.');
      end;
    finally
      IniFile.Free;
    end;
  except
    on E: Exception do
      Memo1.Lines.Add('Ошибка проверки обновлений: ' + E.Message);
  end;
end;

procedure TForm1.autoUpdateCheckBoxClick(Sender: TObject);
begin
  with TIniFile.Create(ExtractFilePath(Application.ExeName) + SETTINGS_FILE) do
  try
    WriteBool('Settings', 'AutoCheckUpdates', autoUpdateCheckBox.Checked);
  finally
    Free;
  end;
end;

procedure TForm1.UpdateIntervalEditChange(Sender: TObject);
var
  Value: Integer;
begin
  Value := StrToIntDef(updateIntervalEdit.Text, 24);
  if Value < 1 then Value := 1;
  if Value > 168 then Value := 168; // Не более 1 недели

  with TIniFile.Create(ExtractFilePath(Application.ExeName) + SETTINGS_FILE) do
  try
    WriteInteger('Settings', 'UpdateCheckInterval', Value);
    updateIntervalEdit.Text := IntToStr(Value);
  finally
    Free;
  end;
end;

destructor TForm1.Destroy;
begin
  FFileHashes.Free;
  FPatchList.Free;
  inherited;
end;

function TForm1.GetCurrentClientVersion: string;
var
  IniFile: TIniFile;
begin
  Result := DEFAULT_CLIENT_VERSION;
  if FGamePath = '' then Exit;

  try
    IniFile := TIniFile.Create(IncludeTrailingPathDelimiter(FGamePath) + VERSION_FILE);
    try
      Result := IniFile.ReadString('Version', 'Client', DEFAULT_CLIENT_VERSION);
    finally
      IniFile.Free;
    end;
  except
    on E: Exception do
      Memo1.Lines.Add('Ошибка чтения версии клиента: ' + E.Message);
  end;
end;

function TForm1.GetServerVersion: string;
var
  HTTPClient: THTTPClient;
  Response: IHTTPResponse;
  JSONValue: TJSONValue;
begin
  Result := '';
  HTTPClient := THTTPClient.Create;
  try
    try
      HTTPClient.ConnectionTimeout := 5000;
      HTTPClient.ResponseTimeout := 10000;
      Response := HTTPClient.Get(SERVER_VERSION_URL);

      if Response.StatusCode = 200 then
      begin
        JSONValue := TJSONObject.ParseJSONValue(Response.ContentAsString);
        if Assigned(JSONValue) then
        try
          if JSONValue is TJSONObject then
            Result := (JSONValue as TJSONObject).GetValue('version').Value;
        finally
          JSONValue.Free;
        end;
      end
      else
      begin
        Memo1.Lines.Add('Ошибка получения версии сервера: HTTP ' + IntToStr(Response.StatusCode));
      end;
    except
      on E: Exception do
        Memo1.Lines.Add('Ошибка получения версии сервера: ' + E.Message);
    end;
  finally
    HTTPClient.Free;
  end;
end;

function TForm1.ExtractVersionFromUrl(const Url: string): string;
var
  StartPos, EndPos: Integer;
begin
  StartPos := Pos('to_', Url) + 3;
  EndPos := Pos('.zip', Url);
  if (StartPos > 3) and (EndPos > StartPos) then
    Result := Copy(Url, StartPos, EndPos - StartPos)
  else
    Result := '';
end;

function TForm1.CheckForUpdates: Boolean;
begin
  Result := False;
  try
    FServerVersion := GetServerVersion;

    if FServerVersion = '' then
    begin
      Memo1.Lines.Add('Не удалось получить версию сервера');
      Exit;
    end;

    Memo1.Lines.Add('Текущая версия: ' + FClientVersion);
    Memo1.Lines.Add('Актуальная версия: ' + FServerVersion);

    Result := CompareText(FClientVersion, FServerVersion) <> 0;
    if Result then
      statusLabel.Caption := 'Статус: Требуется обновление'
    else
      Memo1.Lines.Add('У вас актуальная версия');
      playButton.Enabled := True;
      statusLabel.Caption := 'Статус: Можно играть';
  except
    on E: Exception do
      Memo1.Lines.Add('Ошибка проверки обновлений: ' + E.Message);
  end;
end;

function TForm1.GetPatchList: Boolean;
var
  HTTPClient: THTTPClient;
  Response: IHTTPResponse;
  JSONValue: TJSONValue;
  JSONArray: TJSONArray;
  i: Integer;
begin
  Result := False;
  FPatchList.Clear;

  HTTPClient := THTTPClient.Create;
  try
    try
      HTTPClient.ConnectionTimeout := 5000;
      HTTPClient.ResponseTimeout := 10000;
      Response := HTTPClient.Get(PATCH_LIST_URL + FClientVersion);

      if Response.StatusCode = 200 then
      begin
        JSONValue := TJSONObject.ParseJSONValue(Response.ContentAsString);
        if Assigned(JSONValue) then
        try
          if JSONValue is TJSONObject then
          begin
            JSONArray := (JSONValue as TJSONObject).GetValue('patches') as TJSONArray;
            for i := 0 to JSONArray.Count - 1 do
            begin
              FPatchList.Add((JSONArray.Items[i] as TJSONObject).GetValue('url').Value);
            end;
            Result := True;
            Memo1.Lines.Add('Получено патчей: ' + IntToStr(FPatchList.Count));
          end;
        finally
          JSONValue.Free;
        end;
      end
      else
      begin
        Memo1.Lines.Add('Ошибка получения списка патчей: HTTP ' + IntToStr(Response.StatusCode));
      end;
    except
      on E: Exception do
        Memo1.Lines.Add('Ошибка получения списка патчей: ' + E.Message);
    end;
  finally
    HTTPClient.Free;
  end;
end;

function TForm1.DownloadAndApplyPatches: Boolean;
var
  i: Integer;
  PatchFile: string;
  PatchURL: string;
  NewVersion: string;
begin
  Result := True;

  ProgressBar1.Max := FPatchList.Count * 2;
  ProgressBar1.Position := 0;

  for i := 0 to FPatchList.Count - 1 do
  begin
    if not FScanning then
    begin
      Result := False;
      Memo1.Lines.Add('Обновление прервано');
      Break;
    end;

    PatchURL := FPatchList[i];
    PatchFile := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'patch_' + IntToStr(i) + '.zip';

    Memo1.Lines.Add(Format('[%d/%d] Загрузка: %s', [i+1, FPatchList.Count, PatchURL]));

    if not DownloadFile(PatchURL, PatchFile) then
    begin
      Memo1.Lines.Add('Ошибка загрузки');
      Result := False;
      Break;
    end;
    ProgressBar1.Position := ProgressBar1.Position + 1;
    Application.ProcessMessages;

    Memo1.Lines.Add('Применение патча...');
    ApplyPatch(PatchFile);
    ProgressBar1.Position := ProgressBar1.Position + 1;
    Application.ProcessMessages;

    NewVersion := ExtractVersionFromUrl(PatchURL);
    if NewVersion <> '' then
    begin
      FClientVersion := NewVersion;
      with TIniFile.Create(IncludeTrailingPathDelimiter(FGamePath) + VERSION_FILE) do
      try
        WriteString('Version', 'Client', FClientVersion);
      finally
        Free;
      end;
      Memo1.Lines.Add('Версия обновлена: ' + FClientVersion);
    end;
  end;
end;

procedure TForm1.ApplyPatch(const PatchFile: string);
var
  ZipFile: TZipFile;
  i: Integer;
  FileName: string;
  OutputPath: string;
begin
  if not FileExists(PatchFile) then
  begin
    Memo1.Lines.Add('Файл патча не найден: ' + PatchFile);
    Exit;
  end;

  ZipFile := TZipFile.Create;
  try
    try
      ZipFile.Open(PatchFile, zmRead);

      for i := 0 to ZipFile.FileCount - 1 do
      begin
        if not FScanning then Break;

        FileName := ZipFile.FileNames[i];
        OutputPath := IncludeTrailingPathDelimiter(FGamePath) + FileName;

        Memo1.Lines.Add('Обновление: ' + FileName);
        if not CreateDirectoryForFile(OutputPath) then
        begin
          Memo1.Lines.Add('Ошибка создания директории для: ' + FileName);
          Continue;
        end;

        if FileExists(OutputPath) then
          DeleteFile(OutputPath);

        try
          ZipFile.Extract(i, OutputPath, False);
        except
          on E: Exception do
            Memo1.Lines.Add('Ошибка распаковки файла ' + FileName + ': ' + E.Message);
        end;
        Application.ProcessMessages;
      end;

      Memo1.Lines.Add('Патч применен');
    except
      on E: Exception do
        Memo1.Lines.Add('Ошибка распаковки: ' + E.Message);
    end;
  finally
    ZipFile.Free;
    if FileExists(PatchFile) then
      DeleteFile(PatchFile);
  end;
end;

procedure TForm1.updateButtonClick(Sender: TObject);
begin
  if FUpdateInProgress then Exit;

  FUpdateInProgress := True;
  try
    Memo1.Lines.Add('--- Проверка обновлений ---');

    if not CheckForUpdates then
    begin
      Memo1.Lines.Add('Обновления не требуются');
      Exit;
    end;

    updateButton.Enabled := False;
    stopButton.Enabled := True;
    FScanning := True;

    if not GetPatchList then
    begin
      Memo1.Lines.Add('Не удалось получить список патчей');
      Exit;
    end;

    if FPatchList.Count = 0 then
    begin
      Memo1.Lines.Add('Нет доступных патчей');
      Exit;
    end;

    if DownloadAndApplyPatches then
    begin
      Memo1.Lines.Add('Все патчи успешно применены!');
      Memo1.Lines.Add('Новая версия: ' + FClientVersion);
    end;

    Memo1.Lines.Add('--- Обновление завершено ---');
  finally
    FUpdateInProgress := False;
    updateButton.Enabled := True;
    stopButton.Enabled := False;
    FScanning := False;
    ProgressBar1.Position := 0;
  end;
end;

procedure TForm1.CheckGameLocation;
var
  CurrentDir: string;
begin
  CurrentDir := ExtractFilePath(Application.ExeName);

  if ValidateGamePath(CurrentDir) then
  begin
    FGamePath := CurrentDir;
    SaveSettings;
    Memo1.Lines.Add('Игра найдена: ' + FGamePath);
    Exit;
  end;

  if (FGamePath <> '') and ValidateGamePath(FGamePath) then
  begin
    Memo1.Lines.Add('Используется сохраненный путь: ' + FGamePath);
    Exit;
  end;

  Memo1.Lines.Add('Игра не найдена. Укажите папку с игрой.');
  SelectGameFolder;
end;

function TForm1.ValidateGamePath(const Path: string): Boolean;
var
  FullPath: string;
begin
  FullPath := IncludeTrailingPathDelimiter(Path);
  Result := FileExists(FullPath + BIN32_PATH) or
            FileExists(FullPath + BIN64_PATH) or
            FileExists(FullPath + 'bin32\Aion.bin') or
            FileExists(FullPath + 'bin64\Aion.bin');
end;

procedure TForm1.SelectGameFolder;
var
  DirSelected: Boolean;
  SelectedPath: string;
begin
  DirSelected := SelectDirectory('Выберите папку с игрой', '', SelectedPath,
    [sdNewUI, sdNewFolder, sdShowEdit, sdValidateDir], Self);

  if DirSelected then
  begin
    SelectedPath := IncludeTrailingPathDelimiter(SelectedPath);
    if ValidateGamePath(SelectedPath) then
    begin
      FGamePath := SelectedPath;
      SaveSettings;
      Memo1.Lines.Add('Папка выбрана: ' + FGamePath);
      UpdateUI;
    end
    else
    begin
      MessageDlg('Не найдены файлы игры. Выберите другую папку.', mtError, [mbOK], 0);
      SelectGameFolder;
    end;
  end
  else if FGamePath = '' then
  begin
    if MessageDlg('Без указания папки лаунчер не может работать. Закрыть программу?',
       mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      Application.Terminate
    else
      SelectGameFolder;
  end;
end;

procedure TForm1.SaveSettings;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + SETTINGS_FILE);
  try
    IniFile.WriteString('Settings', 'GamePath', FGamePath);
    IniFile.WriteBool('Settings', 'AutoCheckUpdates', autoUpdateCheckBox.Checked);
    IniFile.WriteInteger('Settings', 'UpdateCheckInterval', StrToIntDef(updateIntervalEdit.Text, 24));
  finally
    IniFile.Free;
  end;
end;

procedure TForm1.LoadSettings;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + SETTINGS_FILE);
  try
    FGamePath := IniFile.ReadString('Settings', 'GamePath', '');
    autoUpdateCheckBox.Checked := IniFile.ReadBool('Settings', 'AutoCheckUpdates', True);
    updateIntervalEdit.Text := IniFile.ReadInteger('Settings', 'UpdateCheckInterval', 24).ToString;
  finally
    IniFile.Free;
  end;
end;

procedure TForm1.UpdateUI;
begin
  playButton.Enabled := (FGamePath <> '') and ValidateGamePath(FGamePath);
  checkButton.Enabled := playButton.Enabled;
  updateButton.Enabled := playButton.Enabled;
end;

function TForm1.CalculateFileHash(const FilePath: string): string;
var
  FileStream: TFileStream;
begin
  Result := '';
  try
    FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
    try
      Result := THashMD5.GetHashString(FileStream);
    finally
      FileStream.Free;
    end;
  except
    on E: Exception do
      Memo1.Lines.Add('Ошибка вычисления хэша файла ' + FilePath + ': ' + E.Message);
  end;
end;

procedure TForm1.LoadServerHashes;
var
  HTTPClient: THTTPClient;
  Response: IHTTPResponse;
  JSONValue: TJSONValue;
  JSONArray: TJSONArray;
  JSONItem: TJSONValue;
  i: Integer;
begin
  Memo1.Lines.Add('Загрузка хэшей с сервера...');
  Application.ProcessMessages;

  HTTPClient := THTTPClient.Create;
  try
    try
      HTTPClient.ConnectionTimeout := 5000;
      HTTPClient.ResponseTimeout := 10000;
      Response := HTTPClient.Get(SERVER_HASHES_URL);

      if Response.StatusCode = 200 then
      begin
        JSONValue := TJSONObject.ParseJSONValue(Response.ContentAsString);
        if Assigned(JSONValue) then
        try
          if JSONValue is TJSONObject then
          begin
            JSONItem := (JSONValue as TJSONObject).GetValue('files');
            if Assigned(JSONItem) and (JSONItem is TJSONArray) then
            begin
              JSONArray := JSONItem as TJSONArray;
              FFileHashes.Clear;

              for i := 0 to JSONArray.Count - 1 do
              begin
                JSONItem := JSONArray.Items[i];
                if JSONItem is TJSONObject then
                begin
                  FFileHashes.Add(
                    (JSONItem as TJSONObject).GetValue('path').Value,
                    (JSONItem as TJSONObject).GetValue('hash').Value
                  );
                end;
              end;
              Memo1.Lines.Add('Загружено хэшей: ' + IntToStr(FFileHashes.Count));
            end;
          end;
        finally
          JSONValue.Free;
        end;
      end
      else
        raise Exception.Create('Ошибка загрузки: ' + IntToStr(Response.StatusCode));
    except
      on E: Exception do
      begin
        Memo1.Lines.Add('Ошибка загрузки хэшей: ' + E.Message);
        raise;
      end;
    end;
  finally
    HTTPClient.Free;
  end;
end;

function TForm1.CreateDirectoryForFile(const FilePath: string): Boolean;
var
  Dir: string;
begin
  Dir := ExtractFilePath(FilePath);
  if not DirectoryExists(Dir) then
    Result := ForceDirectories(Dir)
  else
    Result := True;
end;

function TForm1.DownloadFile(const RemotePath, LocalPath: string): Boolean;
var
  HTTPClient: THTTPClient;
  Response: IHTTPResponse;
  FileStream: TFileStream;
  RemoteURL: string;
  FixedRemotePath: string;
begin
  Result := False;
  if not CreateDirectoryForFile(LocalPath) then
  begin
    Memo1.Lines.Add('Ошибка создания директории: ' + LocalPath);
    Exit;
  end;

  FixedRemotePath := StringReplace(RemotePath, '\', '/', [rfReplaceAll]);
  RemoteURL := FixedRemotePath;

  Memo1.Lines.Add('Скачивание: ' + RemoteURL + ' -> ' + LocalPath);

  HTTPClient := THTTPClient.Create;
  try
    try
      HTTPClient.HandleRedirects := True;
      HTTPClient.ConnectionTimeout := 10000;
      HTTPClient.ResponseTimeout := 30000;

      FileStream := TFileStream.Create(LocalPath, fmCreate);
      try
        Response := HTTPClient.Get(RemoteURL, FileStream);
        if Response.StatusCode = 200 then
        begin
          Result := True;
          Memo1.Lines.Add('Успешно скачан: ' + LocalPath);
        end
        else
        begin
          Memo1.Lines.Add('Ошибка скачивания (' + IntToStr(Response.StatusCode) + '): ' + LocalPath);
          if FileExists(LocalPath) then
            DeleteFile(LocalPath);
        end;
      finally
        FileStream.Free;
      end;
    except
      on E: Exception do
      begin
        Memo1.Lines.Add('Ошибка скачивания: ' + E.Message);
        if FileExists(LocalPath) then
          DeleteFile(LocalPath);
      end;
    end;
  finally
    HTTPClient.Free;
  end;
end;

procedure TForm1.CompareFiles;
var
  Files: TStringDynArray;
  FilePath, RelativePath, ServerHash, LocalHash: string;
  MismatchCount, FixedCount: Integer;
  LocalFilePath: string;
begin
  if FFileHashes.Count = 0 then
  begin
    Memo1.Lines.Add('Нет данных о хэшах файлов');
    Exit;
  end;

  Memo1.Lines.Add('Начало проверки файлов...');
  ProgressBar1.Position := 0;
  ProgressBar1.Max := FFileHashes.Count;
  MismatchCount := 0;
  FixedCount := 0;
  FScanning := True;
  stopButton.Enabled := True;
  checkButton.Enabled := False;

  try
    Files := TDirectory.GetFiles(FGamePath, '*.*', TSearchOption.soAllDirectories);
    for FilePath in Files do
    begin
      if not FScanning then
      begin
        statusLabel.Caption := 'Статус: Требуется проверка клиента';
        Memo1.Lines.Add('Проверка прервана');
        playButton.Enabled := False;
        Break;
      end;

      RelativePath := ExtractRelativePath(FGamePath, FilePath);

      if FFileHashes.TryGetValue(RelativePath, ServerHash) then
      begin
        LocalHash := CalculateFileHash(FilePath);

        if not SameText(LocalHash, ServerHash) then
        begin
          Inc(MismatchCount);
          Memo1.Lines.Add(Format('Несоответствие: %s (ожидалось: %s, найдено: %s)',
            [RelativePath, ServerHash, LocalHash]));

          LocalFilePath := IncludeTrailingPathDelimiter(FGamePath) + RelativePath;
          if DownloadFile(FServerBaseURL + RelativePath, LocalFilePath) then
          begin
            Inc(FixedCount);
            Memo1.Lines.Add('Файл заменен: ' + RelativePath);
          end;
        end;
      end;

      ProgressBar1.Position := ProgressBar1.Position + 1;
      Application.ProcessMessages;
    end;

    if FScanning then
    begin
      Memo1.Lines.Add(Format('Проверка завершена. Несоответствий: %d из %d', [MismatchCount, FFileHashes.Count]));
      Memo1.Lines.Add(Format('Исправлено файлов: %d', [FixedCount]));
    end;
  finally
    FScanning := False;
    stopButton.Enabled := False;
    checkButton.Enabled := True;
  end;
end;

procedure TForm1.checkButtonClick(Sender: TObject);
begin
  checkButton.Enabled := False;
  try
    statusLabel.Caption := 'Статус: Проверка клиента...';
    Memo1.Lines.Add('--- Начало проверки ---');
    playButton.Enabled := False;
    LoadServerHashes;
    CompareFiles;
    Memo1.Lines.Add('--- Проверка завершена ---');
    statusLabel.Caption := 'Статус: Можно играть...';
    playButton.Enabled := True;
  finally
    checkButton.Enabled := True;
  end;
end;

function TForm1.IsProcessRunning(PID: DWORD): Boolean;
var
  hProcess: THandle;
  ExitCode: DWORD;
begin
  Result := False;
  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION, False, PID);
  if hProcess <> 0 then
  try
    if GetExitCodeProcess(hProcess, ExitCode) then
      Result := (ExitCode = STILL_ACTIVE);
  finally
    CloseHandle(hProcess);
  end;
end;

function TForm1.GetProcessName(hProcess: THandle): string;
var
  ModName: array[0..MAX_PATH] of Char;
begin
  if GetModuleFileNameEx(hProcess, 0, ModName, MAX_PATH) <> 0 then
    Result := ModName
  else
    Result := 'Не удалось получить имя';
end;

//function TForm1.CheckDependencies: Boolean;
//var
//  Dependencies: array[0..2] of string; // Статический массив фиксированного размера
//  I: Integer;
//begin
//  Result := True;
//  Dependencies[0] := 'vcruntime140.dll';
//  Dependencies[1] := 'd3dx9_43.dll';
//  Dependencies[2] := 'xinput1_3.dll';

//  for I := 0 to High(Dependencies) do
//  begin
//    if GetModuleHandle(PChar(Dependencies[I])) = 0 then
//    begin
//      Memo1.Lines.Add('Не найдена зависимость: ' + Dependencies[I]);
//      Result := False;
//    end;
//  end;
//end;

function TForm1.RunAsAdmin(const ExePath, Params: string): Boolean;
var
  SEInfo: TShellExecuteInfo;
begin
  ZeroMemory(@SEInfo, SizeOf(SEInfo));
  SEInfo.cbSize := SizeOf(SEInfo);
  SEInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
  SEInfo.lpVerb := 'runas';
  SEInfo.lpFile := PChar(ExePath);
  SEInfo.lpParameters := PChar(Params);
  SEInfo.nShow := SW_SHOWNORMAL;

  Result := ShellExecuteEx(@SEInfo);
  if Result then
    WaitForSingleObject(SEInfo.hProcess, INFINITE);
end;

procedure TForm1.playButtonClick(Sender: TObject);
var
  GameExe: string;
begin
  GameExe := IncludeTrailingPathDelimiter(FGamePath) + 'bin64\aion.bin';

  if not FileExists(GameExe) then
  begin
    GameExe := IncludeTrailingPathDelimiter(FGamePath) + 'bin32\aion.bin';
    if not FileExists(GameExe) then
    begin
      MessageDlg('Файл игры не найден!', mtError, [mbOK], 0);
      Exit;
    end;
  end;

  // Проверка зависимостей
  //if not CheckDependencies then
  //begin
  //  if MessageDlg('Не найдены необходимые DLL-библиотеки. Запустить игру все равно?',
  //     mtWarning, [mbYes, mbNo], 0) = mrNo then
  //    Exit;
  //end;

  // Попробуем сначала обычный запуск
  if not TryNormalLaunch(GameExe) then
  begin
    // Если не получилось, пробуем с правами админа
    Memo1.Lines.Add('Пробуем запуск с правами администратора...');
    if not RunAsAdmin(GameExe, '-ip:127.0.0.1 -port:2106 -cc:1 -lang:enu -noweb -nowebshop -nokicks -noauthgg -charnamemenu -ingameshop -win10-mouse-fix-autodetect -disable-xigncode') then
      MessageDlg('Не удалось запустить игру', mtError, [mbOK], 0);
  end;
end;

function TForm1.TryNormalLaunch(const ExePath: string): Boolean;
var
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
  ExitCode: DWORD;
begin
  ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_SHOWNORMAL;

  Result := CreateProcess(
    nil,
    PChar('"' + ExePath + '" -ip:127.0.0.1 -port:2106 -lang:enu'),
    nil,
    nil,
    False,
    CREATE_NEW_CONSOLE,
    nil,
    PChar(ExtractFilePath(ExePath)),
    StartupInfo,
    ProcessInfo);

  if Result then
  begin
    WaitForSingleObject(ProcessInfo.hProcess, 5000);
    GetExitCodeProcess(ProcessInfo.hProcess, ExitCode);

    // Коды ошибок Windows
    case ExitCode of
      STILL_ACTIVE: Memo1.Lines.Add('Игра запущена успешно');
      3221225477: Memo1.Lines.Add('Ошибка: ACCESS_VIOLATION - проверьте зависимости');
      3221225786: Memo1.Lines.Add('Ошибка: DLL_NOT_FOUND');
      else Memo1.Lines.Add('Код выхода: ' + IntToStr(ExitCode));
    end;

    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ProcessInfo.hProcess);
  end;
end;

procedure TForm1.stopButtonClick(Sender: TObject);
begin
  if FScanning then
  begin
    FScanning := False;
    Memo1.Lines.Add('Остановка проверки...');
    stopButton.Enabled := False;
    playButton.Enabled := True;
  end;
end;

end.
