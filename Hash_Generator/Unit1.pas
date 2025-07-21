unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  System.IOUtils, System.Hash, System.JSON, System.Threading, System.SyncObjs, System.DateUtils;

type
  TForm1 = class(TForm)
    ButtonScan: TButton;
    MemoLog: TMemo;
    ProgressBar1: TProgressBar;
    procedure ButtonScanClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TFileHashItem = record
    FilePath: string;
    Hash: string;
  end;

  TFileHashList = array of TFileHashItem;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function IsIgnoredFile(const FileName: string): Boolean;
begin
  Result := (ExtractFileExt(FileName) = '.tmp') or
            (ExtractFileExt(FileName) = '.log') or
            (ExtractFileExt(FileName) = '.bak');
end;

function CalculateFileHash(const FilePath: string): string;
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
  try
    Result := THashMD5.GetHashString(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure SaveHashesToJSON(const FileHashes: TFileHashList; const OutputFile: string);
var
  JSONRoot: TJSONObject;
  JSONFiles: TJSONArray;
  i: Integer;
begin
  JSONRoot := TJSONObject.Create;
  try
    JSONFiles := TJSONArray.Create;

    for i := 0 to High(FileHashes) do
    begin
      JSONFiles.AddElement(
        TJSONObject.Create
          .AddPair('path', FileHashes[i].FilePath)
          .AddPair('hash', FileHashes[i].Hash)
      );
    end;

    JSONRoot.AddPair('files', JSONFiles);
    TFile.WriteAllText(OutputFile, JSONRoot.Format(2));
  finally
    JSONRoot.Free;
  end;
end;

procedure ScanAndHashFiles(const FolderPath: string; Memo: TMemo; ProgressBar: TProgressBar);
var
  Files: TStringList;
  FileHashes: TFileHashList;
  TotalFiles: Integer;
  BaseDir: string;
  ProcessedCount: Integer;
  StartTime: TDateTime;
  i: Integer;
  FileArray: TArray<string>;
  ElapsedSeconds: Double;
begin
  StartTime := Now;
  BaseDir := IncludeTrailingPathDelimiter(FolderPath);

  // Получаем список файлов
  Files := TStringList.Create;
  try
    FileArray := TDirectory.GetFiles(FolderPath, '*.*', TSearchOption.soAllDirectories);
    for i := 0 to High(FileArray) do
      Files.Add(FileArray[i]);

    // Фильтрация файлов
    TotalFiles := 0;
    SetLength(FileHashes, Files.Count);
    for i := 0 to Files.Count - 1 do
    begin
      if not IsIgnoredFile(Files[i]) then
      begin
        FileHashes[TotalFiles].FilePath := Files[i];
        Inc(TotalFiles);
      end;
    end;
    SetLength(FileHashes, TotalFiles);
  finally
    Files.Free;
  end;

  // Обновляем UI
  TThread.Synchronize(nil,
    procedure
    begin
      ProgressBar.Max := TotalFiles;
      ProgressBar.Position := 0;
      Memo.Lines.Add('Начато сканирование...');
      Memo.Lines.Add(Format('Обрабатывается %d файлов...', [TotalFiles]));
      Application.ProcessMessages;
    end);

  ProcessedCount := 0;

  // Параллельная обработка файлов
  TParallel.For(0, TotalFiles - 1,
    procedure(Index: Integer)
    var
      RelativePath: string;
    begin
      RelativePath := ExtractRelativePath(BaseDir, FileHashes[Index].FilePath);
      FileHashes[Index].Hash := CalculateFileHash(FileHashes[Index].FilePath);
      FileHashes[Index].FilePath := RelativePath;

      // Обновляем прогресс каждые 50 файлов
      if TInterlocked.Increment(ProcessedCount) mod 50 = 0 then
        TThread.Queue(nil,
          procedure
          begin
            ProgressBar.Position := ProcessedCount;
          end);
    end);

  // Сохраняем результаты
  SaveHashesToJSON(FileHashes, TPath.Combine(FolderPath, 'file_hashes.json'));

  // Вычисляем время выполнения
  ElapsedSeconds := (Now - StartTime) * SecsPerDay;

  // Финальное обновление UI
  TThread.Synchronize(nil,
    procedure
    begin
      ProgressBar.Position := TotalFiles;
      Memo.Lines.Add(Format('Готово! Обработано %d файлов', [TotalFiles]));
      Memo.Lines.Add(Format('Время выполнения: %.2f секунд', [ElapsedSeconds]));
      Memo.Lines.Add('Результаты сохранены в file_hashes.json');
    end);
end;

procedure TForm1.ButtonScanClick(Sender: TObject);
var
  GameFolder: string;
begin
  GameFolder := ExtractFilePath(Application.ExeName);
  ButtonScan.Enabled := False;
  MemoLog.Clear;
  ProgressBar1.Position := 0;

  // Запускаем в отдельном потоке
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        ScanAndHashFiles(GameFolder, MemoLog, ProgressBar1);
      except
        on E: Exception do
          TThread.Synchronize(nil,
            procedure
            begin
              MemoLog.Lines.Add('Ошибка: ' + E.Message);
            end);
      end;

      TThread.Synchronize(nil,
        procedure
        begin
          ButtonScan.Enabled := True;
        end);
    end).Start;
end;

end.
