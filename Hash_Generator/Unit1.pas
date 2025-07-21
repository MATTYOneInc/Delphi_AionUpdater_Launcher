unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  System.IOUtils, System.Hash, System.JSON, System.Threading, System.SyncObjs, System.DateUtils;

type
  TForm1 = class(TForm)
    ButtonScan: TButton;
    ButtonCancel: TButton;
    MemoLog: TMemo;
    ProgressBar1: TProgressBar;
    procedure ButtonScanClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCancelOperation: Boolean;
    FIsProcessing: Boolean;
    procedure UpdateUI;
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCancelOperation := False;
  FIsProcessing := False;
  ButtonCancel.Enabled := False;
end;

procedure TForm1.UpdateUI;
begin
  ButtonScan.Enabled := not FIsProcessing;
  ButtonCancel.Enabled := FIsProcessing;
end;

function IsIgnoredFile(const FileName: string): Boolean;
begin
  Result := (ExtractFileExt(FileName) = '.tmp') or
            (ExtractFileExt(FileName) = '.log') or
            (ExtractFileExt(FileName) = '.bak');
end;

function CalculateFileHash(const FilePath: string; out ErrorMsg: string): string;
var
  FileStream: TFileStream;
begin
  Result := '';
  ErrorMsg := '';

  if not FileExists(FilePath) then
  begin
    ErrorMsg := 'Файл не существует';
    Exit;
  end;

  try
    FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
    try
      Result := THashMD5.GetHashString(FileStream);
    finally
      FileStream.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;
end;

procedure SaveHashesToJSON(const FileHashes: TFileHashList; const OutputFile: string);
var
  JSONRoot: TJSONObject;
  JSONFiles: TJSONArray;
  i: Integer;
  StreamWriter: TStreamWriter;
begin
  JSONRoot := TJSONObject.Create;
  try
    JSONFiles := TJSONArray.Create;

    for i := 0 to High(FileHashes) do
    begin
      if FileHashes[i].Hash <> '' then // Пропускаем файлы с ошибками
      begin
        JSONFiles.AddElement(
          TJSONObject.Create
            .AddPair('path', FileHashes[i].FilePath)
            .AddPair('hash', FileHashes[i].Hash)
        );
      end;
    end;

    JSONRoot.AddPair('files', JSONFiles);

    // Оптимизированное сохранение больших JSON
    StreamWriter := TStreamWriter.Create(OutputFile, False, TEncoding.UTF8);
    try
      StreamWriter.Write(JSONRoot.Format(2));
    finally
      StreamWriter.Free;
    end;
  finally
    JSONRoot.Free;
  end;
end;

procedure TForm1.ButtonScanClick(Sender: TObject);
var
  GameFolder: string;
begin
  GameFolder := ExtractFilePath(Application.ExeName);
  FCancelOperation := False;
  FIsProcessing := True;
  UpdateUI;

  MemoLog.Clear;
  ProgressBar1.Position := 0;

  // Запускаем в отдельном потоке
  TThread.CreateAnonymousThread(
    procedure
    var
      Files: TArray<string>;
      FileHashes: TFileHashList;
      TotalFiles: Integer;
      BaseDir: string;
      ProcessedCount: Integer;
      StartTime: TDateTime;
      i: Integer;
      ElapsedSeconds: Double;
      ErrorMsg: string;
    begin
      try
        StartTime := Now;
        BaseDir := IncludeTrailingPathDelimiter(GameFolder);

        // Получаем список файлов
        Files := TDirectory.GetFiles(GameFolder, '*.*', TSearchOption.soAllDirectories);

        // Фильтрация файлов
        TotalFiles := 0;
        SetLength(FileHashes, Length(Files));
        for i := 0 to High(Files) do
        begin
          if FCancelOperation then Break;

          if not IsIgnoredFile(Files[i]) then
          begin
            FileHashes[TotalFiles].FilePath := Files[i];
            Inc(TotalFiles);
          end;
        end;

        if FCancelOperation then
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              MemoLog.Lines.Add('Операция отменена пользователем');
            end);
          Exit;
        end;

        SetLength(FileHashes, TotalFiles);

        // Обновляем UI
        TThread.Synchronize(nil,
          procedure
          begin
            ProgressBar1.Max := TotalFiles;
            ProgressBar1.Position := 0;
            MemoLog.Lines.Add('Начато сканирование...');
            MemoLog.Lines.Add(Format('Обрабатывается %d файлов...', [TotalFiles]));
            Application.ProcessMessages;
          end);

        ProcessedCount := 0;

        // Параллельная обработка файлов
        TParallel.For(0, TotalFiles - 1,
          procedure(Index: Integer)
          var
            RelativePath, ErrorMsg: string;
          begin
            if FCancelOperation then
              Exit;

            FileHashes[Index].Hash := CalculateFileHash(FileHashes[Index].FilePath, ErrorMsg);

            if ErrorMsg <> '' then
            begin
              TThread.Queue(nil,
                procedure
                begin
                  MemoLog.Lines.Add('Ошибка: ' + FileHashes[Index].FilePath);
                  MemoLog.Lines.Add('  ' + ErrorMsg);
                end);
            end
            else
            begin
              RelativePath := ExtractRelativePath(BaseDir, FileHashes[Index].FilePath);
              FileHashes[Index].FilePath := RelativePath;
            end;

            // Обновляем прогресс каждые 50 файлов
            if TInterlocked.Increment(ProcessedCount) mod 50 = 0 then
              TThread.Queue(nil,
                procedure
                begin
                  ProgressBar1.Position := ProcessedCount;
                end);
          end);

        if FCancelOperation then
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              MemoLog.Lines.Add('Операция отменена пользователем');
            end);
          Exit;
        end;

        // Сохраняем результаты
        SaveHashesToJSON(FileHashes, TPath.Combine(GameFolder, 'file_hashes.json'));

        // Вычисляем время выполнения
        ElapsedSeconds := (Now - StartTime) * SecsPerDay;

        // Финальное обновление UI
        TThread.Synchronize(nil,
          procedure
          begin
            ProgressBar1.Position := TotalFiles;
            MemoLog.Lines.Add(Format('Готово! Обработано %d файлов', [TotalFiles]));
            MemoLog.Lines.Add(Format('Время выполнения: %.2f секунд', [ElapsedSeconds]));
            MemoLog.Lines.Add('Результаты сохранены в file_hashes.json');
          end);
      except
        on E: Exception do
          TThread.Synchronize(nil,
            procedure
            begin
              MemoLog.Lines.Add('Критическая ошибка: ' + E.Message);
            end);
      end;

      TThread.Synchronize(nil,
        procedure
        begin
          FIsProcessing := False;
          UpdateUI;
        end);
    end).Start;
end;

procedure TForm1.ButtonCancelClick(Sender: TObject);
begin
  if FIsProcessing then
  begin
    FCancelOperation := True;
    MemoLog.Lines.Add('Пытаемся отменить операцию...');
  end;
end;

end.
