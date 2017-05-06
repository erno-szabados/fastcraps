unit gameuserguideform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

const SUMMARY_FILE = 'docs/summary.txt';

type

  { TUserGuideForm }
  TUserGuideForm = class(TForm)
    UserGuideOkButton: TButton;
    UserGuideMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure UserGuideOkButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  UserGuideForm: TUserGuideForm;

implementation

{$R *.lfm}

{ TUserGuideForm }

procedure TUserGuideForm.FormCreate(Sender: TObject);
begin
  UserGuideMemo.Lines.LoadFromFile(SUMMARY_FILE);
end;

procedure TUserGuideForm.UserGuideOkButtonClick(Sender: TObject);
begin
     Close;
end;

end.

