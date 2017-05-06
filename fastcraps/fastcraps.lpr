program fastcraps;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  GameMainForm;

{$R *.res}


begin
  Application.Title:='FastCraps';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TGameMainForm, MainForm);
//  Application.CreateForm(TUserGuideForm, UserGuideForm);
  Application.Run;
end.

