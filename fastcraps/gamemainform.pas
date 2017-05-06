unit GameMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IDEWindowIntf, Forms, Controls, Graphics,
  Dialogs, Menus, ExtCtrls, StdCtrls, Spin, Buttons, Math, CrapsGame,
  gameuserguideform;

type

  { TGameMainForm }

  TGameMainForm = class(TForm)
    NewGameButton: TButton;
    PoolLabel:     TLabel;
    PoolValueLabel: TLabel;
    DicePanel2:    TPanel;
    DicePanel3:    TPanel;
    DicePanel4:    TPanel;
    BetLabel:      TLabel;
    BetValueLabel: TLabel;
    EarningsLabel: TLabel;
    EarningsValueLabel: TLabel;
    LossesLabel:   TLabel;
    ReturnsValueLabel: TLabel;
    ReturnsLabel:  TLabel;
    OddsValueLabel: TLabel;
    OddsLabel:     TLabel;
    LossesValueLabel: TLabel;
    GameStatsPanel: TPanel;
    PointButton4:  TBitBtn;
    PointButton10: TBitBtn;
    PointButton9:  TBitBtn;
    PointButton8:  TBitBtn;
    PointButton6:  TBitBtn;
    PointButton5:  TBitBtn;
    PointGroupBox: TGroupBox;
    DiceImage1:    TImage;
    DiceImage2:    TImage;
    DicePanel1:    TPanel;
    DiceImage3:    TImage;
    DiceImage4:    TImage;
    DiceImage5:    TImage;
    DiceImage6:    TImage;
    DiceImage7:    TImage;
    DiceImage8:    TImage;
    LabelRoll2:    TLabel;
    LabelRoll3:    TLabel;
    LabelRoll4:    TLabel;
    OutcomePanel:  TPanel;
    PlayerPanel:   TPanel;
    RollButton1:   TButton;
    BetButton:     TButton;
    LabelRoll1:    TLabel;
    YourBetLabel:  TLabel;
    BetPanel:      TPanel;
    MainMenu:      TMainMenu;
    MenuItemAbout: TMenuItem;
    MenuItemExit:  TMenuItem;
    MenuItemFile:  TMenuItem;
    MenuItemHelp:  TMenuItem;
    MenuItemNewGame: TMenuItem;
    MenuItemUserGuide: TMenuItem;
    RollButton2:   TButton;
    RollButton3:   TButton;
    RollButton4:   TButton;
    RollPanel1:    TPanel;
    RollPanel2:    TPanel;
    RollPanel3:    TPanel;
    RollPanel4:    TPanel;
    BetSpinEdit:   TSpinEdit;
    OutcomeMessage: TStaticText;
    PlayerStatPanel: TPanel;
    procedure BetButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemNewGameClick(Sender: TObject);
    procedure MenuItemUserGuideClick(Sender: TObject);
    procedure NewGameButtonClick(Sender: TObject);
    procedure RollButton1Click(Sender: TObject);
    procedure RollButton2Click(Sender: TObject);
    procedure RollButton3Click(Sender: TObject);
    procedure RollButton4Click(Sender: TObject);
    procedure BetSpinEditChange(Sender: TObject);
  private
    { private declarations }
    procedure SetPictureForDice(img: TImage; i: integer);
    procedure ResetPicture(img: TImage);
    procedure SetPictureForPoint(game: TCrapsGame);
    procedure ResetPictureForPoints;
    procedure SetStateForWin;
    procedure SetStateForLoss;
    procedure SetStateForNextRound;
    procedure SetStateForNewGame;
  public
    { public declarations }
  end;

const
  IMG_EMPTY_TOKEN = 7;
  IMG_TOKEN = 8;

var
  MainForm: TGameMainForm;
  game:     TCrapsGame;
  pictures: array[1..8] of TPicture;

implementation

{$R *.lfm}

{ TGameMainForm }

procedure TGameMainForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  game := TCrapsGame.Create;
  PoolValueLabel.Caption := Format('%d', [game.Player.Pool]);

  BetSpinEdit.MaxValue := Min(game.MAX_BET_AMOUNT, game.Player.Pool);
  for i := 1 to 6 do
  begin
    pictures[i] := TPicture.Create;
    pictures[i].LoadFromFile(Format('assets/image/d%d.png', [i]));
  end;
  pictures[IMG_EMPTY_TOKEN] := TPicture.Create;
  pictures[IMG_EMPTY_TOKEN].LoadFromFile('assets/image/emptytoken.png');
  pictures[IMG_TOKEN] := TPicture.Create;
  pictures[IMG_TOKEN].LoadFromFile('assets/image/token.png');
end;

procedure TGameMainForm.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to 8 do
  begin
    FreeAndNil(pictures[i]);
  end;
  FreeAndNil(game);
end;


procedure TGameMainForm.BetButtonClick(Sender: TObject);
begin
  game.Bet := BetSpinEdit.Value;
  // substractBet
  game.Player.Pool := game.Player.Pool - game.Bet;
  OutcomeMessage.Caption := Format('Ok, player bet is %d.', [game.Bet]);
  BetValueLabel.Caption := Format('%d', [game.Bet]);
  BetSpinEdit.Enabled := False;
  BetButton.Enabled := False;
  RollButton1.Enabled := True;
end;


procedure TGameMainForm.MenuItemAboutClick(Sender: TObject);
begin
  ShowMessage('Fast Craps 0.2' + sLineBreak + 'Credits:' + sLineBreak +
    'Programming: Erno Szabados');
end;

procedure TGameMainForm.MenuItemExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TGameMainForm.MenuItemNewGameClick(Sender: TObject);
begin
  SetStateForNewGame;
end;

procedure TGameMainForm.MenuItemUserGuideClick(Sender: TObject);
var UserGuideForm : TUserGuideForm;
begin
  Application.CreateForm(TUserGuideForm, UserGuideForm);
  //Form := UserGuideForm.Create(Self);
  try
    UserGuideForm.ShowModal;
  finally
    UserGuideForm.Free;
  end;
end;

procedure TGameMainForm.NewGameButtonClick(Sender: TObject);
begin
  SetStateForNextRound;
end;

procedure TGameMainForm.SetPictureForDice(img: TImage; i: integer);
begin
  case i of
    1..6:
    begin
      img.Picture.Bitmap := pictures[i].Bitmap;
    end;
  end;
end;

procedure TGameMainForm.ResetPicture(img: TImage);
begin
  img.Picture.Bitmap := nil;
end;

procedure TGameMainForm.SetPictureForPoint(game: TCrapsGame);
begin
  case game.CurrentRoll.GetSum() of
    4: PointButton4.Glyph   := pictures[IMG_TOKEN].Bitmap;
    5: PointButton5.Glyph   := pictures[IMG_TOKEN].Bitmap;
    6: PointButton6.Glyph   := pictures[IMG_TOKEN].Bitmap;
    8: PointButton8.Glyph   := pictures[IMG_TOKEN].Bitmap;
    9: PointButton9.Glyph   := pictures[IMG_TOKEN].Bitmap;
    10: PointButton10.Glyph := pictures[IMG_TOKEN].Bitmap;
  end;
end;

procedure TGameMainForm.ResetPictureForPoints;

begin
  PointButton4.Glyph  := pictures[IMG_EMPTY_TOKEN].Bitmap;
  PointButton5.Glyph  := pictures[IMG_EMPTY_TOKEN].Bitmap;
  PointButton6.Glyph  := pictures[IMG_EMPTY_TOKEN].Bitmap;
  PointButton8.Glyph  := pictures[IMG_EMPTY_TOKEN].Bitmap;
  PointButton9.Glyph  := pictures[IMG_EMPTY_TOKEN].Bitmap;
  PointButton10.Glyph := pictures[IMG_EMPTY_TOKEN].Bitmap;
end;

procedure TGameMainForm.SetStateForWin;
begin
  game.CreditingReturns;
  OutcomeMessage.Caption     :=
    Format('%s %s %s', [game.GetOutcome, sLineBreak, game.GetReturnsMessage]);
  EarningsValueLabel.Caption := Format('%d', [game.Player.Earnings]);
  PoolValueLabel.Caption     := Format('%d', [game.Player.Pool]);
  ReturnsValueLabel.Caption  := '0';
  BetValueLabel.Caption      := '0';
  NewGameButton.Enabled      := True;
end;

procedure TGameMainForm.SetStateForLoss;
begin
  game.CreditingReturns;
  LossesValueLabel.Caption  := Format('%d', [game.Player.Losses]);
  OutcomeMessage.Caption    :=
    Format('%s %s %s', [game.GetOutcome, sLineBreak, game.GetReturnsMessage]);
  PoolValueLabel.Caption    := Format('%d', [game.Player.Pool]);
  ReturnsValueLabel.Caption := '0';
  BetValueLabel.Caption     := '0';
  if game.Player.Pool = 0 then
  begin
    ShowMessage('Game Over...' + sLineBreak + 'Your pool has been depleted.' +
      sLineBreak + 'Thank you for playing!');
  end
  else
  begin
    NewGameButton.Enabled := True;

  end;
end;

procedure TGameMainForm.SetStateForNextRound;
begin
  NewGameButton.Enabled := False;
  if game.Player.Pool > 0 then
  begin
    BetSpinEdit.Enabled  := True;
    BetSpinEdit.Value    := 0;
    BetSpinEdit.MaxValue := Min(game.MAX_BET_AMOUNT, game.Player.Pool);
    BetButton.Enabled    := True;
  end;
  OddsValueLabel.Caption := '-';
  ResetPicture(DiceImage1);
  ResetPicture(DiceImage2);
  ResetPicture(DiceImage3);
  ResetPicture(DiceImage4);
  ResetPicture(DiceImage5);
  ResetPicture(DiceImage6);
  ResetPicture(DiceImage7);
  ResetPicture(DiceImage8);
  ResetPictureForPoints;
  game.ResetState;
end;

procedure TGameMainForm.SetStateForNewGame;
begin
  game.NewGame;
  NewGameButton.Enabled  := False;
  BetSpinEdit.Enabled    := True;
  BetSpinEdit.Value      := 0;
  BetSpinEdit.MaxValue   := Min(game.MAX_BET_AMOUNT, game.Player.Pool);
  BetButton.Enabled      := True;
  OddsValueLabel.Caption := '-';
  PoolValueLabel.Caption := Format('%d', [game.Player.Pool]);
  EarningsValueLabel.Caption := Format('%d', [game.Player.Earnings]);
  LossesValueLabel.Caption := Format('%d', [game.Player.Losses]);
  ResetPicture(DiceImage1);
  ResetPicture(DiceImage2);
  ResetPicture(DiceImage3);
  ResetPicture(DiceImage4);
  ResetPicture(DiceImage5);
  ResetPicture(DiceImage6);
  ResetPicture(DiceImage7);
  ResetPicture(DiceImage8);
  ResetPictureForPoints;
end;


// handle set the point roll
procedure TGameMainForm.RollButton1Click(Sender: TObject);
var
  roll: TDiceRoll;
begin
  game.StepSetPoint;
  roll := game.CurrentRoll;
  setPictureForDice(DiceImage1, roll.GetDice(1));
  setPictureForDice(DiceImage2, roll.GetDice(2));

  RollButton1.Enabled    := False;
  OddsValueLabel.Caption := Format('%d:1', [game.GetOdds]);
  ReturnsValueLabel.Caption := Format('%d', [game.GetReturns]);

  case game.Outcome of
    PASS_BET_POINT_SET:
    begin
      SetPictureForPoint(game);
      OutcomeMessage.Caption := game.GetOutcome;
      RollButton2.Enabled    := True;
    end;
    PASS_BET_WIN: SetStateForWin;
    PASS_BET_LOSE: SetStateForLoss;
    PASS_BET_PUSH:
    begin
      game.CreditingReturns;
      OutcomeMessage.Caption    :=
        Format('%s %s %s', [game.GetOutcome, sLineBreak, game.GetReturnsMessage]);
      PoolValueLabel.Caption    := Format('%d', [game.Player.Pool]);
      ReturnsValueLabel.Caption := '0';
      BetValueLabel.Caption     := '0';
      NewGameButton.Enabled     := True;
    end;
  end;

end;

procedure TGameMainForm.RollButton2Click(Sender: TObject);
var
  roll: TDiceRoll;
begin
  game.StepRollForPoint;
  roll := game.CurrentRoll;
  setPictureForDice(DiceImage3, roll.GetDice(1));
  setPictureForDice(DiceImage4, roll.GetDice(2));

  RollButton2.Enabled    := False;
  OddsValueLabel.Caption := Format('%d:1', [game.GetOdds]);
  ReturnsValueLabel.Caption := Format('%d', [game.GetReturns]);

  case game.Outcome of
    PASS_BET_WIN: SetStateForWin;
    PASS_BET_LOSE: SetStateForLoss;
    PASS_BET_POINT_SET:
    begin
      RollButton3.Enabled := True;
    end;
  end;

end;

procedure TGameMainForm.RollButton3Click(Sender: TObject);
var
  roll: TDiceRoll;
begin
  game.StepRollForPoint;
  roll := game.CurrentRoll;
  setPictureForDice(DiceImage5, roll.GetDice(1));
  setPictureForDice(DiceImage6, roll.GetDice(2));

  RollButton3.Enabled    := False;
  OddsValueLabel.Caption := Format('%d:1', [game.GetOdds]);
  ReturnsValueLabel.Caption := Format('%d', [game.GetReturns]);

  case game.Outcome of
    PASS_BET_WIN: SetStateForWin;
    PASS_BET_LOSE: SetStateForLoss;
    PASS_BET_POINT_SET:
    begin
      RollButton4.Enabled := True;
    end;
  end;
end;

procedure TGameMainForm.RollButton4Click(Sender: TObject);
var
  roll: TDiceRoll;
begin
  game.StepRollForPoint;
  roll := game.CurrentRoll;
  setPictureForDice(DiceImage7, roll.GetDice(1));
  setPictureForDice(DiceImage8, roll.GetDice(2));

  RollButton4.Enabled    := False;
  OddsValueLabel.Caption := Format('%d:1', [game.GetOdds]);
  ReturnsValueLabel.Caption := Format('%d', [game.GetReturns]);

  case game.Outcome of
    PASS_BET_WIN: SetStateForWin;
    PASS_BET_LOSE: SetStateForLoss;
  end;

end;

procedure TGameMainForm.BetSpinEditChange(Sender: TObject);
begin
  game.Bet := BetSpinEdit.Value;
  BetValueLabel.Caption := Format('%d', [game.Bet]);
  PoolValueLabel.Caption := Format('%d', [game.Player.Pool - game.Bet]);
end;

end.
