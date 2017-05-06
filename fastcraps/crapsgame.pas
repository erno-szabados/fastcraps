unit CrapsGame;

{ Fast Craps - a simplified craps game. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Crt, Math;

type
  { TDiceRoll }
  TDiceRoll = class(TObject)
  const
    DICES = 2;
  private
    n: array[1..DICES] of integer;
    procedure Roll;
    procedure SimRoll;
  public
    constructor Create;
    function GetSum(): integer;
    function GetDice(i: integer): integer;
  end;

type

  { TPlayer }

  TPlayer = class(TObject)
  private
    fPool:     integer;
    fEarnings: integer;
    fLosses:   integer;
  public
    procedure ResetPlayer;
    property Pool: integer read fPool write fPool;
    property Earnings: integer read fEarnings write fEarnings;
    property Losses: integer read fLosses write fLosses;
  end;

  TPassBetResult = (PASS_BET_PUSH = 100, PASS_BET_WIN, PASS_BET_LOSE,
    PASS_BET_POINT_SET);

  { TCrapsGame }

  TCrapsGame = class(TObject)
  const
    MAX_BET_AMOUNT = 99;
    MAX_ROUNDS     = 3;
    STEP_DELAY_MS  = 2000;
    STARTING_POOL  = 10;
  private
    fPlayer:   TPlayer;
    fCounter:  integer;
    fPoint:    integer;
    set_point: TDiceRoll;
    // set point included
    rounds:    array [0..MAX_ROUNDS + 1] of TDiceRoll;
    fOutcome:  TPassBetResult;
    fBet:      integer;
    fReturns:  integer;
    procedure SeedRandom;
    procedure SetBet(b: integer);
    function fCurrentRoll: TDiceRoll;
  public
    constructor Create;
    procedure StepSetPoint;
    procedure StepRollForPoint;
    procedure CreditingReturns;
    procedure ResetState;
    procedure NewGame;
    function GetOutcome: Utf8String;
    function GetReturnsMessage: Utf8String;
    function GetOdds: integer;
    function GetReturns: integer;
    property Bet: integer read fBet write SetBet;
    property Returns: integer read fReturns;
    property Point: integer read fPoint;
    property Outcome: TPassBetResult read fOutcome;
    property Counter: integer read fCounter;
    property CurrentRoll: TDiceRoll read fCurrentRoll;
    property Player: TPlayer read fPlayer;
  end;

implementation

{ TGame }

var
  cnt: integer;

{ TPlayer }

procedure TPlayer.ResetPlayer;
begin
  Pool     := TCrapsGame.STARTING_POOL;
  Earnings := 0;
  Losses   := 0;
end;

procedure TDiceRoll.Roll;
var
  i: integer;
begin
  for i := 1 to DICES do
    n[i] := Random(6) + 1;
end;

procedure TDiceRoll.SimRoll;
begin
  case cnt of
    1:
    begin
      n[1] := 5;
      n[2] := 6;
      cnt  := cnt + 1;
    end;
    2:
    begin
      n[1] := 2;
      n[2] := 3;
      cnt  := cnt + 1;
    end;
    3:
    begin
      n[1] := 4;
      n[2] := 3;
      cnt  := cnt + 1;
    end;
    4:
    begin
      n[1] := 3;
      n[2] := 3;
      cnt  := cnt + 1;
    end;
  end;
end;

constructor TDiceRoll.Create;
begin
  roll;
end;

function TDiceRoll.GetSum(): integer;
begin
  Result := n[1] + n[2];
end;

function TDiceRoll.GetDice(i: integer): integer;
begin
  Result := n[i];
end;

// Odds depend on current round and the point set.
function TCrapsGame.GetOdds: integer;
begin
  Result := 0;
  if fCounter = 0 then
    Result := 1
  else
    case fPoint of
      4, 10:
        Result := 5;
      5, 9:
        Result := 4;
      6, 8:
        Result := 3;
      else
        Result := 1;
    end;

  Result := Result - Max(0, (fCounter - 1));

end;

function TCrapsGame.GetReturns: integer;
begin
  case fOutcome of
    PASS_BET_WIN, PASS_BET_POINT_SET: fReturns := GetOdds * Bet;
    PASS_BET_PUSH: fReturns := Bet;
    PASS_BET_LOSE: fReturns := 0;
  end;
  Result := fReturns;
end;

procedure TCrapsGame.SetBet(b: integer);
begin
  if b <= Player.Pool then
    fBet := b;
end;

function TCrapsGame.fCurrentRoll: TDiceRoll;
begin
  Result := rounds[counter];
end;

constructor TCrapsGame.Create;
begin
  seedRandom;
  cnt      := 1;
  fCounter := 0;
  fPoint   := 0;
  fPlayer  := TPlayer.Create;
  fPlayer.Pool := STARTING_POOL;
end;

procedure TCrapsGame.StepSetPoint;
begin
  // setting the point
  rounds[0] := TDiceRoll.Create;
  set_point := rounds[0];
  fPoint    := set_point.getSum();
  case fPoint of
    7:
      fOutcome := PASS_BET_PUSH;
    11:
      fOutcome := PASS_BET_WIN;
    2, 3, 12:
      fOutcome := PASS_BET_LOSE;
    4, 5, 6, 8, 9, 10:
      fOutcome := PASS_BET_POINT_SET;
  end;
end;

procedure TCrapsGame.StepRollForPoint;
var
  sum: integer;
begin
  fCounter := fCounter + 1;
  rounds[fCounter] := TDiceRoll.Create;
  sum      := rounds[fCounter].GetSum();
  if sum = fPoint then
    fOutcome := PASS_BET_WIN
  else
  begin
    if sum = 7 then
      fOutcome := PASS_BET_LOSE;
    if fCounter = MAX_ROUNDS then
      fOutcome := PASS_BET_LOSE;
  end;
end;

function TCrapsGame.GetReturnsMessage: Utf8String;
begin
  case Outcome of
    PASS_BET_WIN:
      Result := Format('Bet was %d. Returns are %d credits. Congratulations!',
        [Bet, Returns]);
    PASS_BET_PUSH:
      Result := Format('Bet was %d. Returns are %d credits. No gain, no loss.',
        [Bet, Returns]);
    PASS_BET_LOSE:
      Result := Format('Bet was %d. Returns are %d credits. Better luck next time!',
        [Bet, Returns]);

  end;
end;

 // this should be a player class method but then game must be forward
 // declared TODO refactor if multiple players planned.
procedure TCrapsGame.CreditingReturns;
begin
  case Outcome of
    PASS_BET_WIN:
    begin
      Player.Pool     := Player.Pool + GetReturns + Bet;
      Player.Earnings := Player.Earnings + GetReturns;
    end;
    PASS_BET_LOSE:
    begin
      Player.Losses := Player.Losses + Bet;
    end;
    PASS_BET_PUSH:
    begin
      Player.Pool := Player.Pool + Bet;
    end;
  end;
end;

procedure TCrapsGame.ResetState;
var
  i: integer;
begin
  cnt      := 1;
  fCounter := 0;
  fPoint   := 0;
  // Do we need a new state for initial?
  fOutcome := PASS_BET_POINT_SET;

  for i := 1 to MAX_ROUNDS do
    FreeAndNil(rounds[i]);
end;

procedure TCrapsGame.NewGame;
begin
  Player.ResetPlayer;
  ResetState;
end;


function TCrapsGame.GetOutcome: Utf8String;
begin
  case fOutcome of
    PASS_BET_LOSE: Result := 'Pass bet loses.';
    PASS_BET_WIN: Result  := 'Pass bet wins!';
    PASS_BET_PUSH: Result := 'Pass bet pushes.';
    PASS_BET_POINT_SET: Result := Format('Point has been set to %d.', [Point]);
  end;
end;


procedure TCrapsGame.SeedRandom;
var
  f: file of integer;
  i: integer;
begin
  i := 0;
  filemode := 0;
  AssignFile(f, '/dev/urandom');
  reset(f, 1);
  Read(f, i);
  CloseFile(f);
  RandSeed := i and $7FFFFFFF;
end;

end.
