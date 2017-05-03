unit CrapsGame;

{ Fast Craps - a simplified craps game. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Crt;

const
  MAX_BET_AMOUNT = 999;


type
  { TDiceRoll }
  TDiceRoll = class(TObject)
  const
    DICES = 2;
  private
    n: array[1..DICES] of integer;
    procedure Roll;
  public
    constructor Create;
    function GetSum(): integer;
    procedure Print;
  end;

  TPassBetResult = (PASS_BET_PUSH = 100, PASS_BET_WIN, PASS_BET_LOSE,
    PASS_BET_POINT_SET);

  { TCrapsGame }

  TCrapsGame = class(TObject)
  const
    MAX_ROUNDS    = 3;
    STEP_DELAY_MS = 2000;
  private
    counter:   integer;
    point:     integer;
    set_point: TDiceRoll;
    // set point included
    rounds:    array [0..MAX_ROUNDS + 1] of TDiceRoll;
    outcome:   TPassBetResult;
    fBet:      integer;
    fReturns:  integer;
    procedure SeedRandom;
    function GetOdds(): integer;
    procedure SetBet(b: integer);
    procedure CalculateReturns;
  public
    constructor Create;
    procedure StepSetPoint;
    procedure StepRollForPoint;
    procedure SimulateFastCraps;
    procedure ReadBet;
    procedure PrintOdds;
    procedure PrintReturns;
    procedure PrintOutcome;
    property Bet: integer read fBet write SetBet;
    property Returns: integer read fReturns;
  end;

implementation

{ TGame }

procedure TDiceRoll.Roll;
var
  i: integer;
begin
//  for i := 1 to DICES do
  //  n[i] := Random(6) + 1;
  n[1] := 5;
  n[2] := 6;
end;

constructor TDiceRoll.Create;
begin
  roll;
end;

procedure TDiceRoll.Print;
begin
  Writeln(Format('Roll is %d + %d = %d.', [n[1], n[2], n[1] + n[2]]));
end;

function TDiceRoll.GetSum(): integer;
begin
  Result := n[1] + n[2];
end;

// Odds depend on current round and the point set.
function TCrapsGame.GetOdds: integer;
begin
  Result := 0;
  case point of
    4, 10:
      Result := 5;
    5, 9:
      Result := 4;
    6, 8:
      Result := 3;
    else
      exit;
  end;

  Result := Result - counter;
  Result := Result + 1;

end;

procedure TCrapsGame.SetBet(b: integer);
begin
  fBet := b;
end;

procedure TCrapsGame.CalculateReturns;
begin
  case outcome of
    PASS_BET_WIN: fReturns  := 1 + GetOdds() * Bet;
    PASS_BET_LOSE: fReturns := -1 * Bet;
    else
      fReturns := Bet;
  end;
end;

constructor TCrapsGame.Create;
begin
  seedRandom;
  counter := 0;
  point   := 0;
end;

procedure TCrapsGame.StepSetPoint;
begin
  writeln('Setting the point...');
  rounds[0] := TDiceRoll.Create;
  set_point := rounds[0];
  set_point.print;
  point := set_point.getSum();
  case point of
    7:
      outcome := PASS_BET_PUSH;
    11:
      outcome := PASS_BET_WIN;
    2, 3, 12:
      outcome := PASS_BET_LOSE;
    4, 5, 6, 8, 9, 10:
      outcome := PASS_BET_POINT_SET;
  end;
  writeln(Format('Point is %d.', [point]));
end;

procedure TCrapsGame.StepRollForPoint;
var
  sum: integer;
begin
  writeln('Rolling for the point...');
  rounds[counter] := TDiceRoll.Create;
  PrintOdds;
  rounds[counter].Print;
  sum := rounds[counter].GetSum();
  if sum = point then
    outcome := PASS_BET_WIN
  else
  if sum = 7 then
    outcome := PASS_BET_LOSE;
end;

procedure TCrapsGame.SimulateFastCraps;
begin
  Writeln('Fast Craps - Pass Bet');
  ReadBet;
  StepSetPoint;
  Delay(STEP_DELAY_MS);
  case outcome of
    PASS_BET_LOSE, PASS_BET_WIN, PASS_BET_PUSH:
    begin
      CalculateReturns;
      PrintOutcome;
      PrintReturns;
    end;
    PASS_BET_POINT_SET:
    begin
      while counter < MAX_ROUNDS do
      begin
        counter := counter + 1;
        StepRollForPoint;
        Delay(STEP_DELAY_MS);
        if outcome = PASS_BET_LOSE then
          break
        else if outcome = PASS_BET_WIN then
          break;
      end;
      if outcome <> PASS_BET_WIN then
        outcome := PASS_BET_LOSE;
      CalculateReturns;
      PrintOutcome;
      PrintReturns;
    end;
  end;
end;

procedure TCrapsGame.ReadBet;
var
  amount:  integer;
  isValid: boolean;
begin
  isValid := False;
  while not isValid do
  begin
    try
      Write(Format('Player bet(0-%d):', [MAX_BET_AMOUNT]));
      readln(amount);
    except
      on EInOutError: Exception do
        writeln('Not understood, please try again.');
    end;
    if (amount < MAX_BET_AMOUNT) and (amount > 0) then
    begin
      Bet := amount;
      writeln(Format('Ok, player bet is %d.', [Bet]));
      isValid := True;
    end;
  end;
end;

procedure TCrapsGame.PrintOdds;
begin
  Writeln(Format('Odds are %d to 1!', [GetOdds()]));
end;

procedure TCrapsGame.PrintReturns;
begin
  if Returns > Bet then
    writeln(Format('Bet was %d. Returns are %d credits. Congratulations!',
      [Bet, Returns]))
  else if Returns = Bet then
    writeln(Format('Bet was %d. Returns are %d credits. No gain, no loss.',
      [Bet, Returns]))
  else
    writeln(Format('Bet was %d. Returns are %d credits. Better luck next time!',
      [Bet, Returns]));
end;

procedure TCrapsGame.PrintOutcome;
begin
  case outcome of
    PASS_BET_LOSE: Writeln('Pass bet loses.');
    PASS_BET_WIN: Writeln('Pass bet wins!');
    PASS_BET_PUSH: Writeln('Pass bet pushes.');
    PASS_BET_POINT_SET: Writeln(Format('Point has been set to %d.', [point]));
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
  //Writeln(Format('Random seed: %d', [i and $7FFFFFFF]));
end;

end.
