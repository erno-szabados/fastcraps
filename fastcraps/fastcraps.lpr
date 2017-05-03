program fastcraps;

uses
  CrapsGame;

var
  game: tcrapsgame;
begin
  game := TCrapsGame.Create;
  game.SimulateFastCraps;
end.
