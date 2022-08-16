module Entities where
import qualified Point as P

data Status
  = Start
  | Playing
  | Won
  | Lost
  deriving (Eq)

data Entity a = Entity
  { tam :: P.Point a,
    pos :: P.Point a,
    vel :: P.Point a
  }

data Mundo m = Game
  { status :: Status,
    btnLeft :: Bool,
    btnRight :: Bool,
    btnAction :: Bool,
    rngs :: [m],
    player :: Entity m,
    bullets :: [Entity m],
    enemies :: [Entity m]
  }