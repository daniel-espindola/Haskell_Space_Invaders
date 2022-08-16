module Logic where

import Entities
import qualified Point as P

larguraTela, alturaTela :: Int
larguraTela = 800
alturaTela = 600

inicializaMundo :: (Fractional a, Ord a) => [a] -> Mundo a
inicializaMundo rng =
    Game
      Start
      False
      False
      False
      rng
      newPlayer
      []
      []
  where
    newPlayer = Entity (70, 20) (0, -250) (0, 0)
    -- TODO: CRIAR INIMIGOS

atualizaPlayer :: (Ord a, Num a) => a -> Mundo a -> Mundo a
atualizaPlayer dt g = g1
  where
    velPlayer = 200
    dx
      | btnLeft g = -dt * velPlayer
      | btnRight g = dt * velPlayer
      | otherwise = 0
    oldPlayer = player g
    (x0, y) = pos oldPlayer
    newPos = x0 + dx
    posCorrigida
      | newPos > 360 = 360
      | newPos < -360 = -360
      | otherwise = newPos
    newPlayer = oldPlayer {pos = (posCorrigida, y)}
    g1 = g {player = newPlayer}

atualizaMundo :: (Fractional a, Ord a) => a -> Mundo a -> Mundo a
atualizaMundo dt m =
  if status m == Playing
    then rodaGame
    else endScreen
  where
    rodaGame = atualizaPlayer dt m
    endScreen = if btnAction m
           then novoMundo {status = Playing}
           else m
    novoMundo = inicializaMundo (rngs m)
