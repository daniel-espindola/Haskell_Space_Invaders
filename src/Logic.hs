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
      myInvaders
  where
    newPlayer = Entity (70, 20) (0, -250) (0, 0)
    ([mag, dir], rands1) = splitAt 2 rng
    vx = (150 + 200 * mag) * (if dir < 0.5 then 1 else -1)
    myInvaders = [Entity
                  (70, 20)
                  (fromIntegral x * 100, fromIntegral y * 50 + 150)
                  (vx, 0)
                  | x <- [-2 .. (2 :: Int)],
                    y <- [0 .. (2 :: Int)]
                ]

-- (x, y)

-- (x, y)

-- x * 200
-- (x, y)

autoUpdateItem :: (Num a) => a -> Entity a -> Entity a
autoUpdateItem dt i@(Entity _ p1 vel) = i {pos = p1 P.+ dt P.* vel}

--entender oq a funcao null faz
--tentar substituir ela por length

atualizaInimigos :: (Fractional a, Ord a) => a -> Mundo a -> Mundo a
atualizaInimigos dt m = m2
  where
    myEnemies = enemies m
    e1 = map (autoUpdateItem dt) myEnemies
    xs = map (fst . pos) myEnemies
    x1min = minimum xs
    x1max = maximum xs
    v@(vx, _) = vel $ head myEnemies
    move v0 v1 e = e {pos = pos e P.+ dt P.* v0, vel = v1}
    e2
      | vx > 0 && x1max > 380 = map (move (380 - x1max, 500) (P.negate v)) e1
      | vx < 0 && x1min < (-380) = map (move (-380 - x1min, -500) (P.negate v)) e1
      | otherwise = e1
    m2 = m {enemies = e2}
    --Colocar código de tiros dos inimigos

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
    rodaGame = atualizaPlayer dt $
               atualizaInimigos dt m
    endScreen = if btnAction m
           then novoMundo {status = Playing}
           else m
    novoMundo = inicializaMundo (rngs m)
