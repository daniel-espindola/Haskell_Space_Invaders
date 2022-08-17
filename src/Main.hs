import Logic
import Entities
import Graphics.Gloss as G
import Graphics.Gloss.Interface.IO.Interact as G
import System.Random (newStdGen, randoms)

desenhaMundo :: Mundo Float -> G.Picture
desenhaMundo m = case status m of 
  Start -> Pictures
           [G.Color G.white $ G.Translate (posXTitulo - 70) posYTitulo $ G.Scale (tamTitulo*2) (tamTitulo*2) $ G.Text "Haskell Space Invaders!", 
            G.Color G.white $ G.Translate (posXTitulo + 200) (posYTitulo - 100) $ G.Scale tamTitulo tamTitulo $ G.Text "Controles:",
            G.Color G.white $ G.Translate posXTitulo (posYTitulo - 200) $ G.Scale tamTitulo tamTitulo $ G.Text "'A'/'S' e Esquerda/Direita movem a nave",
            G.Color G.white $ G.Translate posXTitulo (posYTitulo - 250) $ G.Scale tamTitulo tamTitulo $ G.Text "Espaco Atira",
            G.Color G.white $ G.Translate posXTitulo (posYTitulo - 350) $ G.Scale tamTitulo tamTitulo $ G.Text "Aperte Espaco para comecar"]
  Won -> G.Color G.white $ G.Translate posXTitulo posYTitulo $ G.Scale tamTitulo tamTitulo $ G.Text "mensagem vitoria"
  Lost -> G.Color G.white $ G.Translate posXTitulo posYTitulo $ G.Scale tamTitulo tamTitulo $ G.Text "mensagem derrota!"
  _ -> G.Pictures (playerPic : enemiesPics)
  where
    tamTitulo = 0.25
    posXTitulo = -300
    posYTitulo = fromIntegral (alturaTela) / 2 - 100
    playerPic = desenhaEntidade G.white (player m)
    enemiesPics = map (desenhaEntidade G.red) (enemies m)

desenhaEntidade :: G.Color -> Entity Float -> G.Picture
desenhaEntidade c it = G.Color c $ G.Translate x y $ G.rectangleSolid sx sy
  where
    (x, y) = pos it
    (sx, sy) = tam it

--Controles--
--Movimento = seta esquerda direita / A S
--Tiro/Passar Tela = Espaço
inputHandler :: G.Event -> Mundo Float -> Mundo Float
inputHandler (G.EventKey (G.SpecialKey G.KeyRight) G.Down     _ _) m = m {btnRight = True}
inputHandler (G.EventKey (G.SpecialKey G.KeyRight) G.Up       _ _) m = m {btnRight = False}
inputHandler (G.EventKey (Char 'd') G.Down                    _ _) m = m {btnRight = True}
inputHandler (G.EventKey (Char 'd') G.Up                      _ _) m = m {btnRight = False}
inputHandler (G.EventKey (G.SpecialKey G.KeyLeft) G.Down      _ _) m = m {btnLeft = True}
inputHandler (G.EventKey (G.SpecialKey G.KeyLeft) G.Up        _ _) m = m {btnLeft = False}
inputHandler (G.EventKey (Char 'a') G.Down                    _ _) m = m {btnLeft = True}
inputHandler (G.EventKey (Char 'a') G.Up                      _ _) m = m {btnLeft = False}
inputHandler (G.EventKey (G.SpecialKey G.KeySpace) G.Down     _ _) m = m {btnAction = True}
inputHandler (G.EventKey (G.SpecialKey G.KeySpace) G.Up       _ _) m = m {btnAction = False}
--inputHandler (G.EventKey (G.MouseButton G.LeftButton) G.Down  _ _) m = m {btnAction = True}
--inputHandler (G.EventKey (G.MouseButton G.LeftButton) G.Up    _ _) m = m {btnAction = False}
inputHandler _ m = m

idleH :: Float -> Mundo Float -> Mundo Float
idleH = atualizaMundo

main :: IO ()
main = do
  rng <- randoms <$> newStdGen
  
  G.play 
    janela
    G.black 
    60 
    (inicializaMundo rng)
    desenhaMundo
    inputHandler 
    idleH
  where janela = G.InWindow "Haskell Space Invaders" (larguraTela, alturaTela) (0, 0)