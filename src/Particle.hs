module Particle
( Particle (..)
, newParticle
, updateParticle
, drawParticle
) where

import Graphics.Gloss

data Particle = Particle { position :: Point
                         , velocity :: Point
                         , bgColor :: Color
                         , elapsed :: Float
                         , lifetime :: Float }

newParticle :: Particle
newParticle = Particle { position = (0, -200)
                       , velocity = (0, 4)
                       , bgColor = black
                       , elapsed = 0
                       , lifetime = 1.6 }

updateParticle :: Float -> Particle -> Particle
updateParticle ms p = p { position = (x + vx, y + vy)
                        , elapsed = (elapsed p) + ms }
  where (x, y)  = position p
        (vx, vy) = velocity p

drawParticle :: Particle -> Picture
drawParticle p = translate x y $ particlePicture
  where (x, y) = position p
        particlePicture = color (withAlpha alpha black) $ circleSolid 16
        alpha = max (((lifetime p) - (elapsed p)) / (lifetime p)) 0
