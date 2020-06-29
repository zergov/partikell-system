module Particle
( Particle (..)
, newParticle
, updateParticle
, drawParticle
, deadParticle
) where

import Graphics.Gloss

data Particle = Particle { position :: Point
                         , velocity :: Point
                         , bgColor :: Color
                         , elapsed :: Float
                         , lifetime :: Float }

newParticle :: Point -> Point -> Particle
newParticle pos vel = Particle { position = pos
                               , velocity = vel
                               , bgColor = black
                               , elapsed = 0
                               , lifetime = 1.6 }

updateParticle :: Float -> Particle -> Particle
updateParticle ms p = p { position = (x + vx, y + vy)
                        , elapsed = (elapsed p) + ms }
  where (x, y)  = position p
        (vx, vy) = velocity p

drawParticle :: Particle -> Picture
drawParticle p = Pictures [particlePicture]
  where (x, y) = position p
        particlePicture = (translate x y) . color (withAlpha alpha black) $ circleSolid 16
        alpha = max (((lifetime p) - (elapsed p)) / (lifetime p)) 0

drawDebug :: Particle -> Picture
drawDebug p = Pictures [position', velocity']
  where (x, y) = position p
        posStr = "position: " ++ (show . position $ p)
        position' = (translate (x + 10) y) . (scale 0.2 0.2) . text $ posStr
        velStr = "velocity: " ++ (show . velocity $ p)
        velocity' = (translate (x + 10) (y - 10)) . (scale 0.2 0.2) . text $ velStr


deadParticle :: Particle -> Bool
deadParticle p = (elapsed p) > (lifetime p)
