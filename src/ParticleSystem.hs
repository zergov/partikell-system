module ParticleSystem
( ParticleSystem (..)
, newParticleSystem
, updateParticleSystem
, drawParticleSystem
) where

import Graphics.Gloss
import Particle (Particle)

data ParticleSystem = ParticleSystem { position :: Point
                                     , elapsed :: Float
                                     , particles :: [Particle] }

newParticleSystem :: ParticleSystem
newParticleSystem = ParticleSystem { position = (0, -200)
                                   , elapsed = 0
                                   , particles = [] }

updateParticleSystem :: Float -> ParticleSystem -> ParticleSystem
updateParticleSystem ms ps = ps { elapsed = (elapsed ps) + ms }

drawParticleSystem :: ParticleSystem -> Picture
drawParticleSystem ps = translate x y $ circle 16
  where (x, y) = position ps
