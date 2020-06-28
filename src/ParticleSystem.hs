module ParticleSystem
( ParticleSystem (..)
, newParticleSystem
, updateParticleSystem
, drawParticleSystem
) where

import Graphics.Gloss
import Particle (Particle, newParticle, updateParticle, drawParticle)

data ParticleSystem = ParticleSystem { position :: Point
                                     , elapsed :: Float
                                     , birthRate :: Float
                                     , particles :: [Particle] }

newParticleSystem :: ParticleSystem
newParticleSystem = ParticleSystem { position = (0, -200)
                                   , elapsed = 0
                                   , birthRate = 2
                                   , particles = [] }

updateParticleSystem :: Float -> ParticleSystem -> ParticleSystem
updateParticleSystem ms ps = ps { elapsed = elapsed'
                                , particles = particles' }
  where elapsed' = (elapsed ps) + ms
        elapsedSecond = fromIntegral $ (floor elapsed') - (floor $ elapsed ps)
        particlesToCreate = (birthRate ps) * elapsedSecond
        particles' = map (updateParticle ms) (particles ps) ++ (take (floor particlesToCreate) $ repeat newParticle)

drawParticleSystem :: ParticleSystem -> Picture
drawParticleSystem ps = Pictures $ [origin, debug] ++ particlesPictures
  where (x, y) = position ps
        origin = translate x y $ circle 8
        particlesPictures = drawParticle <$> (particles ps)
        debug = drawSystemStats ps

drawSystemStats :: ParticleSystem -> Picture
drawSystemStats ps = Pictures [particlesCount, elapsedTime]
  where (x, y) = position ps
        elapsedTimeStr = "Elapsed time: " ++ (show . elapsed $ ps)
        elapsedTime = (translate x (y - 100)) . (scale 0.2 0.2). text $ elapsedTimeStr
        particlesCountStr = "Particle count: " ++ (show . length . particles $ ps)
        particlesCount = (translate x (y - 140)) . (scale 0.2 0.2). text $ particlesCountStr
