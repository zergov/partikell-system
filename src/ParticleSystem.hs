module ParticleSystem
( ParticleSystem (..)
, newParticleSystem
, updateParticleSystem
, drawParticleSystem
) where

import System.Random
import Graphics.Gloss
import Particle (Particle, newParticle, updateParticle, drawParticle, deadParticle)

data ParticleSystem = ParticleSystem { position :: Point
                                     , elapsed :: Float
                                     , particles :: [Particle]
                                     , velocityX :: (Float, Float)
                                     , velocityY :: (Float, Float) }

newParticleSystem :: ParticleSystem
newParticleSystem = ParticleSystem { position = (0, -200)
                                   , elapsed = 0
                                   , particles = []
                                   , velocityX = (-4, 4)
                                   , velocityY = (10, 2) }

updateParticleSystem :: Float -> ParticleSystem -> IO ParticleSystem
updateParticleSystem ms ps = do
  newParticles <- (spawnParticles ps 1)
  let particles' = removeDeadParticles . map (updateParticle ms) $ (particles ps) ++ newParticles
  return $ ps { elapsed = (elapsed ps) + ms
              , particles = particles' }

spawnParticles :: ParticleSystem -> Int -> IO [Particle]
spawnParticles ps n = sequence . take n . repeat $ spawnParticle ps

spawnParticle :: ParticleSystem -> IO Particle
spawnParticle ps = do
  vx <- randomRIO (velocityX ps)
  vy <- randomRIO (velocityY ps)
  return $ newParticle (position ps) (vx, vy)

removeDeadParticles :: [Particle] -> [Particle]
removeDeadParticles ps = filter (not . deadParticle) ps

drawParticleSystem :: ParticleSystem -> IO Picture
drawParticleSystem ps = do
  return $ Pictures $ [origin, debug] ++ particlesPictures
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
