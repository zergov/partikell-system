module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Simulate

import ParticleSystem

window :: Display
window = FullScreen

main :: IO ()
main = simulateIO window white 60 newParticleSystem drawParticleSystem updateScene

updateScene :: ViewPort -> Float -> ParticleSystem -> IO ParticleSystem
updateScene _ ms ps = updateParticleSystem ms ps
