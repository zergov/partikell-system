module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Color

import ParticleSystem

window :: Display
window = FullScreen

main :: IO ()
main = simulate window white 60 newParticleSystem drawParticleSystem updateScene

updateScene :: ViewPort -> Float -> ParticleSystem -> ParticleSystem
updateScene _ ms ps = updateParticleSystem ms ps
