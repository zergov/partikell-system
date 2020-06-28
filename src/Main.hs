module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Color

import Particle

window :: Display
window = FullScreen

main :: IO ()
main = simulate window white 60 newParticle drawParticle updateScene

updateScene :: ViewPort -> Float -> Particle -> Particle
updateScene _ ms p = updateParticle ms p
