module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

window :: Display
window = InWindow "Nice Window" (400, 400) (10, 10)

main :: IO ()
main = simulate window white 60 0 drawModel updateModel

drawModel :: Int -> Picture
drawModel n = text $ show n

updateModel :: ViewPort -> Float -> Int -> Int
updateModel _ ms n = n + 1
