{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import           CodeWorld

main :: IO ()
main = exercise3

-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

botCircle, midCircle, topCircle :: Color -> Picture
topCircle c = colored c (translated 0   2.5  (solidCircle 1))
midCircle c = colored c (translated 0    0 (solidCircle 1))
botCircle c = colored c (translated 0 (-2.5) (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 7.5

trafficLight :: Color -> Color -> Color -> Picture
trafficLight cb cm ct  = botCircle cb & midCircle cm & topCircle ct & frame

trafficController :: Integer -> Picture
trafficController s
  | s == 1 || s == 2 = trafficLight green black black
  | s == 3           = trafficLight black yellow black
  | s == 4 || s == 5 = trafficLight black black red
  | otherwise                = trafficLight black yellow red

trafficLightAnimation :: Double -> Picture
trafficLightAnimation t = trafficController (round t `mod` 6)

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2

tree :: Picture -> Integer -> Picture
tree b 0 =  b
tree b n = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree b (n-1))  & rotated (- pi/10) (tree b (n-1)))

blossom :: Double -> Picture
blossom t = colored yellow (translated 0 0 (solidCircle (min t 10/50)))

animatree :: Double -> Picture
animatree t = tree (blossom t) 8

exercise2 :: IO ()
exercise2 = animationOf animatree

-- Exercise 3

wall, ground, storage, box :: Picture
wall =  colored (gray 0.4) (thickRectangle 0.05 0.5 0.5) & colored (gray 0.5) (solidRectangle 1 1)
ground =  colored (darker 0.2 green) (solidRectangle 1 1)
storage = colored yellow (solidCircle 0.2) & ground
box = colored (dark brown) (thickRectangle 0.05 0.5 0.5) & colored (dull brown) (solidRectangle 1 1)

drawTile :: Integer -> Picture
drawTile t = case t of
              1 -> wall
              2 -> ground
              3 -> storage
              4 -> box
              _ -> blank


drawAt :: Integer -> Integer -> Picture
drawAt x y =  translated (fromInteger x) (fromInteger y) (drawTile (maze x y))

pictureOfMaze :: Picture
pictureOfMaze = foldr ((&) . uncurry drawAt) blank [(x,y) | x <- [-10..10], y <- [-10..10]]

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze

maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
