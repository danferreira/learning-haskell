{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings #-}

import           CodeWorld

-- Exercise 3

data Tile = Wall | Ground | Storage | Box | Blank
data Direction = R | U | L | D
data Coord = C Integer Integer

-- Objects --

wall, ground, storage, box, player :: Picture
wall =  colored (gray 0.4) (thickRectangle 0.05 0.5 0.5) &
          colored (gray 0.5) (solidRectangle 1 1)
ground =  colored (darker 0.2 green) (solidRectangle 1 1)
storage = colored yellow (solidCircle 0.2) & ground
box = colored (dark brown) (thickRectangle 0.05 0.5 0.5) &
      colored (dull brown) (solidRectangle 1 1)

player = translated 0 0.3 cranium
       & path [(0,0),(0.3,0.05)]
       & path [(0,0),(0.3,-0.05)]
       & path [(0,-0.2),(0,0.1)]
       & path [(0,-0.2),(0.1,-0.5)]
       & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18 & sector (7/6*pi) (1/6*pi) 0.18

-- Objects --

drawTile :: Tile -> Picture
drawTile t = case t of
              Wall    -> wall
              Ground  -> ground
              Storage -> storage
              Box     -> box
              Blank   -> blank


drawAt :: Coord -> Picture
drawAt c@(C x y) =  translated (fromInteger x) (fromInteger y) (drawTile (maze c))

pictureOfMaze :: Picture
pictureOfMaze = foldr ((&) . drawAt . uncurry C) blank [(x,y) | x <- [-10..10], y <- [-10..10]]

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

initialCoord :: Coord
initialCoord = C 0 1

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

handleTime :: Double -> Coord -> Coord
handleTime _ c = c

drawState :: Coord -> Picture
drawState c = atCoord c player & pictureOfMaze

handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c
    | key == "Right" = tryCoord R c
    | key == "Up"    = tryCoord U c
    | key == "Left"  = tryCoord L c
    | key == "Down"  = tryCoord D c
handleEvent _ c      = c

tryCoord :: Direction -> Coord -> Coord
tryCoord d c | isOk = newCoord
             | otherwise = c
      where
        newCoord = adjacentCoord d c
        isOk = case maze newCoord of Ground  -> True
                                     Storage -> True
                                     _       -> False

exercise1 :: IO ()
exercise1 = interactionOf initialCoord handleTime handleEvent drawState


-- Exercise 2 --

data State = State Coord Direction

initialState :: State
initialState = State (C 0 1) R

player2 :: Direction -> Picture
player2 R = translated 0 0.3 cranium
          & path [(0,0),(0.3,0.05)]
          & path [(0,0),(0.3,-0.05)]
          & path [(0,-0.2),(0,0.1)]
          & path [(0,-0.2),(0.1,-0.5)]
          & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & sector (7/6*pi) (1/6*pi) 0.18
player2 L = scaled (-1) 1 (player2 R) -- Cunning!
player2 U = translated 0 0.3 cranium
          & path [(0,0),(0.3,0.05)]
          & path [(0,0),(-0.3,0.05)]
          & path [(0,-0.2),(0,0.1)]
          & path [(0,-0.2),(0.1,-0.5)]
          & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = solidCircle 0.18
player2 D = translated 0 0.3 cranium
          & path [(0,0),(0.3,-0.05)]
          & path [(0,0),(-0.3,-0.05)]
          & path [(0,-0.2),(0,0.1)]
          & path [(0,-0.2),(0.1,-0.5)]
          & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & translated   0.06  0.08 (solidCircle 0.04)
                & translated (-0.06) 0.08 (solidCircle 0.04)


drawState2 :: State -> Picture
drawState2 (State c d) = atCoord c (player2 d) & pictureOfMaze

handleEvent2 :: Event -> State -> State
handleEvent2 (KeyPress key) s
    | key == "Right" = tryState s R
    | key == "Up"    = tryState s U
    | key == "Left"  = tryState s L
    | key == "Down"  = tryState s D
handleEvent2 _ s     = s

tryState :: State -> Direction -> State
tryState (State from _) d
              | isOk = State newCoord d
              | otherwise = State from d
      where
        newCoord = adjacentCoord d from
        isOk = case maze newCoord of Ground  -> True
                                     Storage -> True
                                     _       -> False


exercise2 :: IO ()
exercise2 = interactionOf initialState (\_ c -> c) handleEvent2 drawState2

--- Exercise 3 ---

resetableInteractionOf :: world ->
     (Double -> world -> world) ->
     (Event -> world -> world)  ->
     (world -> Picture)         ->
     IO ()
resetableInteractionOf initial ht he ds =
  interactionOf initial ht he' ds
    where
      he' (KeyPress "Esc") _ =  initial
      he' e s                = he e s

exercise3 :: IO()
exercise3 = resetableInteractionOf initialState (\_ c -> c) handleEvent2 drawState2

main :: IO()
main = exercise3
