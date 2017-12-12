{-# LANGUAGE OverloadedStrings #-}
import           CodeWorld

-- Lists

data List a = Empty | Entry a (List a)

mapList :: (a -> b) -> List a -> List b
mapList _ Empty        = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty        = blank
combine (Entry p ps) = p & combine ps

allList :: List Bool -> Bool
allList Empty        = True
allList (Entry b bs) = b && allList bs

-- Coordinates


data Coord = C Integer Integer

data Direction = R | U | L | D

eqCoord :: Coord -> Coord -> Bool
eqCoord (C x1 y1) (C x2 y2) = x1 == x2 && y1 == y2

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

moveFromTo :: Coord -> Coord -> Coord -> Coord
moveFromTo c1 c2 c3 = if eqCoord c1 c3 then c2 else c3


-- The maze

data Tile = Wall | Ground | Storage | Box | Blank

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

noBoxMaze :: Coord -> Tile
noBoxMaze c = case maze c of
              Box -> Ground
              t   -> t

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes Empty c = noBoxMaze c
mazeWithBoxes (Entry c1 cs) c2
        | eqCoord c1 c2 = Box
        | otherwise     = mazeWithBoxes cs c2

isOnStorage :: Coord -> Bool
isOnStorage c = case maze c of
                 Storage -> True
                 _       -> False

isWon :: State -> Bool
isWon (State _ _ l) = allList $ mapList isOnStorage l

-- The state

data State = State Coord Direction (List Coord)

initialBoxes :: List Coord
initialBoxes = foldr Entry Empty $ filter (isBox . maze) coords
      where
        coords = map (uncurry C)[(x,y) | x <- [-10..10], y <- [-10..10]]
        isBox Box = True
        isBox _   = False

initialState :: State
initialState = State (C 0 1) R initialBoxes

-- Event handling

handleEvent :: Event -> State -> State
handleEvent _ s
    | isWon s = s
handleEvent (KeyPress key) s
    | key == "Right" = tryState s R
    | key == "Up"    = tryState s U
    | key == "Left"  = tryState s L
    | key == "Down"  = tryState s D
handleEvent _ s     = s

tryState :: State -> Direction -> State
tryState (State from _ l) d
              | isOk || isBoxOk = State to d (mapList (moveFromTo to beyond) l)
              | otherwise = State from d l
      where
        to = adjacentCoord d from
        beyond = adjacentCoord d to
        currentMaze = mazeWithBoxes l
        isOk = case currentMaze to of Ground  -> True
                                      Storage -> True
                                      _       -> False
        isBoxOk = case currentMaze to of
                      Box -> case currentMaze beyond of Ground  -> True
                                                        Storage -> True
                                                        _       -> False
                      _ -> False

-- Drawing

wall, ground, storage, box :: Picture
wall =  colored (gray 0.4) (thickRectangle 0.05 0.5 0.5) &
          colored (gray 0.5) (solidRectangle 1 1)
ground =  colored (darker 0.2 green) (solidRectangle 1 1)
storage = colored yellow (solidCircle 0.2) & ground
box = colored (dark brown) (thickRectangle 0.05 0.5 0.5) &
      colored (dull brown) (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt (C r c)))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: Coord -> Picture
drawTileAt c = atCoord c (drawTile (noBoxMaze c))

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic


player :: Direction -> Picture
player R = translated 0 0.3 cranium
         & path [(0,0),(0.3,0.05)]
         & path [(0,0),(0.3,-0.05)]
         & path [(0,-0.2),(0,0.1)]
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & sector (7/6*pi) (1/6*pi) 0.18
player L = scaled (-1) 1 (player R) -- Cunning!
player U = translated 0 0.3 cranium
         & path [(0,0),(0.3,0.05)]
         & path [(0,0),(-0.3,0.05)]
         & path [(0,-0.2),(0,0.1)]
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = solidCircle 0.18
player D = translated 0 0.3 cranium
         & path [(0,0),(0.3,-0.05)]
         & path [(0,0),(-0.3,-0.05)]
         & path [(0,-0.2),(0,0.1)]
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & translated   0.06  0.08 (solidCircle 0.04)
                & translated (-0.06) 0.08 (solidCircle 0.04)

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

drawState :: State -> Picture
drawState s@(State c d l) =
                     hasWon s &
                     atCoord c (player d) &
                     pictureOfBoxes l &
                     pictureOfMaze

hasWon :: State -> Picture
hasWon s = if isWon s then text "You Won!" else blank

-- The complete interaction

sokoban :: Interaction State
sokoban = Interaction initialState (\_ c -> c) handleEvent drawState

-- The general interaction type

data Interaction world = Interaction
        world
        (Double -> world -> world)
        (Event -> world -> world)
        (world -> Picture)


runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle draw)
  = interactionOf state0 step handle draw

-- Resetable interactions

resetable :: Interaction s -> Interaction s
resetable (Interaction state0 step handle draw)
  = Interaction state0 step handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s              = handle e s

-- Start screen

startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!")

data SSState world = StartScreen | Running world

withStartScreen :: Interaction s  -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = StartScreen

    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)

    handle' (KeyPress key) StartScreen | key == " " = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s


-- The main function

main :: IO ()
main = runInteraction (resetable (withStartScreen sokoban))
