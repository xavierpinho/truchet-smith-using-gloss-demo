module Main where

import Graphics.Gloss
import System.Random
import Data.List (mapAccumL)

quarterCircle :: Float -> Picture
quarterCircle tileSize = Arc 0 90 (tileSize/2)

data Tile = TileA | TileB

drawTile :: Float -> Tile -> Picture
drawTile tileSize tile = case tile of
  TileA -> Rotate 90 (drawTile tileSize TileB)
  TileB -> Pictures [Translate (-offs) (-offs) arc1, Translate offs offs arc2]
    where arc1 = quarterCircle tileSize
          arc2 = Rotate 180 arc1
          offs = tileSize/2

layHorizontally :: Float -> [Picture] -> Picture
layHorizontally space = Pictures . snd . mapAccumL (\offs pict -> (offs + space, Translate offs 0 pict)) 0

layVertically :: Float -> [Picture] -> Picture
layVertically space = Pictures . snd . mapAccumL (\offs pict -> (offs + space, Translate 0 offs pict)) 0

drawRow :: Float -> [Tile] -> Picture
drawRow tileSize = layHorizontally tileSize . map (drawTile tileSize)

drawGrid :: Float -> [[Tile]] -> Picture
drawGrid tileSize = layVertically tileSize . map (drawRow tileSize) 

genTile :: StdGen -> (StdGen, Tile)
genTile g = (g', if rand then TileA else TileB)
  where (rand, g') = randomR (True, False) g

genRow :: StdGen -> Int -> (StdGen, [Tile])
genRow g width = mapAccumL (\g' _ -> genTile g') g [1..width]

genGrid :: StdGen -> Int -> Int -> (StdGen, [[Tile]])
genGrid g width height = mapAccumL (\g' _ -> genRow g' width) g [1..height] 

main = do
  let tileSize   = 20
  let gridWidth  = 20
  let gridHeight = 20
  let winWidth   = gridWidth * tileSize
  let winHeight  = gridHeight * tileSize
  let winXpos    = 0
  let winYpos    = 0
  let window     = InWindow "Truchet-Smith" (round winWidth, round winHeight) (winXpos, winYpos)
  let center     = Translate (-winWidth/2 + (tileSize/2)) (-winHeight/2 + (tileSize/2))
  g             <- newStdGen
  let grid       = snd (genGrid g (round gridWidth) (round gridHeight))
  let gridPict   = drawGrid tileSize grid
  display window white (center gridPict)
