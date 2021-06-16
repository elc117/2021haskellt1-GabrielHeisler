module Main (main) where
import Text.Printf()
import SvgFunctions
    ( svgRect, Rect, svgBegin, svgEnd, svgStyle, svgElements, Circle, svgCircle, svgStyle2 )


redPalette :: [(Int,Int,Int)]
redPalette = cycle [(75,0,130),(90,10,140),(100,20,180)]

bluePalette :: [(Int,Int,Int)]
bluePalette = cycle [(0,20,150),(0,30,180),(0,40,220)]

lightBluePalette :: [(Int,Int,Int)]
lightBluePalette = [(117, 213, 227-(i*25))| i <- [0..25]]

yellowPalette :: [(Int,Int,Int)]
yellowPalette = cycle [(250,200,30),(250,180,40),(240,150,20)]


genHorizontalStripes :: Float -> Float -> Float -> Float -> [Rect]
genHorizontalStripes w h size lines = [((0, h*m), w, (h*(size/1000))*m) | m <- [0.9375, 0.8125..0.0625]]

genOcean :: Float -> Float -> [Rect]
genOcean w h = [((0, h/1.75+m), w, w/50)| m <- [0, w/50..w/2]]

genSky :: Float -> Float -> [Rect]
genSky w h = [((0, 0+m), w, w/10)| m <- [0, w/10..w/2]]

genSun :: Float -> Float -> Float -> [Circle]
genSun x y r = [((x, y), r-m*r/10) | m <- [0..(r/10)]]

main :: IO ()
main =
  writeFile "main.svg" svgstrs
  where svgstrs = svgBegin w h ++ sky ++ sun  ++ ocean ++ horStripes ++ svgEnd

        ocean = svgElements svgRect getOcean (map svgStyle bluePalette)
        horStripes = svgElements svgRect getHorizontalStripes (map svgStyle redPalette)
        sky = svgElements svgRect getSky (map svgStyle lightBluePalette)
        sun = svgElements svgCircle getSun (map svgStyle2 yellowPalette)

        getSun = genSun sun_position_x sun_position_y sun_radius
        getSky = genSky w h
        getOcean = genOcean w h
        getHorizontalStripes = genHorizontalStripes w h size lines

        --Size é a variável usada para defiinir a espessura das linhas horizontais (0 ou <0 retira as linhas)
        size = 30
        lines = 8

        --Variaveis para definir x, y e raio do sol
        sun_position_x = w/2
        sun_position_y = h/1.75
        sun_radius = h/3.75

        --Variaveis para definir largura e altura da tela (Largura -> w || Altura -> h)
        (w,h) = (800, 800)