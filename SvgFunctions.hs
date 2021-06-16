module SvgFunctions where
import Text.Printf ( printf )

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point, Float)

svgRect :: Rect -> String -> String
svgRect ((x,y),w,h) = printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h


svgCircle :: Circle -> String -> String
svgCircle ((x, y), r) = printf "<circle cx='%f' cy='%f' r='%f' fill='%s' />\n" x y r

svgBegin :: Float -> Float -> String
svgBegin = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n"

svgEnd :: String
svgEnd = "</svg>"

svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b

svgStyle2 :: (Int,Int,Int) -> String
svgStyle2 (r,g,b) = printf "rgb(%d,%d,%d)" r g b

svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles