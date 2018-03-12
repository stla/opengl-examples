module Utils.Colour
  where
import           Data.Colour.Palette.BrewerSet (ColorCat (..), brewerSet)
import           Data.Colour.Palette.ColorSet  (d3Colors1, webColors)
import           Data.Colour.SRGB.Linear       (channelBlue, channelGreen,
                                                channelRed, toRGB)
import           Graphics.Rendering.OpenGL.GL  (Color3 (..), Color4 (..), GLfloat)

pickColor3 :: Int -> Color3 GLfloat
pickColor3 i = Color3 r g b 
  where
    kolor = webColors i
    rgb = toRGB kolor
    r = realToFrac $ channelRed rgb
    g = realToFrac $ channelGreen rgb
    b = realToFrac $ channelBlue rgb

pickColor :: Int -> Color4 GLfloat
pickColor i = Color4 r g b 1
  where
    kolor = webColors i
    rgb = toRGB kolor
    r = realToFrac $ channelRed rgb
    g = realToFrac $ channelGreen rgb
    b = realToFrac $ channelBlue rgb

pickColor' :: Int -> GLfloat -> Color4 GLfloat
pickColor' i a = Color4 r g b a
  where
    kolor = d3Colors1 i
    rgb = toRGB kolor
    r = realToFrac $ channelRed rgb
    g = realToFrac $ channelGreen rgb
    b = realToFrac $ channelBlue rgb

pickColor'' :: Int -> GLfloat -> Color4 GLfloat
pickColor'' i a = Color4 r g b a
  where
    kolor = brewerSet RdYlGn 10 !! i
    rgb = toRGB kolor
    r = realToFrac $ channelRed rgb
    g = realToFrac $ channelGreen rgb
    b = realToFrac $ channelBlue rgb

interpolateColor :: (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat)
                 -> (GLfloat,GLfloat,GLfloat) -> GLfloat -> Color4 GLfloat
interpolateColor (r1,g1,b1) (r2,g2,b2) (r3,g3,b3) t =
  if t < 0.5
    then Color4 (interpolate r1 r2 (2*t))
                (interpolate g1 g2 (2*t))
                (interpolate b1 b2 (2*t))
                1
    else Color4 (interpolate r2 r3 (2*(t-0.5)))
                (interpolate g2 g3 (2*(t-0.5)))
                (interpolate b2 b3 (2*(t-0.5)))
                1
  where
    interpolate :: GLfloat -> GLfloat -> GLfloat -> GLfloat
    interpolate a b t' = a + t'*(b-a)
