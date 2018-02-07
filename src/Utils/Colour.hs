module Utils.Colour
  where
import           Data.Colour.Palette.BrewerSet (ColorCat (..), brewerSet)
import           Data.Colour.Palette.ColorSet  (d3Colors1, webColors)
import           Data.Colour.SRGB.Linear       (channelBlue, channelGreen,
                                                channelRed, toRGB)
import           Graphics.Rendering.OpenGL.GL  (Color4 (..), GLfloat)

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
