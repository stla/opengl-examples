module Utils.Colour
  where
import           Data.Colour.Palette.ColorSet (webColors)
import           Data.Colour.SRGB.Linear      (channelBlue, channelGreen,
                                               channelRed, toRGB)
import           Graphics.Rendering.OpenGL.GL (GLfloat, Color4 (..))

pickColor :: Int -> Color4 GLfloat
pickColor i = Color4 r g b 1
  where
    kolor = webColors i
    rgb = toRGB kolor
    r = realToFrac $ channelRed rgb
    g = realToFrac $ channelGreen rgb
    b = realToFrac $ channelBlue rgb
