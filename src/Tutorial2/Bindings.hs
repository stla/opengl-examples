module Tutorial2.Bindings
  (idle, display, reshape, keyboardMouse)
  where
import           Graphics.UI.GLUT
import           Tutorial2.Display

reshape :: ReshapeCallback
reshape size =
  viewport $= (Position 0 0, size)

keyboardMouse :: KeyboardMouseCallback
keyboardMouse _key _state _modifiers _position = return ()
