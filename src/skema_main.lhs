\begin{code}
import Control.Monad.Trans( liftIO )
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Misc.DrawingArea( castToDrawingArea )
import Graphics.UI.Gtk.Glade( xmlNew, xmlGetWidget )
import qualified Graphics.Rendering.Cairo as Cr
import Paths_skema( getDataFileName )
\end{code}

\begin{code}
main :: IO ()
main= do
  initGUI
  glade <- getDataFileName "skema.glade"
  Just xml <- xmlNew glade
  window <- xmlGetWidget xml castToWindow "main"
  onDestroy window mainQuit
 
  canvas <- xmlGetWidget xml castToDrawingArea "canvas"
  
  widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

  widgetShowAll window 

  canvas `on` exposeEvent $ tryEvent exposeCanvas

  canvas `on` buttonPressEvent $ tryEvent $ do
        liftIO $ putStrLn "key press event"

  onDestroy window mainQuit
  mainGUI
\end{code}

\begin{code}
exposeCanvas = do
  canvas <- eventWindow
  liftIO $ drawCanvas canvas
\end{code}

\begin{code}
drawCanvas :: DrawWindow -> IO ()
drawCanvas canvas = do
  let drawable = castToDrawable canvas
  (w,h) <- drawableGetSize drawable
  renderWithDrawable canvas (myDraw (fromIntegral w) (fromIntegral h))
  return ()
\end{code}

\begin{code}
myDraw :: Double -> Double -> Cr.Render ()
myDraw w h = do
    Cr.setSourceRGB 0.7 0.7 0.7
    Cr.paint

    Cr.setSourceRGB 0 1 1
    Cr.setLineWidth 5

    Cr.moveTo (0.5 * w) (0.43 * h)
    Cr.lineTo (0.33 * w) (0.71 * h)
    Cr.lineTo (0.66 * w) (0.71 * h)

    Cr.closePath

    Cr.stroke

    Cr.setSourceRGB 0.4 1 1
    Cr.rectangle (0.1 * w) (0.1 * h) (0.4 * w) (0.25 * h)
    Cr.fill
    Cr.setSourceRGB 0.7 1 1
    Cr.rectangle (0.1 * w) (0.1 * h) (0.4 * w) (0.25 * h)
    Cr.stroke

    Cr.setSourceRGB 0 0 0
    Cr.moveTo 0 0
    Cr.lineTo w h
    Cr.moveTo w 0
    Cr.lineTo 0 h
    Cr.setLineWidth (0.1 * (h + w))
    Cr.stroke

    Cr.rectangle 0 0 (0.5 * w) (0.5 * h)
    Cr.setSourceRGBA 1 0 0 0.8
    Cr.fill

    Cr.rectangle 0 (0.5 * h) (0.5 * w) (0.5 * h)
    Cr.setSourceRGBA 0 1 0 0.6
    Cr.fill

    Cr.rectangle (0.5 * w) 0 (0.5 * w) (0.5 * h)
    Cr.setSourceRGBA 0 0 1 0.4
    Cr.fill
    Cr.setSourceRGB 0 1 0
    Cr.selectFontFace "Arial" Cr.FontSlantNormal Cr.FontWeightNormal
    Cr.setFontSize 20
    Cr.moveTo 0 10
    Cr.showText "test"
\end{code}
