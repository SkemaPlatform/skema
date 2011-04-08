%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file is part of Skema.

% Skema is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.

% Skema is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.

% You should have received a copy of the GNU General Public License
% along with Skema.  If not, see <http://www.gnu.org/licenses/>.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Control.Monad.Trans( liftIO )
import Control.Concurrent.MVar( newMVar, takeMVar, putMVar )
import Graphics.UI.Gtk
    ( on, mainQuit, initGUI, mainGUI, onDestroy
    , castToWindow, widgetShowAll, renderWithDrawable
    , widgetModifyBg, StateType(..), Color(..)
    , eventWindow, castToDrawable, drawableGetSize
    , DrawWindow, DrawingArea )
import Graphics.UI.Gtk.Abstract.Widget
    ( widgetAddEvents, exposeEvent, buttonPressEvent, buttonReleaseEvent
    , leaveNotifyEvent, motionNotifyEvent, widgetQueueDraw
    , EventMask(..) )
import Graphics.UI.Gtk.Gdk.EventM
    ( tryEvent, eventButton, eventClick, eventCoordinates
    , MouseButton(..), Click(..) )
import Graphics.UI.Gtk.Misc.DrawingArea( castToDrawingArea )
import Graphics.UI.Gtk.Glade( xmlNew, xmlGetWidget )
import qualified Graphics.Rendering.Cairo as Cr
import Paths_skema( getDataFileName )
import Skema( SkemaState(..), XS(..), io, runXS, get, put )
import Skema.SkemaDoc
    ( SkemaDoc(..), VisualNode(..), Position(..), currentHeight, currentWidth
    , nodePointRad, nodeLineColor, nodeBoxColor, nodeHeadColor )
import Skema.Util( deg2rad )
\end{code}

\begin{code}
data SelectedElement = SE_NOTHING
                     | SE_NODE
                     | SE_NODE_BAR
                     | SE_INOUT
                       deriving( Show )
\end{code}

\begin{code}
main :: IO ()
main= do
  _ <- initGUI
  glade <- getDataFileName "skema.glade"
  Just xml <- xmlNew glade
  window <- xmlGetWidget xml castToWindow "main"
  _ <- onDestroy window mainQuit
 
  canvas <- xmlGetWidget xml castToDrawingArea "canvas"
  
  widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

  widgetShowAll window 

  let st = SkemaState 
           { doc = SkemaDoc [] }

  state <- newMVar st

  widgetAddEvents canvas [Button1MotionMask]

  _ <- canvas `on` exposeEvent $ tryEvent $ do
         eventCanvas <- eventWindow
         sks <- liftIO $ takeMVar state
         (_,new_sks) <- liftIO $ runXS sks $ drawCanvas eventCanvas
         liftIO $ putMVar state new_sks

  _ <- canvas `on` buttonPressEvent $ tryEvent $ do
         LeftButton <- eventButton
         SingleClick <- eventClick
         (mx,my) <- eventCoordinates
         liftIO $ print (mx,my)
         sks <- liftIO $ takeMVar state
         (_,new_sks) <- liftIO $ runXS sks $ testButton canvas
         liftIO $ putMVar state new_sks

  _ <- canvas `on` buttonReleaseEvent $ tryEvent $ do
         LeftButton <- eventButton
         (mx,my) <- eventCoordinates
         liftIO $ putStrLn $ "release" ++ show (mx,my)
         
  _ <- canvas `on` leaveNotifyEvent $ tryEvent $ do
         (mx,my) <- eventCoordinates
         liftIO $ putStrLn $ "out in" ++ show (mx,my)

  _ <- canvas `on` motionNotifyEvent $ tryEvent $ do
         (mx,my) <- eventCoordinates
         liftIO $ putStrLn $ "move to" ++ show (mx,my)
        
  _ <- canvas `on` buttonPressEvent $ tryEvent $ do
         RightButton <- eventButton
         liftIO $ putStrLn "boton derecho"

  mainGUI
\end{code}

\begin{code}
--selectNode :: 
\end{code}

\begin{code}
testButton :: DrawingArea -> XS ()
testButton canvas = do
  state <- get
  let old_doc = doc state
      old_nodes = nodes old_doc
      new_node = VisualNode $ Position (20 + 50*(fromIntegral$length old_nodes)) (20 + 20*(fromIntegral$length old_nodes))
  put $ state {
               doc = old_doc { 
                       nodes = old_nodes ++ [new_node]}}
  io $ widgetQueueDraw canvas
\end{code}

\begin{code}
drawCanvas :: DrawWindow -> XS ()
drawCanvas canvas = do
  let drawable = castToDrawable canvas
  state <- get
  (w,h) <- io $ drawableGetSize drawable
  io $ renderWithDrawable canvas (
                             myDraw 
                             (fromIntegral w) 
                             (fromIntegral h) 
                             (doc state))
  return ()
\end{code}

\begin{code}
myDraw :: Double -> Double -> SkemaDoc -> Cr.Render ()
myDraw _ _ skdoc = do
    Cr.setSourceRGB 0.45 0.45 0.45
    Cr.paint

    Cr.selectFontFace "arial" Cr.FontSlantNormal Cr.FontWeightNormal
    mapM_ drawVisualNode (nodes skdoc)
\end{code}

\begin{code}
drawVisualNode :: VisualNode -> Cr.Render ()
drawVisualNode node = do
    let px = posx.position $ node
        py = posy.position $ node
        wid = currentWidth node
        hei = currentHeight node
        rad = 4
        pointRad = nodePointRad node
        (rline, gline, bline) = nodeLineColor node
        (rbox, gbox, bbox) = nodeBoxColor node
        (rhead, ghead,bhead) = nodeHeadColor node

    -- shadow 1
    Cr.newPath
    Cr.arc (px+wid) (py+2*rad) rad (deg2rad $ -90) (deg2rad 0)
    Cr.arc (px+wid) (py+hei) rad (deg2rad 0) (deg2rad 90)
    Cr.arc (px) (py+hei) rad (deg2rad 90) (deg2rad 180)
    Cr.arc (px) (py+2*rad) rad (deg2rad 180) (deg2rad $ -90)
    Cr.closePath
    Cr.setSourceRGBA 0 0 0 0.2
    Cr.fill

    -- shadow 2
    Cr.newPath
    Cr.arc (px+wid-2) (py+2*rad+2) rad (deg2rad $ -90) (deg2rad 0)
    Cr.arc (px+wid-2) (py+hei-2) rad (deg2rad 0) (deg2rad 90)
    Cr.arc (px+2) (py+hei-2) rad (deg2rad 90) (deg2rad 180)
    Cr.arc (px+2) (py+2*rad+2) rad (deg2rad 180) (deg2rad $ -90)
    Cr.closePath
    Cr.setSourceRGBA 0 0 0 0.2
    Cr.fill

    -- box
    Cr.newPath
    Cr.arc (px+wid-rad) (py+rad) rad (deg2rad $ -90) (deg2rad 0)
    Cr.arc (px+wid-rad) (py+hei-rad) rad (deg2rad 0) (deg2rad 90)
    Cr.arc (px+rad) (py+hei-rad) rad (deg2rad 90) (deg2rad 180)
    Cr.arc (px+rad) (py+rad) rad (deg2rad 180) (deg2rad $ -90)
    Cr.closePath
    Cr.setSourceRGB rbox gbox bbox
    Cr.fillPreserve
    Cr.setLineWidth 1
    Cr.setSourceRGB rline gline bline
    Cr.stroke

    -- header
    Cr.newPath
    Cr.arc (px+wid-rad) (py+rad) rad (deg2rad $ -90) (deg2rad 0)
    Cr.lineTo (px+wid) (py+12)
    Cr.lineTo px (py+12)
    Cr.arc (px+rad) (py+rad) rad (deg2rad 180) (deg2rad $ -90)
    Cr.closePath
    Cr.setSourceRGB rhead ghead bhead
    Cr.fill

    -- in/out points
    Cr.setLineWidth 1
    Cr.arc px (py+hei-20) pointRad (deg2rad 0) (deg2rad 360)
    Cr.setSourceRGB 0.78 0.78 0.16
    Cr.fillPreserve
    Cr.setSourceRGB rline gline bline
    Cr.stroke

    Cr.arc px (py+hei-30) pointRad (deg2rad 0) (deg2rad 360)
    Cr.setSourceRGB 0.78 0.78 0.16
    Cr.fillPreserve
    Cr.setSourceRGB rline gline bline
    Cr.stroke

    Cr.arc (px+wid) (py+20) pointRad (deg2rad 0) (deg2rad 360)
    Cr.setSourceRGB 0.16 0.78 0.78
    Cr.fillPreserve
    Cr.setSourceRGB rline gline bline
    Cr.stroke

    Cr.setSourceRGB 1 1 1
    Cr.setFontSize 10
    Cr.moveTo (px + 2) (py + 11)
    Cr.showText "Test Node"

    Cr.setSourceRGB 1 1 1
    Cr.setFontSize 8
    Cr.moveTo (px+6) (py+hei-18)
    Cr.showText "in x"
    Cr.moveTo (px+6) (py+hei-28)
    Cr.showText "in y"
    textSize <- Cr.textExtents "out z"
    Cr.moveTo (px+wid-(Cr.textExtentsWidth textSize)-6) (py+22)
    Cr.showText "out z"
\end{code}
