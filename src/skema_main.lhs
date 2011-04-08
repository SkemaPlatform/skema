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
import qualified Data.Map as Map
    ( Map, singleton, elems, assocs, adjust, keys, insert )
import Data.List( sort )
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
import Skema
    ( SkemaState(..), XS(..), io, runXS, trace
    , statePutSelectedPos, statePutSelectedElem, statePutSkemaDoc, stateGet )
import Skema.SkemaDoc
    ( SkemaDoc(..), Kernel(..), Node(..), Position(..), SelectedElement(..)
    , nodePosx, nodePosy, nodeHeight, nodeWidth, nodePointRad, nodeHeadHeight
    , nodeTranslate, nodeLineColor, nodeBoxColor, nodeHeadColor, nodeName
    , selectNode, isSelected )
import Skema.Util( deg2rad )
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
           { skemaDoc = SkemaDoc 
                        (Map.singleton 0 (Kernel "Adder"))
                        (Map.singleton 0 (NodeKernel (Position 210 20) 0))
           , selectedPos = (0,0) 
           , selectedElem = SE_NOTHING }

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
         sks <- liftIO $ takeMVar state
         (_,new_sks) <- liftIO $ runXS sks $ selectElement mx my canvas
         liftIO $ putMVar state new_sks

  _ <- canvas `on` buttonReleaseEvent $ tryEvent $ do
         LeftButton <- eventButton
         (mx,my) <- eventCoordinates
         trace $ "release" ++ show (mx,my)
         
  _ <- canvas `on` leaveNotifyEvent $ tryEvent $ do
         (mx,my) <- eventCoordinates
         trace $ "out in" ++ show (mx,my)

  _ <- canvas `on` motionNotifyEvent $ tryEvent $ do
         (mx,my) <- eventCoordinates
         sks <- liftIO $ takeMVar state
         (_,new_sks) <- liftIO $ runXS sks $ moveTo mx my canvas
         liftIO $ putMVar state new_sks                    
        
  mainGUI
\end{code}

\begin{code}
selectElement :: Double -> Double -> DrawingArea -> XS ()
selectElement mx my canvas = do
  stDoc <- stateGet skemaDoc
  let sels = filter isSelected . map (selectNode mx my) . Map.assocs.nodes $ stDoc
  if null sels
    then do
      insertNewNode mx my
      statePutSelectedElem SE_NOTHING
      io $ widgetQueueDraw canvas
    else statePutSelectedElem (last sels)
  statePutSelectedPos (mx, my)
\end{code}

\begin{code}
insertNewNode :: Double -> Double -> XS ()
insertNewNode x y = do
  old_doc <- stateGet skemaDoc
  let last_i = ((last.sort.Map.keys.nodes) old_doc) + 1
      new_node = NodeKernel (Position x y) 0
  statePutSkemaDoc $ old_doc { 
                         nodes = Map.insert last_i new_node (nodes old_doc)}
\end{code}

\begin{code}
moveTo :: Double -> Double -> DrawingArea -> XS ()
moveTo mx my canvas = do
  stElem <- stateGet selectedElem
  moveSelectedElement mx my stElem
  io $ widgetQueueDraw canvas
\end{code}

\begin{code}
moveSelectedElement :: Double -> Double -> SelectedElement -> XS ()
moveSelectedElement _ _ SE_NOTHING = return ()
moveSelectedElement mx my (SE_NODE k) = do
  oldDoc <- stateGet skemaDoc
  (ox,oy) <- stateGet selectedPos
  let diffx = mx - ox
      diffy = my - oy
      new_nodes = Map.adjust (nodeTranslate diffx diffy) k (nodes oldDoc)

  statePutSkemaDoc $ oldDoc { nodes = new_nodes }
  statePutSelectedPos (mx,my)
\end{code}

\begin{code}
drawCanvas :: DrawWindow -> XS ()
drawCanvas canvas = do
  let drawable = castToDrawable canvas
  stDoc <- stateGet skemaDoc
  (w,h) <- io $ drawableGetSize drawable
  io $ renderWithDrawable canvas (
                             myDraw 
                             (fromIntegral w) 
                             (fromIntegral h) 
                             stDoc)
\end{code}

\begin{code}
myDraw :: Double -> Double -> SkemaDoc -> Cr.Render ()
myDraw _ _ skdoc = do
    Cr.setSourceRGB 0.45 0.45 0.45
    Cr.paint

    Cr.selectFontFace "arial" Cr.FontSlantNormal Cr.FontWeightNormal
    mapM_ (drawVisualNode skdoc) (Map.elems.nodes $ skdoc)
\end{code}

\begin{code}
drawVisualNode :: SkemaDoc -> Node -> Cr.Render ()
drawVisualNode skdoc node = do
    let px = nodePosx node
        py = nodePosy node
        wid = nodeWidth node
        hei = nodeHeight node
        headHeight = nodeHeadHeight node
        name = nodeName skdoc node
        rad = 4
        pointRad = nodePointRad node
        (rline, gline, bline) = nodeLineColor node
        (rbox, gbox, bbox) = nodeBoxColor node
        (rhead, ghead,bhead) = nodeHeadColor node

    -- shadow 1
    Cr.newPath
    Cr.arc (px+wid) (py+2*rad) rad (deg2rad $ -90) (deg2rad 0)
    Cr.arc (px+wid) (py+hei) rad (deg2rad 0) (deg2rad 90)
    Cr.arc px (py+hei) rad (deg2rad 90) (deg2rad 180)
    Cr.arc px (py+2*rad) rad (deg2rad 180) (deg2rad $ -90)
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
    Cr.lineTo (px+wid) (py+headHeight)
    Cr.lineTo px (py+headHeight)
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
    Cr.showText name

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
