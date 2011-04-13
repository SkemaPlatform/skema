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
    ( singleton, assocs, adjust, keys, insert, fromList )
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
import Paths_skema( getDataFileName )
import Skema
    ( SkemaState(..), XS(..), io, runXS, trace
    , statePutSelectedPos, statePutSelectedElem, statePutSkemaDoc, stateGet )
import Skema.SkemaDoc
    ( SkemaDoc(..), Kernel(..), Node(..), Position(..), SelectedElement(..)
    , nodeTranslate, selectNode, isSelected )
import Skema.Editor.Canvas( drawSkemaDoc )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
                        (Map.fromList [(0,Kernel "Adder"),(1,Kernel "Scaler")])
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
      new_node = NodeKernel (Position x y) 1
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
                             drawSkemaDoc
                             (fromIntegral w) 
                             (fromIntegral h) 
                             stDoc)
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
