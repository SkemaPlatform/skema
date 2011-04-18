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
module Skema.Editor.MainWindow( prepareMainWindow ) where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Control.Monad( when )
import Control.Monad.Trans( liftIO )
import Control.Concurrent.MVar( MVar, takeMVar, putMVar )
import qualified Data.IntMap as M( adjust, keys, insert )
import Data.List( sort )
import Data.Maybe( isNothing, isJust, fromJust )
import Graphics.UI.Gtk
    ( on, renderWithDrawable, eventWindow, castToDrawable, drawableGetSize
    , DrawWindow, DrawingArea )
import Graphics.UI.Gtk.Abstract.Widget
    ( widgetAddEvents, exposeEvent, buttonPressEvent, buttonReleaseEvent
    , leaveNotifyEvent, motionNotifyEvent, widgetQueueDraw
    , EventMask(..) )
import Graphics.UI.Gtk.Gdk.EventM
    ( tryEvent, eventButton, eventClick, eventCoordinates
    , MouseButton(..), Click(..) )
import Graphics.UI.Gtk.Misc.DrawingArea( castToDrawingArea )
import Graphics.UI.Gtk.Glade( GladeXML, xmlGetWidget )
import Skema.Editor.SkemaState
    ( SkemaState(..), XS(..), io, runXS, trace, statePutSelectedPos
    , statePutSelectedPos2, statePutSelectedElem, statePutSkemaDoc
    , stateGet, stateSelectElement )
import Skema.Editor.Canvas( drawSkemaDoc, drawSelected )
import Skema.SkemaDoc
    ( SkemaDoc(..), Node(..), SelectedElement(..)
    , nodeTranslate )
import Skema.Util( Pos2D(..) )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
prepareMainWindow :: GladeXML -> MVar SkemaState -> IO ()
prepareMainWindow xml state = do
  canvas <- xmlGetWidget xml castToDrawingArea "canvas"
  
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
         sks <- liftIO $ takeMVar state
         (_,new_sks) <- liftIO $ runXS sks $ releaseElement mx my 
         liftIO $ putMVar state new_sks
         liftIO $ widgetQueueDraw canvas
         
  _ <- canvas `on` leaveNotifyEvent $ tryEvent $ do
         (mx,my) <- eventCoordinates
         trace $ "out in" ++ show (mx,my)

  _ <- canvas `on` motionNotifyEvent $ tryEvent $ do
         (mx,my) <- eventCoordinates
         sks <- liftIO $ takeMVar state
         (_,new_sks) <- liftIO $ runXS sks $ moveTo mx my canvas
         liftIO $ putMVar state new_sks
  
  return ()
\end{code}

\begin{code}
selectElement :: Double -> Double -> DrawingArea -> XS ()
selectElement mx my canvas = do
  selElement <- stateSelectElement mx my
  when (isNothing selElement) $ do
                               insertNewNode mx my
                               io $ widgetQueueDraw canvas
  statePutSelectedElem selElement
  statePutSelectedPos (mx, my)
\end{code}

\begin{code}
insertNewNode :: Double -> Double -> XS ()
insertNewNode x y = do
  old_doc <- stateGet skemaDoc
  let last_i = (last.sort.M.keys.nodes) old_doc + 1
      new_node = NodeKernel (Pos2D (x,y)) 1
  statePutSkemaDoc $ old_doc { 
                         nodes = M.insert last_i new_node (nodes old_doc)}
\end{code}

\begin{code}
moveTo :: Double -> Double -> DrawingArea -> XS ()
moveTo mx my canvas = do
  stElem <- stateGet selectedElem
  when (isJust stElem) $ do
                        moveSelectedElement mx my (fromJust stElem)
                        io $ widgetQueueDraw canvas
\end{code}

\begin{code}
moveSelectedElement :: Double -> Double -> SelectedElement -> XS ()
moveSelectedElement mx my (SeNODE k) = do
  oldDoc <- stateGet skemaDoc
  (ox,oy) <- stateGet selectedPos
  let diffx = mx - ox
      diffy = my - oy
      new_nodes = M.adjust (nodeTranslate diffx diffy) k (nodes oldDoc)

  statePutSkemaDoc $ oldDoc { nodes = new_nodes }
  statePutSelectedPos (mx,my)
moveSelectedElement mx my (SeIOP k j) = do
  statePutSelectedPos2 (mx,my)
\end{code}

\begin{code}
releaseElement :: Double -> Double -> XS ()
releaseElement _ _ = do
  statePutSelectedElem Nothing
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
     
  stElem <- stateGet selectedElem
  (ox,oy) <- stateGet selectedPos
  (mx,my) <- stateGet selectedPos2
  io $ renderWithDrawable canvas (
                                  drawSelected
                                  (fromIntegral w) 
                                  (fromIntegral h)
                                  stElem
                                  ox oy
                                  mx my
                                 )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
