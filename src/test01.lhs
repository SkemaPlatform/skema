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
import Graphics.UI.Gtk
    ( mainQuit, initGUI, mainGUI, onDestroy, onExpose
    , castToWindow, widgetShowAll, widgetGetSize
    , widgetGetDrawWindow, renderWithDrawable
    , widgetModifyBg, StateType(..), Color(..) )
import Graphics.UI.Gtk.Gdk.Events( eventSent )
import Graphics.UI.Gtk.Misc.DrawingArea( castToDrawingArea )
import Graphics.UI.Gtk.Glade( xmlNew, xmlGetWidget )
import qualified Graphics.Rendering.Cairo as Cr
import Paths_skema( getDataFileName )
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
  _ <- onExpose canvas (\x -> do 
                          (w,h) <- widgetGetSize canvas
                          drawin <- widgetGetDrawWindow canvas
                          renderWithDrawable drawin (myDraw (fromIntegral w) (fromIntegral h))
                          return (eventSent x))
                         
  mainGUI
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
