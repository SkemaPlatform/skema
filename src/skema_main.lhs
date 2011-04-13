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
import Control.Concurrent.MVar( newMVar )
import qualified Data.Map as M( singleton, fromList )
import Graphics.UI.Gtk
    ( mainQuit, initGUI, mainGUI, onDestroy, castToWindow, widgetShowAll )
import Graphics.UI.Gtk.Glade( xmlNew, xmlGetWidget )
import Paths_skema( getDataFileName )
import Skema( SkemaState(..) )
import Skema.SkemaDoc
    ( SkemaDoc(..), Kernel(..), Node(..), Position(..), SelectedElement(..) )
import Skema.Editor.MainWindow( prepareMainWindow )
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
 
  let st = SkemaState 
           { skemaDoc = SkemaDoc 
                        (M.fromList [(0,Kernel "Adder"),(1,Kernel "Scaler")])
                        (M.singleton 0 (NodeKernel (Position 210 20) 0))
           , selectedPos = (0,0) 
           , selectedElem = SeNOTHING }

  state <- newMVar st

  prepareMainWindow xml state      
  
  widgetShowAll window 

  mainGUI
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
