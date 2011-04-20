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
module Skema.Editor.PFPreviewWindow( showPFPreviewWindow ) where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Control.Monad.Trans( liftIO )
import Control.Concurrent.MVar( MVar, takeMVar, putMVar )
import Graphics.UI.Gtk.Glade( xmlNew, xmlGetWidget )
import Graphics.UI.Gtk.Windows.Dialog( castToDialog, dialogRun )
import Graphics.UI.Gtk.Multiline.TextView( castToTextView, textViewGetBuffer )
import Graphics.UI.Gtk.Multiline.TextBuffer( textBufferSetText )
import Paths_skema( getDataFileName )
import Skema.Editor.SkemaState( SkemaState(..) )
import Skema.SkemaDoc( extractProgramFlow )
import Skema.ProgramFlow( generateJSONString )
import Skema.Util( prettyJSON )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
showPFPreviewWindow :: MVar SkemaState -> IO ()
showPFPreviewWindow state = do
  glade <- getDataFileName "pf_preview.glade"
  Just xml <- xmlNew glade
  window <- xmlGetWidget xml castToDialog "main"
 
  sks <- liftIO $ takeMVar state
  let json = prettyJSON . generateJSONString . extractProgramFlow . skemaDoc $ sks
  liftIO $ putMVar state sks
  
  tv <- xmlGetWidget xml castToTextView "text_view"
  tbuffer <- textViewGetBuffer tv
  textBufferSetText tbuffer json

  _ <- dialogRun window 

  return ()
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
