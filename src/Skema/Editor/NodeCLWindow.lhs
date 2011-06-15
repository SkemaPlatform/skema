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
module Skema.Editor.NodeCLWindow( showNodeCLWindow ) where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Data.Maybe( isNothing )
import Control.Monad( when )
import Control.Concurrent.MVar( MVar )
import Skema.Editor.SkemaState( SkemaState(..) )
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView
import Paths_skema( getDataDir )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
showNodeCLWindow :: MVar SkemaState -> IO ()
showNodeCLWindow state = do
  window <- dialogNew
  
  datadir <- getDataDir
  
  slman <- sourceLanguageManagerGetDefault
  paths <- sourceLanguageManagerGetSearchPath slman
  sourceLanguageManagerSetSearchPath slman (Just $ paths ++ [datadir])
  slang <- sourceLanguageManagerGetLanguage slman "opencl"
  when (isNothing slang) $ print "error: no language file"
  
  slsty <- sourceStyleSchemeManagerGetDefault
  ssty <- sourceStyleSchemeManagerGetScheme slsty "classic"
  
  internal <- dialogGetUpper window
  sbuff <- sourceBufferNew Nothing
  sourceBufferSetLanguage sbuff slang
  sourceBufferSetStyleScheme sbuff (Just ssty)
  sourceBufferSetHighlightSyntax sbuff True
  
  sourceview <- sourceViewNewWithBuffer sbuff
  sw <- scrolledWindowNew Nothing Nothing
  containerAdd sw sourceview
  
  boxPackEnd internal sw PackGrow 0
  
  widgetShowAll window 
  
  _ <- dialogRun window 

  return ()
\end{code}
