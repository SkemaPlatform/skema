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
import Graphics.UI.Gtk( 
  get, containerAdd, widgetShowAll, widgetSetSizeRequest, scrolledWindowNew, 
  widgetDestroy )
import Graphics.UI.Gtk.General.StockItems( stockApply, stockCancel )
import Graphics.UI.Gtk.Abstract.Box( Packing(..), boxPackStart )
import Graphics.UI.Gtk.Windows.Dialog( 
  ResponseId(..), dialogNew, dialogRun, dialogGetUpper, dialogAddButton )
import Graphics.UI.Gtk.Display.Label( labelNew )
import Graphics.UI.Gtk.Multiline.TextBuffer( textBufferSetText, textBufferText )
import Graphics.UI.Gtk.SourceView( 
  sourceLanguageManagerGetDefault, sourceLanguageManagerGetSearchPath, 
  sourceLanguageManagerSetSearchPath, sourceLanguageManagerGetLanguage, 
  sourceStyleSchemeManagerGetDefault, sourceStyleSchemeManagerGetScheme, 
  sourceBufferNew, sourceBufferSetLanguage, sourceBufferSetStyleScheme, 
  sourceBufferSetHighlightSyntax, sourceViewNewWithBuffer )
import Skema.SkemaDoc( Kernel(..) )
import Paths_skema( getDataDir )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
showNodeCLWindow :: Kernel -> IO Kernel
showNodeCLWindow krn = do
  window <- dialogNew
  widgetSetSizeRequest window 640 480
  
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
  textBufferSetText sbuff (body krn)

  sourceview <- sourceViewNewWithBuffer sbuff
  sw <- scrolledWindowNew Nothing Nothing
  containerAdd sw sourceview
  
  lbl0 <- labelNew $ Just "Input Parameters"
  lbl1 <- labelNew $ Just "Output Parameters"
  boxPackStart internal lbl0 PackNatural 0
  boxPackStart internal lbl1 PackNatural 0
  lbl2 <- labelNew $ Just "Kernel Body"
  boxPackStart internal lbl2 PackNatural 0
  boxPackStart internal sw PackGrow 0

  _ <- dialogAddButton window stockApply ResponseAccept
  _ <- dialogAddButton window stockCancel ResponseReject
  
  widgetShowAll window 
  
  resp <- dialogRun window 
  widgetDestroy window
  print resp
  
  newkrn <- case resp of
    ResponseAccept -> do
      newBody <- get sbuff textBufferText
      return krn { body = newBody }
      
    _ -> return krn
  
  return newkrn
\end{code}
