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
import Data.Char( isAlphaNum, isAlpha )
import Control.Monad( when )
import Graphics.UI.Gtk( 
  on, get, containerAdd, widgetShowAll, widgetSetSizeRequest, scrolledWindowNew, 
  widgetDestroy, widgetGetState, widgetModifyBase, Color(..), windowSetDefault )
import Graphics.UI.Gtk.General.StockItems( stockApply, stockCancel )
import Graphics.UI.Gtk.Abstract.Box( Packing(..), boxPackStart )
import Graphics.UI.Gtk.Windows.Dialog( 
  ResponseId(..), dialogNew, dialogRun, dialogGetUpper, dialogAddButton )
import Graphics.UI.Gtk.Display.Label( labelNew )
import Graphics.UI.Gtk.Entry.Editable( editableChanged )
import Graphics.UI.Gtk.Entry.Entry( entryNew, entryText, entrySetText )
import Graphics.UI.Gtk.Layout.HBox( hBoxNew )
import Graphics.UI.Gtk.Multiline.TextBuffer( textBufferSetText, textBufferText )
import Graphics.UI.Gtk.SourceView( 
  sourceLanguageManagerGetDefault, sourceLanguageManagerGetSearchPath, 
  sourceLanguageManagerSetSearchPath, sourceLanguageManagerGetLanguage, 
  sourceStyleSchemeManagerGetDefault, sourceStyleSchemeManagerGetScheme, 
  sourceBufferNew, sourceBufferSetLanguage, sourceBufferSetStyleScheme, 
  sourceBufferSetHighlightSyntax, sourceViewNewWithBuffer, 
  sourceBufferBeginNotUndoableAction, sourceBufferEndNotUndoableAction )
import Skema.SkemaDoc( Kernel(..) )
import Paths_skema( getDataDir )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
badColor, goodColor :: Color
badColor = Color (229*256) (142*256) (142*256)
goodColor = Color (180*256) (244*256) (210*256)
\end{code}

\begin{code}
showNodeCLWindow :: Kernel -> [String] -> IO Kernel
showNodeCLWindow krn usedNames = do
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
  sourceBufferBeginNotUndoableAction sbuff
  textBufferSetText sbuff (body krn)
  sourceBufferEndNotUndoableAction sbuff

  sourceview <- sourceViewNewWithBuffer sbuff
  sw <- scrolledWindowNew Nothing Nothing
  containerAdd sw sourceview
  
  hbox0 <- hBoxNew True 0
  lblName <- labelNew $ Just "Name:"
  boxPackStart hbox0 lblName PackNatural 0
  eName <- entryNew 
  entrySetText eName $ name krn
  eNameState <- widgetGetState eName 
  widgetModifyBase eName eNameState goodColor
  boxPackStart hbox0 eName PackGrow 0
  boxPackStart internal hbox0 PackNatural 0
  
  lbl0 <- labelNew $ Just "Input Parameters"
  lbl1 <- labelNew $ Just "Output Parameters"
  boxPackStart internal lbl0 PackNatural 0
  boxPackStart internal lbl1 PackNatural 0
  lbl2 <- labelNew $ Just "Kernel Body"
  boxPackStart internal lbl2 PackNatural 0
  boxPackStart internal sw PackGrow 0
  
  _ <- eName `on` editableChanged $ do
    newName <- get eName entryText
    if validName newName usedNames 
      then widgetModifyBase eName eNameState goodColor
      else widgetModifyBase eName eNameState badColor

  acceptButton <- dialogAddButton window stockApply ResponseAccept
  _ <- dialogAddButton window stockCancel ResponseReject
  
  windowSetDefault window $ Just acceptButton
  
  widgetShowAll window 
  
  resp <- dialogRun window 
  
  newkrn <- case resp of
    ResponseAccept -> do
      newBody <- get sbuff textBufferText
      newName <- get eName entryText
      return krn { body = newBody, 
                   name = if validName newName usedNames
                          then newName 
                          else name krn 
                 }
  
    _ -> return krn
    
  widgetDestroy window
  return newkrn
\end{code}

\begin{code}
validName :: String -> [String] -> Bool
validName cad xs = checkLength && checkDigits 
                   && (checkInit.head) cad && cad `notElem` xs
  where
    checkLength = (length cad) > 0
    checkDigits = all checkChar cad
    checkChar c = (isAlphaNum c) || (c == '_')
    checkInit c = (isAlpha c) || (c == '_')
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
