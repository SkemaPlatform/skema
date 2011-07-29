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
import System.Glib.Attributes( AttrOp(..) )
import Graphics.UI.Gtk( 
  on, get, containerAdd, widgetShowAll, widgetSetSizeRequest, 
  scrolledWindowNew, widgetDestroy, widgetGetState, widgetModifyBase, Color(..), 
  windowSetDefault, bufferChanged )
import Graphics.UI.Gtk.General.StockItems( stockApply, stockCancel )
import Graphics.UI.Gtk.Abstract.Box( Packing(..), boxPackStart )
import Graphics.UI.Gtk.Windows.Dialog( 
  ResponseId(..), dialogNew, dialogRun, dialogGetUpper, dialogAddButton, 
  dialogSetResponseSensitive )
import Graphics.UI.Gtk.Display.Label( labelNew )
import Graphics.UI.Gtk.Entry.Editable( editableChanged )
import Graphics.UI.Gtk.Entry.Entry( entryNew, entryText, entrySetText )
import Graphics.UI.Gtk.Layout.HBox( hBoxNew )
import Graphics.UI.Gtk.Layout.VBox( vBoxNew )
import Graphics.UI.Gtk.Multiline.TextBuffer( textBufferSetText, textBufferText )
import Graphics.UI.Gtk.ModelView( 
  TreeView, treeViewNew, treeViewSetModel, treeViewSetHeadersVisible, 
  treeViewColumnNew, treeViewColumnSetTitle, treeViewColumnPackStart, 
  treeViewAppendColumn, ListStore, listStoreNew, listStoreGetValue, 
  listStoreSetValue, cellRendererTextNew, cellLayoutSetAttributes, cellText, 
  cellTextEditable, cellRendererComboNew, cellComboTextModel, 
  cellComboHasEntry, makeColumnIdString, customStoreSetColumn, edited, 
  cellRendererToggleNew )
import Graphics.UI.Gtk.SourceView( 
  sourceLanguageManagerGetDefault, sourceLanguageManagerGetSearchPath, 
  sourceLanguageManagerSetSearchPath, sourceLanguageManagerGetLanguage, 
  sourceStyleSchemeManagerGetDefault, sourceStyleSchemeManagerGetScheme, 
  sourceBufferNew, sourceBufferSetLanguage, sourceBufferSetStyleScheme, 
  sourceBufferSetHighlightSyntax, sourceViewNewWithBuffer, 
  sourceBufferBeginNotUndoableAction, sourceBufferEndNotUndoableAction )
import Skema.SkemaDoc( Kernel(..) )
import Skema.Types( openclTypeNames )
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
  
  -- kernel name widgets
  hbox0 <- hBoxNew True 0
  lblName <- labelNew $ Just "Name:"
  boxPackStart hbox0 lblName PackNatural 0
  eName <- entryNew 
  entrySetText eName $ name krn
  eNameState <- widgetGetState eName 
  widgetModifyBase eName eNameState goodColor
  boxPackStart hbox0 eName PackGrow 0
  boxPackStart internal hbox0 PackNatural 0
  
  -- parameters
  hbox1 <- hBoxNew True 0  
  boxPackStart internal hbox1 PackNatural 0
  
  -- input widgets
  vboxInput <- vBoxNew False 0
  boxPackStart hbox1 vboxInput PackNatural 0
  lbl0 <- labelNew $ Just "Input Parameters"
  boxPackStart vboxInput lbl0 PackNatural 0
  
  inputList <- treeViewNew
  storeInputs <- listStoreNew [("test1","float"),("test2","int")] :: IO (ListStore (String,String))
  boxPackStart vboxInput inputList PackNatural 0
  
  setupParameterList inputList storeInputs $ 
    dialogSetResponseSensitive window ResponseAccept True
  
  -- output widgets
  vboxOutput <- vBoxNew False 0
  boxPackStart hbox1 vboxOutput PackNatural 0
  lbl1 <- labelNew $ Just "Output Parameters"
  boxPackStart vboxOutput lbl1 PackNatural 0
  
  outputList <- treeViewNew
  storeOutputs <- listStoreNew [("test1","float")] :: IO (ListStore (String,String))
  boxPackStart vboxOutput outputList PackNatural 0
  
  setupParameterList outputList storeOutputs $ 
    dialogSetResponseSensitive window ResponseAccept True
  
  -- body widgets
  lbl2 <- labelNew $ Just "Kernel Body"
  boxPackStart internal lbl2 PackNatural 0
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
  boxPackStart internal sw PackGrow 0
  
  -- buttons
  acceptButton <- dialogAddButton window stockApply ResponseAccept
  _ <- dialogAddButton window stockCancel ResponseReject  
  windowSetDefault window $ Just acceptButton
  
  dialogSetResponseSensitive window ResponseAccept False
  
  widgetShowAll window 
  
  -- events
  _ <- eName `on` editableChanged $ do
    newName <- get eName entryText
    dialogSetResponseSensitive window ResponseAccept True
    if validName newName usedNames 
      then widgetModifyBase eName eNameState goodColor
      else widgetModifyBase eName eNameState badColor
           
  _ <- sbuff `on` bufferChanged $ do
    dialogSetResponseSensitive window ResponseAccept True
    
  
  -- get the response and return
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
setupParameterList :: TreeView -> ListStore (String,String) -> IO () -> IO ()
setupParameterList list store applyChanged = do
  treeViewSetModel list store
  treeViewSetHeadersVisible list True
  
  col1 <- treeViewColumnNew
  renderer1 <- cellRendererTextNew
  treeViewColumnSetTitle col1 "Name"
  treeViewColumnPackStart col1 renderer1 True
  cellLayoutSetAttributes col1 renderer1 store $ \(r,_) -> [ 
    cellText := r, cellTextEditable := True ]
  _ <- treeViewAppendColumn list col1
  _ <- renderer1 `on` edited $ \ns str -> do
    when (not . null $ ns) $ do
      let n = head ns
      (val1,val2) <- listStoreGetValue store n
      when (val1 /= str) $ do
        listStoreSetValue store n (str,val2)
        applyChanged
  
  clTypes <- listStoreNew openclTypeNames
  let clColumn = makeColumnIdString 0
  customStoreSetColumn clTypes clColumn id
  
  col2 <- treeViewColumnNew
  renderer2 <- cellRendererComboNew
  treeViewColumnSetTitle col2 "Type"
  treeViewColumnPackStart col2 renderer2 False
  cellLayoutSetAttributes col2 renderer2 store $ \(_,t) -> [ 
    cellText := t,
    cellComboTextModel := (clTypes, clColumn), 
    cellComboHasEntry := False,
    cellTextEditable := True ]
  _ <- treeViewAppendColumn list col2
  _ <- renderer2 `on` edited $ \ns str -> do
    when (not . null $ ns) $ do
      let n = head ns
      (val1,val2) <- listStoreGetValue store n
      when (val2 /= str) $ do
        listStoreSetValue store n (val1,str)
        applyChanged
        
  col3 <- treeViewColumnNew
  renderer3 <- cellRendererToggleNew
  treeViewColumnSetTitle col3 "Delete?"
  treeViewColumnPackStart col3 renderer3 True
  _ <- treeViewAppendColumn list col3
  
  return ()
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
