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
import qualified Data.IntMap as MI( fromList, elems )
import Control.Monad( when, unless )
import Control.Arrow( (&&&) )
import System.Glib.Attributes( AttrOp(..) )
import Graphics.UI.Gtk( 
  on, get, containerAdd, widgetShowAll, widgetSetSizeRequest, 
  scrolledWindowNew, widgetDestroy, widgetGetState, widgetModifyBase, Color(..), 
  windowSetDefault, bufferChanged, toWindow )
import Graphics.UI.Gtk.Abstract.Box( Packing(..), boxPackStart )
import Graphics.UI.Gtk.Abstract.Widget( widgetSetSensitive )
import Graphics.UI.Gtk.General.StockItems( stockApply, stockCancel )
import Graphics.UI.Gtk.Buttons.ToggleButton( 
  toggled, toggleButtonGetActive, toggleButtonSetActive )
import Graphics.UI.Gtk.Buttons.CheckButton( checkButtonNewWithLabel )
import Graphics.UI.Gtk.Windows.Dialog( 
  Dialog, ResponseId(..), dialogNew, dialogRun, dialogGetUpper, 
  dialogAddButton, dialogSetResponseSensitive )
import Graphics.UI.Gtk.Windows.MessageDialog( 
  messageDialogNew, DialogFlags(..), MessageType(..), ButtonsType(..) )
import Graphics.UI.Gtk.Display.Label( labelNew )
import Graphics.UI.Gtk.Entry.Editable( editableChanged )
import Graphics.UI.Gtk.Entry.Entry( Entry, entryNew, entryText, entrySetText )
import Graphics.UI.Gtk.Entry.SpinButton( 
  spinButtonNewWithRange, spinButtonSetValue, spinButtonGetValueAsInt )
import Graphics.UI.Gtk.Layout.HBox( hBoxNew )
import Graphics.UI.Gtk.Layout.VBox( VBox, vBoxNew )
import Graphics.UI.Gtk.Multiline.TextBuffer( textBufferSetText, textBufferText )
import Graphics.UI.Gtk.ModelView( 
  -- tree functions
  TreeView, treeViewNew, treeViewSetModel, treeViewSetHeadersVisible, 
  treeViewColumnNew, treeViewColumnSetTitle, treeViewColumnPackStart, 
  treeViewAppendColumn,
  -- list functions
  ListStore, listStoreNew, listStoreGetValue, listStoreSetValue, 
  listStoreGetSize, listStoreRemove, listStoreAppend, listStoreToList,
  -- cell functions
  cellRendererTextNew, cellLayoutSetAttributes, cellText, cellTextEditable, 
  cellRendererComboNew, cellComboTextModel, cellComboHasEntry, 
  makeColumnIdString, cellRendererToggleNew, cellToggled,
  -- additional
  customStoreSetColumn, edited, editingStarted, stringToTreePath )
import Graphics.UI.Gtk.SourceView( 
  SourceBuffer, sourceLanguageManagerGetDefault, 
  sourceLanguageManagerGetSearchPath, sourceLanguageManagerSetSearchPath, 
  sourceLanguageManagerGetLanguage, sourceStyleSchemeManagerGetDefault, 
  sourceStyleSchemeManagerGetScheme, sourceBufferNew, sourceBufferSetLanguage, 
  sourceBufferSetStyleScheme, sourceBufferSetHighlightSyntax, 
  sourceViewNewWithBuffer, sourceBufferBeginNotUndoableAction, 
  sourceBufferEndNotUndoableAction )
import Skema.SkemaDoc( Kernel(..), IOPoint(..), isInputPoint, isOutputPoint )
import Skema.Types( openclTypeNames, IOPointType(..) )
import Skema.Util( duplicates )
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
  
  internal <- dialogGetUpper window
  
  -- create widgets
  eName <- createKernelName internal (name krn)
  (storeInputs, storeOutputs) <- createParameters window internal krn
  
  hbox <- hBoxNew True 0
  tick <- checkButtonNewWithLabel "Fixed Work Items:"
  boxPackStart hbox tick PackNatural 0  
  nWorkItems <- spinButtonNewWithRange 1 1000 1
  case workItems krn of
    Nothing -> do
      widgetSetSensitive nWorkItems False
      toggleButtonSetActive tick False
    Just n -> do
      widgetSetSensitive nWorkItems True
      spinButtonSetValue nWorkItems (fromIntegral n)
      toggleButtonSetActive tick True
      
  boxPackStart hbox nWorkItems PackNatural 0  
  boxPackStart internal hbox PackNatural 0

  sbuff <- createSourceBuffer internal (body krn)
  
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
    eNameState <- widgetGetState eName 
    if validName newName usedNames 
      then widgetModifyBase eName eNameState goodColor
      else widgetModifyBase eName eNameState badColor
           
  _ <- sbuff `on` bufferChanged $
    dialogSetResponseSensitive window ResponseAccept True
    
  _ <- tick `on` toggled $ do
    dialogSetResponseSensitive window ResponseAccept True
    active <- toggleButtonGetActive tick
    widgetSetSensitive nWorkItems active
    
  _ <- nWorkItems `on` editableChanged $ do
    dialogSetResponseSensitive window ResponseAccept True
  
  -- get the response and return
  resp <- dialogRun window   
  newkrn <- case resp of
    ResponseAccept -> do
      newBody <- get sbuff textBufferText
      newName <- get eName entryText      
      
      unless (validName newName usedNames) $
        errorMsg (Just . toWindow $ window) 
        "Invalid Kernel Name. Using old one."
        
      ins <- fmap init $ listStoreToList storeInputs
      outs <- fmap init $ listStoreToList storeOutputs      
      let paramNames = map fst $ ins ++ outs
          dups = duplicates paramNames
          validParams = null dups && all (`validName` []) paramNames
          inPoints = map (\(n,t) -> IOPoint n (read t) InputPoint) ins
          outPoints = map (\(n,t) -> IOPoint n (read t) OutputPoint) outs
          newPoints = MI.fromList . zip [0..] $ inPoints ++ outPoints
      
      activeWI <- toggleButtonGetActive tick
      newWorkItems <- spinButtonGetValueAsInt nWorkItems
        
      unless validParams $ 
        errorMsg (Just . toWindow $ window) 
        "Invalid Parameters Names. Using old ones."
        
      return krn { body = newBody, 
                   name = if validName newName usedNames
                          then newName 
                          else name krn,
                   iopoints = if validParams
                              then newPoints
                              else iopoints krn,
                   workItems = if activeWI
                               then (Just newWorkItems)
                               else Nothing
                 }
  
    _ -> return krn
    
  widgetDestroy window
  return newkrn
    where 
      errorMsg parent msg = do
        msgDlg <- messageDialogNew parent 
                  [DialogModal] MessageError ButtonsOk msg
        widgetShowAll msgDlg
        _ <- dialogRun msgDlg
        widgetDestroy msgDlg
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
createKernelName :: VBox -> String -> IO Entry
createKernelName box old = do
  hbox0 <- hBoxNew True 0
  lblName <- labelNew $ Just "Name:"
  boxPackStart hbox0 lblName PackNatural 0
  eName <- entryNew 
  entrySetText eName old
  eNameState <- widgetGetState eName 
  widgetModifyBase eName eNameState goodColor
  boxPackStart hbox0 eName PackGrow 0
  boxPackStart box hbox0 PackNatural 0
  return eName
\end{code}

\begin{code}
createParameters :: Dialog -> VBox -> Kernel 
                    -> IO (ListStore (String,String), ListStore (String,String))
createParameters window box krn = do
  hbox1 <- hBoxNew True 0  
  boxPackStart box hbox1 PackNatural 0
  
  -- input widgets
  vboxInput <- vBoxNew False 0
  boxPackStart hbox1 vboxInput PackNatural 0
  lbl0 <- labelNew $ Just "Input Parameters"
  boxPackStart vboxInput lbl0 PackNatural 0
  
  let krnIns = map (iopName &&& show . iopDataType) 
               . filter isInputPoint . MI.elems $ iopoints krn
  inputList <- treeViewNew
  storeInputs <- listStoreNew krnIns
  boxPackStart vboxInput inputList PackNatural 0
  
  setupParameterList "inp" inputList storeInputs $ 
    dialogSetResponseSensitive window ResponseAccept True
  
  -- output widgets
  vboxOutput <- vBoxNew False 0
  boxPackStart hbox1 vboxOutput PackNatural 0
  lbl1 <- labelNew $ Just "Output Parameters"
  boxPackStart vboxOutput lbl1 PackNatural 0
  
  let krnOuts = map (iopName &&& show . iopDataType) 
                . filter isOutputPoint . MI.elems $ iopoints krn
  outputList <- treeViewNew
  storeOutputs <- listStoreNew krnOuts
  boxPackStart vboxOutput outputList PackNatural 0
  
  setupParameterList "outp" outputList storeOutputs $ 
    dialogSetResponseSensitive window ResponseAccept True
    
  return (storeInputs, storeOutputs)
\end{code}

\begin{code}
createSourceBuffer :: VBox -> String -> IO SourceBuffer
createSourceBuffer box old = do
  -- configure language
  datadir <- getDataDir
  
  slman <- sourceLanguageManagerGetDefault
  paths <- sourceLanguageManagerGetSearchPath slman
  sourceLanguageManagerSetSearchPath slman (Just $ paths ++ [datadir])
  slang <- sourceLanguageManagerGetLanguage slman "opencl"
  when (isNothing slang) $ print "error: no language file"
  
  slsty <- sourceStyleSchemeManagerGetDefault
  ssty <- sourceStyleSchemeManagerGetScheme slsty "classic"
  
  -- create Source Input Widget
  lbl2 <- labelNew $ Just "Kernel Body"
  boxPackStart box lbl2 PackNatural 0
  sbuff <- sourceBufferNew Nothing
  sourceBufferSetLanguage sbuff slang
  sourceBufferSetStyleScheme sbuff (Just ssty)
  sourceBufferSetHighlightSyntax sbuff True
  sourceBufferBeginNotUndoableAction sbuff
  textBufferSetText sbuff old
  sourceBufferEndNotUndoableAction sbuff
  sourceview <- sourceViewNewWithBuffer sbuff
  sw <- scrolledWindowNew Nothing Nothing
  containerAdd sw sourceview
  boxPackStart box sw PackGrow 0
  return sbuff
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
setupParameterList :: String -> TreeView -> ListStore (String,String) -> IO () 
                      -> IO ()
setupParameterList newpre list store applyChanged = do
  treeViewSetModel list store
  treeViewSetHeadersVisible list True
  
  _ <- listStoreAppend store ("*new*","float")
  
  col1 <- treeViewColumnNew
  renderer1 <- cellRendererTextNew
  treeViewColumnSetTitle col1 "Name"
  treeViewColumnPackStart col1 renderer1 True
  cellLayoutSetAttributes col1 renderer1 store $ \(r,_) -> [ 
    cellText := r, cellTextEditable := True ]
  _ <- treeViewAppendColumn list col1
  _ <- renderer1 `on` edited $ \path str -> unless (null path) $ do
    let n = head path
    (val1,val2) <- listStoreGetValue store n
    when (val1 /= str) $ do
      listStoreSetValue store n (str,val2)
      applyChanged
  _ <- renderer1 `on` editingStarted $ newParameter newpre
       
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
  _ <- renderer2 `on` edited $ \ns str -> unless (null ns) $ do
    let n = head ns
    (val1,val2) <- listStoreGetValue store n
    when (val2 /= str) $ do
      listStoreSetValue store n (val1,str)
      applyChanged
  _ <- renderer2 `on` editingStarted $ newParameter newpre
        
  col3 <- treeViewColumnNew
  renderer3 <- cellRendererToggleNew
  treeViewColumnSetTitle col3 "Delete?"
  treeViewColumnPackStart col3 renderer3 True
  _ <- treeViewAppendColumn list col3
  _ <- renderer3 `on` cellToggled $ \path -> do
    npars <- listStoreGetSize store
    when (npars > 2) $ do
      let treepath = stringToTreePath path
      unless (null treepath) $ do
        let idx = head treepath
        when (idx /= (npars - 1)) $ do
          listStoreRemove store idx
          applyChanged
  
  return ()
    where
      newParameter prefix _ path = unless (null path) $ do
        npars <- listStoreGetSize store
        let idx = head path
        when (idx == (npars - 1)) $ do
          listStoreSetValue store idx (prefix,"float")
          _ <- listStoreAppend store ("*new*","float")
          applyChanged
\end{code}

\begin{code}
validName :: String -> [String] -> Bool
validName cad xs = checkLength && checkDigits 
                   && (checkInit.head) cad && cad `notElem` xs
  where
    checkLength = length cad > 0
    checkDigits = all checkChar cad
    checkChar c = isAlphaNum c || (c == '_')
    checkInit c = isAlpha c || (c == '_')
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
