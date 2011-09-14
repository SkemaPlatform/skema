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
import Control.Monad( when, forM_ )
import Control.Monad.Trans( liftIO )
import Control.Concurrent.MVar( 
  MVar, readMVar, modifyMVar_, modifyMVar, withMVar )
import qualified Data.IntMap as M( keys, insert, elems )
import Data.Maybe( isNothing, isJust, fromJust )
import System.Glib.Attributes( AttrOp(..) )
import Graphics.UI.Gtk( 
  on, renderWithDrawable, eventWindow, castToDrawable, drawableGetSize,
  DrawWindow, DrawingArea, widgetShowAll, mainQuit )
import Graphics.UI.Gtk.Abstract.Widget( 
  widgetAddEvents, exposeEvent, buttonPressEvent, buttonReleaseEvent, 
  motionNotifyEvent, widgetQueueDraw, EventMask(..), widgetDestroy )
import Graphics.UI.Gtk.Display.Statusbar( 
  castToStatusbar, statusbarGetContextId, statusbarPush )
import Graphics.UI.Gtk.Gdk.EventM( 
  tryEvent, eventButton, eventClick, eventCoordinates, MouseButton(..), 
  Click(..) )
import Graphics.UI.Gtk.General.StockItems( stockCancel, stockOpen, stockSave )
import Graphics.UI.Gtk.Glade( GladeXML, xmlGetWidget )
import Graphics.UI.Gtk.MenuComboToolbar.ToolButton( 
  castToToolButton, onToolButtonClicked )
import Graphics.UI.Gtk.MenuComboToolbar.MenuItem( 
  castToMenuItem, menuItemNewWithLabel, menuItemActivate )
import Graphics.UI.Gtk.MenuComboToolbar.MenuShell( menuShellAppend )
import Graphics.UI.Gtk.MenuComboToolbar.Menu(
  menuNew, menuPopup, menuSetTitle )
import Graphics.UI.Gtk.Misc.DrawingArea( castToDrawingArea )
import Graphics.UI.Gtk.ModelView( 
  ListStore, listStoreNew, listStoreClear, listStoreAppend, listStoreGetValue, 
  listStoreIterToIndex, TreeView, TreeIter, castToTreeView, 
  treeViewSetHeadersVisible, treeViewAppendColumn, treeViewSetModel, 
  cursorChanged, treeViewGetCursor, treeViewExpandAll, cellText, 
  cellRendererTextNew, cellLayoutSetAttributes, treeViewColumnNew, 
  treeViewColumnSetTitle, treeViewColumnPackStart, treeModelGetIter )
import Graphics.UI.Gtk.Selectors.FileFilter( 
  fileFilterNew, fileFilterAddPattern, fileFilterSetName )
import Graphics.UI.Gtk.Selectors.FileChooser( 
  FileChooserAction(..), fileChooserSetDoOverwriteConfirmation, 
  fileChooserGetFilename, fileChooserAddFilter )
import Graphics.UI.Gtk.Selectors.FileChooserDialog( 
  FileChooserDialog, fileChooserDialogNew )
import Graphics.UI.Gtk.Windows.Dialog( ResponseId(..), dialogRun )
import Graphics.UI.Gtk.Windows.Window( Window, castToWindow, toWindow )
import Skema.Editor.SkemaState( 
  SkemaState(..), XS(..), io, runXS, statePutSelectedPos, statePutSelectedPos2, 
  statePutSelectedElem, statePutSkemaDoc, stateGet, stateSelectElement, 
  stateInsertNewArrow, statePutSkemaDocFilename )
import Skema.Editor.Canvas( drawSkemaDoc, drawSelected )
import Skema.Editor.PFPreviewWindow( showPFPreviewWindow )
import Skema.Editor.TestProgramWindow( showTestProgramWindow )
import Skema.Editor.NodeCLWindow( showNodeCLWindow )
import Skema.SkemaDoc( 
  SDKernelID, SDNodeID, SkemaDoc(..), Node(..), Kernel(..), SelectedElement(..), 
  IOPoint(..), nodeTranslate, isIOPoint, arrowIOPointType, findInputArrow, 
  deleteArrow, minimalKernel, skemaDocGetKernelsAssocs, skemaDocInsertKernel, 
  skemaDocDeleteKernel, skemaDocUpdateKernel, skemaDocDeleteNode, 
  skemaDocGetNodesAssocs, skemaDocSetNodesAssocs, extractProgramFlow )
import Skema.Types( IOPointType(..) )
import Skema.Util( toJSONString, fromJSONString )
import Skema.Editor.Types( Pos2D(..) )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
prepareMainWindow :: GladeXML -> MVar SkemaState -> IO ()
prepareMainWindow xml state = do
  canvas <- xmlGetWidget xml castToDrawingArea "canvas"
  
  widgetAddEvents canvas [Button1MotionMask]

  skdoc <- fmap skemaDoc $ readMVar state

  ktree <- xmlGetWidget xml castToTreeView "kernels_tree"
  storeKernels <- listStoreNew (skemaDocGetKernelsAssocs skdoc)
  setupKernelsView ktree storeKernels
  treeViewExpandAll ktree

  statusbar <- xmlGetWidget xml castToStatusbar "statusbar"
  sbcontext <- statusbarGetContextId statusbar ""
  _ <- statusbarPush statusbar sbcontext "info"

  _ <- canvas `on` exposeEvent $ tryEvent $ do
    eventCanvas <- eventWindow
    liftIO . modifyMVar_ state $ \sks -> do
      (_,new_sks) <- runXS sks $ drawCanvas eventCanvas
      return new_sks

  _ <- canvas `on` buttonPressEvent $ tryEvent $ do
    LeftButton <- eventButton
    SingleClick <- eventClick
    (mx,my) <- eventCoordinates
    liftIO . modifyMVar_ state $ \sks -> do
      (_,new_sks) <- runXS sks $ selectElement mx my
      return new_sks

  _ <- canvas `on` buttonPressEvent $ tryEvent $ do
    RightButton <- eventButton
    SingleClick <- eventClick
    (mx,my) <- eventCoordinates
    iter <- liftIO $ getTreeIter ktree storeKernels
    when (isJust iter) . liftIO $ do 
      maybeNode <- modifyMVar state $ \sks -> do
        (i,_) <- listStoreGetValue storeKernels (listStoreIterToIndex . fromJust $ iter)
        (node,new_sks) <- runXS sks $ insertElement i mx my canvas
        return (new_sks,node)

      when (isJust maybeNode) . liftIO $ do
        let nodeIdx = fromJust maybeNode
        menu <- menuNew
        menuSetTitle menu "Node Menu"
        item1 <- menuItemNewWithLabel "Delete"
        menuShellAppend menu item1
        menuPopup menu Nothing
        
        _ <- item1 `on` menuItemActivate $ do
          modifyMVar_ state $ \sks -> do
            (_,new_sks) <- runXS sks $ deleteNode nodeIdx
            return new_sks
          widgetQueueDraw canvas

        widgetShowAll menu        

  _ <- canvas `on` buttonReleaseEvent $ tryEvent $ do
         LeftButton <- eventButton
         (mx,my) <- eventCoordinates
         liftIO . modifyMVar_ state $ \sks -> do
           (_,new_sks) <- runXS sks $ releaseElement mx my canvas
           widgetQueueDraw canvas
           return new_sks
         
  _ <- canvas `on` motionNotifyEvent $ tryEvent $ do
         (mx,my) <- eventCoordinates
         liftIO . modifyMVar_ state $ \sks -> do
           (_,new_sks) <- runXS sks $ moveTo mx my canvas
           return new_sks
  
  _ <- ktree `on` cursorChanged $ do
    iter <- getTreeIter ktree storeKernels
    case iter of
      Nothing -> modifyMVar_ state $ \sks -> return $ sks { selectedKernel = Nothing }
      Just val -> modifyMVar_ state $ \sks -> do
        (i,_) <- listStoreGetValue storeKernels (listStoreIterToIndex val)
        return $ sks { selectedKernel = Just i }
    widgetQueueDraw canvas
         
  _ <- ktree `on` buttonPressEvent $ tryEvent $ do
    LeftButton <- eventButton
    DoubleClick <- eventClick
    liftIO $ editKernel canvas state ktree storeKernels
    
  btn_pfv <- xmlGetWidget xml castToToolButton "mtb_pf_view"
  _ <- onToolButtonClicked btn_pfv $ showPFPreviewWindow state
  
  btn_pft <- xmlGetWidget xml castToToolButton "mtb_pf_test"
  _ <- onToolButtonClicked btn_pft $ do
    readMVar state >>= showTestProgramWindow . extractProgramFlow . skemaDoc
  
  btn_new_kernel <- xmlGetWidget xml castToToolButton "ktb_new"
  _ <- onToolButtonClicked btn_new_kernel $ do 
    modifyMVar_ state $ \sks -> do
      (_,new_sks) <- runXS sks newKernel
      clearKernelList storeKernels new_sks
    widgetQueueDraw canvas    
  
  btn_edit_kernel <- xmlGetWidget xml castToToolButton "ktb_edit"
  _ <- onToolButtonClicked btn_edit_kernel 
       $ editKernel canvas state ktree storeKernels
  
  btn_del_kernel <- xmlGetWidget xml castToToolButton "ktb_delete"
  _ <- onToolButtonClicked btn_del_kernel $ do
    iter <- getTreeIter ktree storeKernels
    when (isJust iter) $ do 
      modifyMVar_ state $ \sks -> do
        (i,_) <- listStoreGetValue storeKernels (listStoreIterToIndex . fromJust $ iter)
        (_,new_sks) <- runXS sks $ deleteKernel i
        clearKernelList storeKernels new_sks
      widgetQueueDraw canvas
    
  window <- xmlGetWidget xml castToWindow "main"
  
  -- Menu Items
  mi_open <- xmlGetWidget xml castToMenuItem "mi_open"
  _ <- mi_open `on` menuItemActivate $ do
    filename <- selectOpenFilename window
    when (isJust filename) $ do
      modifyMVar_ state $ \sks -> do
        (ok, new_sks) <- runXS sks (openSkemaDoc $ fromJust filename) 
        when ok (clearKernelList storeKernels new_sks >> return ())
        return new_sks
      widgetQueueDraw canvas
      
  mi_save <- xmlGetWidget xml castToMenuItem "mi_save"
  _ <- mi_save `on` menuItemActivate $ do
    oldname <- withMVar state $ \sks -> runXS sks (stateGet skemaDocFile) 
                                         >>= return . fst
    filename <- if isJust oldname
                then return oldname
                else selectSaveFilename window

    when (isJust filename) $ do
      modifyMVar_ state 
        $ \sks -> runXS sks (saveSkemaDoc $ fromJust filename) 
                  >>= return . snd
  
  mi_save_as <- xmlGetWidget xml castToMenuItem "mi_save_as"
  _ <- mi_save_as `on` menuItemActivate $ do
    filename <- selectSaveFilename window

    when (isJust filename) $ do
      modifyMVar_ state 
        $ \sks -> runXS sks (saveSkemaDoc $ fromJust filename) 
                  >>= return . snd
  
  mi_quit <- xmlGetWidget xml castToMenuItem "mi_quit"
  _ <- mi_quit `on` menuItemActivate $ do
    mainQuit
    
  return ()
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
getTreeIter :: TreeView -> ListStore a -> IO (Maybe TreeIter)
getTreeIter tree store = (fmap fst $ treeViewGetCursor tree)
    >>= treeModelGetIter store
\end{code}

\begin{code}
editKernel :: DrawingArea -> MVar SkemaState -> TreeView -> ListStore (SDKernelID, Kernel) -> IO ()
editKernel canvas state tree kernels = do
  iter <- getTreeIter tree kernels
  when (isJust iter) $ do
    (i,k) <- listStoreGetValue kernels (listStoreIterToIndex . fromJust $ iter)
    usedNames <- withMVar state 
                 $ return . filter (/= name k) . map name . M.elems . library . skemaDoc
    newk <- showNodeCLWindow k usedNames
    when (newk /= k) $ do 
      modifyMVar_ state $ \sks -> do
        (_,new_sks) <- runXS sks $ updateKernel i newk
        clearKernelList kernels new_sks
      widgetQueueDraw canvas    
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
setupKernelsView :: TreeView -> ListStore (SDKernelID,Kernel) -> IO ()
setupKernelsView view model = do
  treeViewSetModel view model
  treeViewSetHeadersVisible view True

  col1 <- treeViewColumnNew
  renderer1 <- cellRendererTextNew
  treeViewColumnSetTitle col1 "Project Kernels"
  treeViewColumnPackStart col1 renderer1 True
  cellLayoutSetAttributes col1 renderer1 model $ \(_,r) -> [ cellText := name r ]
  _ <- treeViewAppendColumn view col1
  
  col2 <- treeViewColumnNew
  renderer2 <- cellRendererTextNew
  treeViewColumnSetTitle col2 "In"
  treeViewColumnPackStart col2 renderer2 True
  cellLayoutSetAttributes col2 renderer2 model $ \(_,r) -> [ 
    cellText := show . length . filter ((==InputPoint) . iopType) . M.elems $ iopoints r ]
  _ <- treeViewAppendColumn view col2

  col3 <- treeViewColumnNew
  renderer3 <- cellRendererTextNew
  treeViewColumnSetTitle col3 "Out"
  treeViewColumnPackStart col3 renderer3 True
  cellLayoutSetAttributes col3 renderer3 model $ \(_,r) -> [ 
    cellText := show . length . filter ((==OutputPoint) . iopType) . M.elems $ iopoints r ]
  _ <- treeViewAppendColumn view col3

  return ()
\end{code}

\begin{code}
clearKernelList :: ListStore (SDKernelID,Kernel) -> SkemaState -> IO SkemaState
clearKernelList list sks = do
  listStoreClear list
  mapM_ (listStoreAppend list) (skemaDocGetKernelsAssocs $ skemaDoc sks)
  return sks{ selectedKernel = Nothing }
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
selectElement :: Double -> Double -> XS ()
selectElement mx my = do
  selElement <- stateSelectElement (Pos2D (mx,my))
  statePutSelectedElem selElement
  statePutSelectedPos (Pos2D (mx, my))
  when (maybe False isIOPoint selElement) $ do
    stDoc <- stateGet skemaDoc
    let selNode = seIOPNode.fromJust $ selElement
        selPoint = seIOPPoint.fromJust $ selElement
        pointType = arrowIOPointType stDoc selNode selPoint
    when (maybe False (==InputPoint) pointType) $ do
      let marrow = findInputArrow stDoc selNode selPoint
      when (isJust marrow) (statePutSkemaDoc (deleteArrow stDoc (fromJust marrow)))
\end{code}

\begin{code}
insertElement :: SDKernelID -> Double -> Double -> DrawingArea 
                 -> XS (Maybe SDNodeID)
insertElement idx mx my canvas = do
  selElement <- stateSelectElement (Pos2D (mx,my))
  if isNothing selElement
    then do
      insertNewNode idx mx my
      io $ widgetQueueDraw canvas
      return Nothing
    else showElementMenu (fromJust selElement)
\end{code}

\begin{code}
insertNewNode :: SDKernelID -> Double -> Double -> XS ()
insertNewNode idx x y = do
  oldDoc <- stateGet skemaDoc
  let keys = M.keys.nodes $ oldDoc
      last_i = if null keys then 0 else maximum keys + 1
      new_node = NodeKernel (Pos2D (x,y)) idx
  statePutSkemaDoc $ oldDoc { 
                         nodes = M.insert last_i new_node $ nodes oldDoc}
\end{code}

\begin{code}
showElementMenu :: SelectedElement -> XS (Maybe SDNodeID)
showElementMenu (SeNODE k) = return $ Just k
showElementMenu _ = return Nothing
\end{code}

\begin{code}
moveTo :: Double -> Double -> DrawingArea -> XS ()
moveTo mx my canvas = do
  stElem <- stateGet selectedElem
  when (isJust stElem) $ do
    moveSelectedElement (Pos2D (mx,my)) (fromJust stElem)
    io $ widgetQueueDraw canvas
\end{code}

\begin{code}
moveSelectedElement :: Pos2D -> SelectedElement -> XS ()
moveSelectedElement mpos (SeNODE k) = do
  oldDoc <- stateGet skemaDoc
  ori <- stateGet selectedPos
  let diff = mpos - ori
      applyf (nk,n) = if nk == k then (nk,nodeTranslate diff n) else (nk,n)
      new_nodes = map applyf . skemaDocGetNodesAssocs $ oldDoc

  statePutSkemaDoc $ skemaDocSetNodesAssocs oldDoc new_nodes
  statePutSelectedPos mpos
moveSelectedElement mpos (SeIOP _ _) = statePutSelectedPos2 mpos
\end{code}

\begin{code}
releaseElement :: Double -> Double -> DrawingArea -> XS ()
releaseElement mx my canvas = do
  stElem <- stateGet selectedElem
  when (isJust stElem) $ do
    releaseSelectedElement mx my (fromJust stElem)
    io $ widgetQueueDraw canvas
    statePutSelectedElem Nothing
\end{code}

\begin{code}
releaseSelectedElement :: Double -> Double -> SelectedElement -> XS ()
releaseSelectedElement mx my (SeIOP ki ji) = do
  endElem <- stateSelectElement (Pos2D (mx,my))
  when (maybe False isIOPoint endElem) $ do
    let (SeIOP kf jf) = (fromJust endElem)
    stateInsertNewArrow ki ji kf jf
releaseSelectedElement _ _ _ = return ()
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
drawCanvas :: DrawWindow -> XS ()
drawCanvas canvas = do
  let drawable = castToDrawable canvas
  stDoc <- stateGet skemaDoc
  selKernel <- stateGet selectedKernel
  (w,h) <- io $ drawableGetSize drawable
  io . renderWithDrawable canvas $ drawSkemaDoc
                                  (fromIntegral w) (fromIntegral h) 
                                  stDoc selKernel
     
  stElem <- stateGet selectedElem
  (Pos2D (ox,oy)) <- stateGet selectedPos
  (Pos2D (mx,my)) <- stateGet selectedPos2
  io . renderWithDrawable canvas $ drawSelected
                                  (fromIntegral w) (fromIntegral h)
                                  stElem
                                  ox oy mx my
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
newKernel :: XS ()
newKernel = do 
  oldDoc <- stateGet skemaDoc
  let krnl = minimalKernel $ library oldDoc
      newDoc = skemaDocInsertKernel oldDoc krnl
  statePutSkemaDoc newDoc
\end{code}

\begin{code}
deleteKernel :: SDKernelID -> XS ()
deleteKernel idx = do
  oldDoc <- stateGet skemaDoc
  statePutSkemaDoc $ skemaDocDeleteKernel oldDoc idx
\end{code}

\begin{code}
updateKernel :: SDKernelID -> Kernel -> XS ()
updateKernel idx krn = do
  oldDoc <- stateGet skemaDoc
  statePutSkemaDoc $ skemaDocUpdateKernel oldDoc idx krn
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
deleteNode :: SDNodeID -> XS ()
deleteNode idx = do
  oldDoc <- stateGet skemaDoc
  statePutSkemaDoc $ skemaDocDeleteNode oldDoc idx
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
setupFilters :: FileChooserDialog -> IO ()
setupFilters dialog = forM_ filters $ \(n,f) -> do
  fileFilter <- fileFilterNew
  fileFilterAddPattern fileFilter f
  fileFilterSetName fileFilter $ concat [n," (",f,")"]
  fileChooserAddFilter dialog fileFilter
    where
      filters = [("Skema Project","*.skema"), ("All","*.*")]

\end{code}

\begin{code}
selectOpenFilename :: Window -> IO (Maybe FilePath)
selectOpenFilename window = do
  chooser <- fileChooserDialogNew Nothing (Just . toWindow $ window)
             FileChooserActionOpen
             [(stockCancel,ResponseNo),(stockOpen,ResponseYes)]
  setupFilters chooser
  resp <- dialogRun chooser
  
  fileName <- fileChooserGetFilename chooser
    
  widgetDestroy chooser
    
  case resp of
    ResponseYes -> return fileName
    _ -> return Nothing
\end{code}

\begin{code}
selectSaveFilename :: Window -> IO (Maybe FilePath)
selectSaveFilename window = do
  chooser <- fileChooserDialogNew Nothing (Just . toWindow $ window)
             FileChooserActionSave
             [(stockCancel,ResponseNo),(stockSave,ResponseYes)]
  setupFilters chooser
  fileChooserSetDoOverwriteConfirmation chooser True
  resp <- dialogRun chooser
  
  fileName <- fileChooserGetFilename chooser
    
  widgetDestroy chooser
    
  case resp of
    ResponseYes -> return fileName
    _ -> return Nothing
\end{code}

\begin{code}
openSkemaDoc :: FilePath -> XS Bool
openSkemaDoc filename = do
  filedat <- io $ catch 
    (fmap Just $ readFile filename)
    (\_ -> return Nothing)
    
  case filedat of
    Nothing -> return False
    Just json -> case fromJSONString json of
      Nothing -> return False
      Just doc -> do
        statePutSkemaDoc doc 
        statePutSkemaDocFilename $ Just filename
        return True
\end{code}

\begin{code}
saveSkemaDoc :: FilePath -> XS ()
saveSkemaDoc filename = do
  jsonData <- fmap toJSONString $ stateGet skemaDoc
  io $ writeFile filename jsonData
  statePutSkemaDocFilename $ Just filename  
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
