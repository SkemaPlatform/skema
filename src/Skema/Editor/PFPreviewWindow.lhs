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
import Control.Monad( unless, forM_ )
import Control.Concurrent.MVar( MVar, readMVar )
import Graphics.UI.Gtk( on, widgetDestroy, toWindow, stockCancel, stockSave )
import Graphics.UI.Gtk.Glade( xmlNew, xmlGetWidget )
import Graphics.UI.Gtk.Windows.Dialog( 
  ResponseId(..), castToDialog, dialogRun, dialogResponse )
import Graphics.UI.Gtk.Multiline.TextView( castToTextView, textViewGetBuffer )
import Graphics.UI.Gtk.Multiline.TextBuffer( textBufferSetText )
import Graphics.UI.Gtk.Buttons.Button( castToButton, buttonActivated )
import Graphics.UI.Gtk.Selectors.FileFilter( 
  fileFilterNew, fileFilterAddPattern, fileFilterSetName )
import Graphics.UI.Gtk.Selectors.FileChooser( 
  FileChooserAction(..), fileChooserSetDoOverwriteConfirmation, 
  fileChooserGetFilename, fileChooserAddFilter )
import Graphics.UI.Gtk.Selectors.FileChooserDialog( fileChooserDialogNew )
import Paths_skema( getDataFileName )
import Skema.Editor.SkemaState( SkemaState(..) )
import Skema.SkemaDoc( extractProgramFlow )
import Skema.ProgramFlow( generateJSONString )
import Skema.JSON( prettyJSON )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
showPFPreviewWindow :: MVar SkemaState -> IO ()
showPFPreviewWindow state = do
  glade <- getDataFileName "pf_preview.glade"
  Just xml <- xmlNew glade
  window <- xmlGetWidget xml castToDialog "main"
 
  sks <- liftIO $ readMVar state
  let json = prettyJSON . generateJSONString . extractProgramFlow . skemaDoc $ sks
   
  tv <- xmlGetWidget xml castToTextView "text_view"
  tbuffer <- textViewGetBuffer tv
  textBufferSetText tbuffer json

  btnAccept <- xmlGetWidget xml castToButton "button_accept"
  _ <- btnAccept `on` buttonActivated $ dialogResponse window ResponseNone
  
  btnSave <- xmlGetWidget xml castToButton "button_save"
  _ <- btnSave `on` buttonActivated $ do
    dialogResponse window ResponseApply
    chooser <- fileChooserDialogNew Nothing (Just . toWindow $ window)
               FileChooserActionSave 
               [(stockCancel,ResponseNo),(stockSave,ResponseYes)]
    forM_ [("Skema Program Flow","*.skm"), ("All","*.*")] 
               $ \(n,f) -> do
      fileFilter <- fileFilterNew
      fileFilterAddPattern fileFilter f
      fileFilterSetName fileFilter $ concat [n," (",f,")"]
      fileChooserAddFilter chooser fileFilter
    
    fileChooserSetDoOverwriteConfirmation chooser True
    _ <- dialogRun chooser
    
    fileName <- fileChooserGetFilename chooser
    print fileName
    
    widgetDestroy chooser
    
  loop window

  widgetDestroy window

  return ()
  
    where
      loop window = do
        resp <- dialogRun window 
        unless (resp == ResponseNone) $ loop window
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
