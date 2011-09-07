-- -----------------------------------------------------------------------------
-- This file is part of Skema.

-- Skema is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.

-- Skema is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with Skema.  If not, see <http://www.gnu.org/licenses/>.
-- -----------------------------------------------------------------------------
module Skema.Editor.TestProgramWindow( showTestProgramWindow ) where

-- -----------------------------------------------------------------------------
import Control.Monad( forM_ )
import Data.Int( Int8, Int16, Int32, Int64 )
import Data.Word( Word8, Word16, Word32, Word64 )
import Graphics.UI.Gtk( 
  widgetShowAll, widgetSetSizeRequest, widgetDestroy, widgetSetSensitivity )
import Graphics.UI.Gtk.Abstract.Box( BoxClass, Packing(..), boxPackStart )
import Graphics.UI.Gtk.Display.Label( labelNew )
import Graphics.UI.Gtk.Layout.HBox( hBoxNew )
import Graphics.UI.Gtk.Layout.VBox( vBoxNew )
import Graphics.UI.Gtk.Windows.Dialog( 
  ResponseId(..), dialogNew, dialogRun, dialogGetUpper, dialogAddButton )
import Graphics.UI.Gtk.Windows.Window( windowSetDefault )
import Graphics.UI.Gtk.Scrolling.ScrolledWindow( 
  scrolledWindowNew, scrolledWindowAddWithViewport )
import Graphics.UI.Gtk.Buttons.Button( buttonNewFromStock )
import Graphics.UI.Gtk.General.StockItems( stockExecute, stockOk )
import Graphics.UI.Gtk.Entry.SpinButton( spinButtonNew )
import Graphics.UI.Gtk.Misc.Adjustment( adjustmentNew )
import Graphics.UI.Gtk.Ornaments.HSeparator( hSeparatorNew )
import Skema.Types( IOPointDataType(..), IOPointType(..) )
import Skema.ProgramFlow( 
  ProgramFlow, IOPoint(..), unasignedOutputPoints, unasignedInputPoints )
import Skema.SIDMap( toInt )

-- -----------------------------------------------------------------------------
showTestProgramWindow :: ProgramFlow -> IO ()
showTestProgramWindow pf = do
  -- TODO: check program flow ins/outs
  
  window <- dialogNew
  widgetSetSizeRequest window 640 480
  
  internal <- dialogGetUpper window
  -- head
  lblName <- labelNew $ Just "Test"
  boxPackStart internal lblName PackNatural 0  
  
  -- body
  hbox0 <- hBoxNew True 0
  
  sw_in <- scrolledWindowNew Nothing Nothing
  vbox_in <- vBoxNew True 0
  forM_ (unasignedInputPoints pf) (createIOControl vbox_in)
  scrolledWindowAddWithViewport sw_in vbox_in  
  boxPackStart hbox0 sw_in PackGrow 0
  
  vbox0 <- vBoxNew True 0
  sep1 <- hSeparatorNew
  boxPackStart vbox0 sep1 PackNatural 0
  btn_run <- buttonNewFromStock stockExecute
  boxPackStart vbox0 btn_run PackNatural 0
  sep2 <- hSeparatorNew
  boxPackStart vbox0 sep2 PackNatural 0
  boxPackStart hbox0 vbox0 PackRepel 0
  
  sw_out <- scrolledWindowNew Nothing Nothing
  vbox_out <- vBoxNew True 0
  forM_ (unasignedOutputPoints pf) (createIOControl vbox_out)
  scrolledWindowAddWithViewport sw_out vbox_out  
  boxPackStart hbox0 sw_out PackGrow 0 
  
  boxPackStart internal hbox0 PackGrow 0  
  
  -- buttons
  okButton <- dialogAddButton window stockOk ResponseAccept
  windowSetDefault window $ Just okButton
  
  widgetShowAll window 
  
  _ <- dialogRun window   
  
  widgetDestroy window
  
  return ()

-- -----------------------------------------------------------------------------
createIOControl :: BoxClass b => b -> IOPoint -> IO ()
createIOControl box pt = do
  hbox0 <- hBoxNew True 0
  lblName <- labelNew . Just $ 
             (show . toInt $ iopNode pt) 
             ++ ": " ++ (iopPoint pt)
             ++ " [" ++ (show $ iopDataType pt) ++ "] "
  boxPackStart hbox0 lblName PackNatural 0
  createEntry (iopType pt == InputPoint) hbox0 $ iopDataType pt
  boxPackStart box hbox0 PackNatural 0
  return ()
  
-- -----------------------------------------------------------------------------
bounds :: Bounded a => (a,a)
bounds = (minBound, maxBound)

createEntry :: BoxClass b => Bool -> b -> IOPointDataType -> IO ()
createEntry act box IOchar = do
  adjust <- adjustmentNew 0 (fromIntegral ma) (fromIntegral mb) 1 1 0
  entry <- spinButtonNew adjust 1 0
  widgetSetSensitivity entry act
  boxPackStart box entry PackNatural 0
    where
      (ma,mb) = bounds :: (Int8, Int8)
createEntry act box IOuchar = do
  adjust <- adjustmentNew 0 (fromIntegral ma) (fromIntegral mb) 1 1 0
  entry <- spinButtonNew adjust 1 0
  widgetSetSensitivity entry act
  boxPackStart box entry PackNatural 0
    where
      (ma,mb) = bounds :: (Word8, Word8)
createEntry act box IOshort = do
  adjust <- adjustmentNew 0 (fromIntegral ma) (fromIntegral mb) 1 1 0
  entry <- spinButtonNew adjust 1 0
  widgetSetSensitivity entry act
  boxPackStart box entry PackNatural 0
    where
      (ma,mb) = bounds :: (Int16, Int16)
createEntry act box IOushort = do
  adjust <- adjustmentNew 0 (fromIntegral ma) (fromIntegral mb) 1 1 0
  entry <- spinButtonNew adjust 1 0
  widgetSetSensitivity entry act
  boxPackStart box entry PackNatural 0
    where
      (ma,mb) = bounds :: (Word16, Word16)
createEntry act box IOint = do
  adjust <- adjustmentNew 0 (fromIntegral ma) (fromIntegral mb) 1 1 0
  entry <- spinButtonNew adjust 1 0
  widgetSetSensitivity entry act
  boxPackStart box entry PackNatural 0
    where
      (ma,mb) = bounds :: (Int32, Int32)
createEntry act box IOuint = do
  adjust <- adjustmentNew 0 (fromIntegral ma) (fromIntegral mb) 1 1 0
  entry <- spinButtonNew adjust 1 0
  widgetSetSensitivity entry act
  boxPackStart box entry PackNatural 0
    where
      (ma,mb) = bounds :: (Word32, Word32)
createEntry act box IOlong = do
  adjust <- adjustmentNew 0 (fromIntegral ma) (fromIntegral mb) 1 1 0
  entry <- spinButtonNew adjust 1 0
  widgetSetSensitivity entry act
  boxPackStart box entry PackNatural 0
    where
      (ma,mb) = bounds :: (Int64, Int64)
createEntry act box IOulong = do
  adjust <- adjustmentNew 0 (fromIntegral ma) (fromIntegral mb) 1 1 0
  entry <- spinButtonNew adjust 1 0
  widgetSetSensitivity entry act
  boxPackStart box entry PackNatural 0
    where
      (ma,mb) = bounds :: (Word64, Word64)
createEntry act box IOfloat = do
  adjust <- adjustmentNew 0 (-1e50) 1e50 0.01 0.01 0
  entry <- spinButtonNew adjust 0.01 2
  widgetSetSensitivity entry act
  boxPackStart box entry PackNatural 0
createEntry act box IOchar2 = createNEntries act box IOchar 2
createEntry act box IOchar4 = createNEntries act box IOchar 4
createEntry act box IOchar8 = createNEntries act box IOchar 8
createEntry act box IOchar16 = createNEntries act box IOchar 16
createEntry act box IOuchar2 = createNEntries act box IOuchar 2
createEntry act box IOuchar4 = createNEntries act box IOuchar 4
createEntry act box IOuchar8 = createNEntries act box IOuchar 8
createEntry act box IOuchar16 = createNEntries act box IOuchar 16
createEntry act box IOshort2 = createNEntries act box IOshort 2
createEntry act box IOshort4 = createNEntries act box IOshort 4
createEntry act box IOshort8 = createNEntries act box IOshort 8
createEntry act box IOshort16 = createNEntries act box IOshort 16
createEntry act box IOushort2 = createNEntries act box IOushort 2
createEntry act box IOushort4 = createNEntries act box IOushort 4
createEntry act box IOushort8 = createNEntries act box IOushort 8
createEntry act box IOushort16 = createNEntries act box IOushort 16
createEntry act box IOint2 = createNEntries act box IOint 2
createEntry act box IOint4 = createNEntries act box IOint 4
createEntry act box IOint8 = createNEntries act box IOint 8
createEntry act box IOint16 = createNEntries act box IOint 16
createEntry act box IOuint2 = createNEntries act box IOuint 2
createEntry act box IOuint4 = createNEntries act box IOuint 4
createEntry act box IOuint8 = createNEntries act box IOuint 8
createEntry act box IOuint16 = createNEntries act box IOuint 16
createEntry act box IOlong2 = createNEntries act box IOlong 2
createEntry act box IOlong4 = createNEntries act box IOlong 4
createEntry act box IOlong8 = createNEntries act box IOlong 8
createEntry act box IOlong16 = createNEntries act box IOlong 16
createEntry act box IOulong2 = createNEntries act box IOulong 2
createEntry act box IOulong4 = createNEntries act box IOulong 4
createEntry act box IOulong8 = createNEntries act box IOulong 8
createEntry act box IOulong16 = createNEntries act box IOulong 16
createEntry act box IOfloat2 = createNEntries act box IOfloat 2
createEntry act box IOfloat4 = createNEntries act box IOfloat 4
createEntry act box IOfloat8 = createNEntries act box IOfloat 8
createEntry act box IOfloat16 = createNEntries act box IOfloat 16

createNEntries :: BoxClass b => Bool -> b -> IOPointDataType -> Int -> IO ()
createNEntries act box t n = do
  vbox <- vBoxNew True 0
  forM_ [0..n-1] $ \_ -> do
    createEntry act vbox t
  boxPackStart box vbox PackNatural 0

-- -----------------------------------------------------------------------------
