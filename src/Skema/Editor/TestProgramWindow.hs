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
import Control.Monad( forM_, forM, when, liftM )
import Control.Concurrent.MVar( MVar, newMVar, readMVar, modifyMVar_ )
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
import Graphics.UI.Gtk.Buttons.Button( buttonNewFromStock, buttonActivated )
import Graphics.UI.Gtk.General.StockItems( stockExecute, stockOk )
import Graphics.UI.Gtk.Entry.SpinButton( 
  SpinButton, spinButtonNew, spinButtonGetValue, afterInput )
import Graphics.UI.Gtk.Misc.Adjustment( adjustmentNew )
import Graphics.UI.Gtk.Ornaments.HSeparator( hSeparatorNew )
import System.Glib.Signals( on )
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
  invals <- forM (unasignedInputPoints pf)
            (liftM (map fst) . createIOControl vbox_in)
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
  outents <- forM (unasignedOutputPoints pf) 
             (liftM (map snd) . createIOControl vbox_out)
  scrolledWindowAddWithViewport sw_out vbox_out  
  boxPackStart hbox0 sw_out PackGrow 0 
  
  boxPackStart internal hbox0 PackGrow 0  
  
  -- buttons
  okButton <- dialogAddButton window stockOk ResponseAccept
  windowSetDefault window $ Just okButton
  
  _ <- btn_run `on` buttonActivated $ do
    forM_ invals $ \xs -> do
      vals <- mapM readMVar xs
      print vals

  widgetShowAll window 
  
  _ <- dialogRun window   
  
  widgetDestroy window
  
  return ()

-- -----------------------------------------------------------------------------
createIOControl :: BoxClass b => b -> IOPoint -> IO [(MVar Double,SpinButton)]
createIOControl box pt = do
  hbox0 <- hBoxNew True 0
  lblName <- labelNew . Just $ 
             (show . toInt $ iopNode pt) 
             ++ ": " ++ (iopPoint pt)
             ++ " [" ++ (show $ iopDataType pt) ++ "] "
  boxPackStart hbox0 lblName PackNatural 0
  ret <- createEntry hbox0 $ iopDataType pt
  
  when (iopType pt == OutputPoint) $
    forM_ ret $ \(_,entry) -> widgetSetSensitivity entry False
    
  boxPackStart box hbox0 PackNatural 0
  return ret
  
-- -----------------------------------------------------------------------------
bounds :: Bounded a => (a,a)
bounds = (minBound, maxBound)

createEntry :: BoxClass b => b -> IOPointDataType 
               -> IO [(MVar Double,SpinButton)]
createEntry box IOchar = do
  adjust <- adjustmentNew 0 (fromIntegral ma) (fromIntegral mb) 1 1 0
  entry <- spinButtonNew adjust 1 0
  boxPackStart box entry PackNatural 0
  dat <- newMVar 0
  _ <- afterInput entry $ do
    newval <- spinButtonGetValue entry
    modifyMVar_ dat (const $ return newval)
    return $ Just newval  
  return [(dat, entry)]
    where
      (ma,mb) = bounds :: (Int8, Int8)
createEntry box IOuchar = do
  adjust <- adjustmentNew 0 (fromIntegral ma) (fromIntegral mb) 1 1 0
  entry <- spinButtonNew adjust 1 0
  boxPackStart box entry PackNatural 0
  dat <- newMVar 0
  _ <- afterInput entry $ do
    newval <- spinButtonGetValue entry
    modifyMVar_ dat (const $ return newval)
    return $ Just newval  
  return [(dat, entry)]
    where
      (ma,mb) = bounds :: (Word8, Word8)
createEntry box IOshort = do
  adjust <- adjustmentNew 0 (fromIntegral ma) (fromIntegral mb) 1 1 0
  entry <- spinButtonNew adjust 1 0
  boxPackStart box entry PackNatural 0
  dat <- newMVar 0
  _ <- afterInput entry $ do
    newval <- spinButtonGetValue entry
    modifyMVar_ dat (const $ return newval)
    return $ Just newval  
  return [(dat, entry)]
    where
      (ma,mb) = bounds :: (Int16, Int16)
createEntry box IOushort = do
  adjust <- adjustmentNew 0 (fromIntegral ma) (fromIntegral mb) 1 1 0
  entry <- spinButtonNew adjust 1 0
  boxPackStart box entry PackNatural 0
  dat <- newMVar 0
  _ <- afterInput entry $ do
    newval <- spinButtonGetValue entry
    modifyMVar_ dat (const $ return newval)
    return $ Just newval  
  return [(dat, entry)]
    where
      (ma,mb) = bounds :: (Word16, Word16)
createEntry box IOint = do
  adjust <- adjustmentNew 0 (fromIntegral ma) (fromIntegral mb) 1 1 0
  entry <- spinButtonNew adjust 1 0
  boxPackStart box entry PackNatural 0
  dat <- newMVar 0
  _ <- afterInput entry $ do
    newval <- spinButtonGetValue entry
    modifyMVar_ dat (const $ return newval)
    return $ Just newval  
  return [(dat, entry)]
    where
      (ma,mb) = bounds :: (Int32, Int32)
createEntry box IOuint = do
  adjust <- adjustmentNew 0 (fromIntegral ma) (fromIntegral mb) 1 1 0
  entry <- spinButtonNew adjust 1 0
  boxPackStart box entry PackNatural 0
  dat <- newMVar 0
  return [(dat, entry)]
    where
      (ma,mb) = bounds :: (Word32, Word32)
createEntry box IOlong = do
  adjust <- adjustmentNew 0 (fromIntegral ma) (fromIntegral mb) 1 1 0
  entry <- spinButtonNew adjust 1 0
  boxPackStart box entry PackNatural 0
  dat <- newMVar 0
  _ <- afterInput entry $ do
    newval <- spinButtonGetValue entry
    modifyMVar_ dat (const $ return newval)
    return $ Just newval  
  return [(dat, entry)]
    where
      (ma,mb) = bounds :: (Int64, Int64)
createEntry box IOulong = do
  adjust <- adjustmentNew 0 (fromIntegral ma) (fromIntegral mb) 1 1 0
  entry <- spinButtonNew adjust 1 0
  boxPackStart box entry PackNatural 0
  dat <- newMVar 0
  _ <- afterInput entry $ do
    newval <- spinButtonGetValue entry
    modifyMVar_ dat (const $ return newval)
    return $ Just newval  
  return [(dat, entry)]
    where
      (ma,mb) = bounds :: (Word64, Word64)
createEntry box IOfloat = do
  adjust <- adjustmentNew 0 (-1e50) 1e50 0.01 0.01 0
  entry <- spinButtonNew adjust 0.01 2
  boxPackStart box entry PackNatural 0
  dat <- newMVar 0
  _ <- afterInput entry $ do
    newval <- spinButtonGetValue entry
    modifyMVar_ dat (const $ return newval)
    return $ Just newval  
  return [(dat, entry)]
createEntry box IOchar2 = createNEntries box IOchar 2
createEntry box IOchar4 = createNEntries box IOchar 4
createEntry box IOchar8 = createNEntries box IOchar 8
createEntry box IOchar16 = createNEntries box IOchar 16
createEntry box IOuchar2 = createNEntries box IOuchar 2
createEntry box IOuchar4 = createNEntries box IOuchar 4
createEntry box IOuchar8 = createNEntries box IOuchar 8
createEntry box IOuchar16 = createNEntries box IOuchar 16
createEntry box IOshort2 = createNEntries box IOshort 2
createEntry box IOshort4 = createNEntries box IOshort 4
createEntry box IOshort8 = createNEntries box IOshort 8
createEntry box IOshort16 = createNEntries box IOshort 16
createEntry box IOushort2 = createNEntries box IOushort 2
createEntry box IOushort4 = createNEntries box IOushort 4
createEntry box IOushort8 = createNEntries box IOushort 8
createEntry box IOushort16 = createNEntries box IOushort 16
createEntry box IOint2 = createNEntries box IOint 2
createEntry box IOint4 = createNEntries box IOint 4
createEntry box IOint8 = createNEntries box IOint 8
createEntry box IOint16 = createNEntries box IOint 16
createEntry box IOuint2 = createNEntries box IOuint 2
createEntry box IOuint4 = createNEntries box IOuint 4
createEntry box IOuint8 = createNEntries box IOuint 8
createEntry box IOuint16 = createNEntries box IOuint 16
createEntry box IOlong2 = createNEntries box IOlong 2
createEntry box IOlong4 = createNEntries box IOlong 4
createEntry box IOlong8 = createNEntries box IOlong 8
createEntry box IOlong16 = createNEntries box IOlong 16
createEntry box IOulong2 = createNEntries box IOulong 2
createEntry box IOulong4 = createNEntries box IOulong 4
createEntry box IOulong8 = createNEntries box IOulong 8
createEntry box IOulong16 = createNEntries box IOulong 16
createEntry box IOfloat2 = createNEntries box IOfloat 2
createEntry box IOfloat4 = createNEntries box IOfloat 4
createEntry box IOfloat8 = createNEntries box IOfloat 8
createEntry box IOfloat16 = createNEntries box IOfloat 16

createNEntries :: BoxClass b => b -> IOPointDataType -> Int 
                  -> IO [(MVar Double,SpinButton)]
createNEntries box t n = do
  vbox <- vBoxNew True 0
  ret <- forM [0..n-1] $ \_ -> createEntry vbox t
  boxPackStart box vbox PackNatural 0
  return $ concat ret

-- -----------------------------------------------------------------------------
