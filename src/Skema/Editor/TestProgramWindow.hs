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
import Data.Maybe( isJust, fromJust )
import Graphics.UI.Gtk( 
  widgetShowAll, widgetDestroy, widgetSetSensitivity )
import Graphics.UI.Gtk.Glade( xmlNew, xmlGetWidget )
import Graphics.UI.Gtk.Abstract.Box( BoxClass, Packing(..), boxPackStart )
import Graphics.UI.Gtk.Display.Label( labelNew )
import Graphics.UI.Gtk.Layout.HBox( hBoxNew )
import Graphics.UI.Gtk.Layout.VBox( vBoxNew )
import Graphics.UI.Gtk.Windows.Dialog( 
  ResponseId(..), castToDialog, dialogRun, dialogAddButton )
import Graphics.UI.Gtk.Windows.Window( windowSetDefault )
import Graphics.UI.Gtk.Scrolling.ScrolledWindow( 
  castToScrolledWindow, scrolledWindowAddWithViewport )
import Graphics.UI.Gtk.Buttons.Button( castToButton, buttonActivated )
import Graphics.UI.Gtk.General.StockItems( stockOk )
import Graphics.UI.Gtk.Entry.SpinButton( 
  SpinButton, spinButtonNew, spinButtonGetValue, afterInput, 
  spinButtonSetValue )
import Graphics.UI.Gtk.Misc.Adjustment( adjustmentNew )
import System.Glib.Signals( on )
import Paths_skema( getDataFileName )
import Skema.Types( 
  IOPointDataType(..), IOPointType(..), dataTypeBase, dataTypeVectorSize )
import Skema.ProgramFlow( 
  ProgramFlow, IOPoint(..), unasignedOutputPoints, unasignedInputPoints )
import Skema.SIDMap( toInt )
import Skema.DataValue( 
  DataValue(..), updateDataValue, extractValue, valuesToByteString, 
  convertToDataValues )
import Skema.RunProtocol( ServerPort(..), runBuffers )

-- -----------------------------------------------------------------------------
showTestProgramWindow :: ProgramFlow -> IO ()
showTestProgramWindow pf = do
  -- TODO: check program flow ins/outs
  
  glade <- getDataFileName "pf_test.glade"
  Just xml <- xmlNew glade
  window <- xmlGetWidget xml castToDialog "main"
  
  sw_in <- xmlGetWidget xml castToScrolledWindow "sw_in"
  vbox_in <- vBoxNew True 0
  invals <- forM (unasignedInputPoints pf)
            (liftM (map fst) . createIOControl vbox_in)
  scrolledWindowAddWithViewport sw_in vbox_in  

  sw_out <- xmlGetWidget xml castToScrolledWindow "sw_out"
  vbox_out <- vBoxNew True 0
  outents <- forM (unasignedOutputPoints pf) 
             (liftM (map snd) . createIOControl vbox_out)
  scrolledWindowAddWithViewport sw_out vbox_out  
  
  btn_run <- xmlGetWidget xml castToButton "btn_run"
  _ <- btn_run `on` buttonActivated $ do
    ins <- forM invals $ liftM valuesToByteString . mapM readMVar
    outs <- runBuffers ins (ServerPort "tesla01.ifca.es" 8080) pf
    when (isJust outs) 
      (do
          let dvals = map (uncurry convertToDataValues) 
                      (zip 
                       (fromJust outs)
                       (map iopDataType $ unasignedOutputPoints pf))
          forM_ (zip outents dvals) $ \(entries,vals) -> do
            forM_ (zip entries vals) (uncurry putOnSpin)
      )
    print "test Ended"
    
  -- buttons
  okButton <- dialogAddButton window stockOk ResponseAccept
  windowSetDefault window $ Just okButton
  
  widgetShowAll window 
  _ <- dialogRun window 
  widgetDestroy window
  
  return ()
  
-- -----------------------------------------------------------------------------
putOnSpin :: SpinButton -> DataValue -> IO ()
putOnSpin entry  = spinButtonSetValue entry . extractValue

-- -----------------------------------------------------------------------------
createIOControl :: BoxClass b => b -> IOPoint -> IO [(MVar DataValue,SpinButton)]
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

configureEntry :: (BoxClass b, Num n) => b -> (n -> DataValue)
                  -> Double -> Double -> Double -> Int
                  -> IO [(MVar DataValue,SpinButton)]
configureEntry box f ma mb step decimals = do
  adjust <- adjustmentNew 0 ma mb step step 0
  entry <- spinButtonNew adjust step decimals
  dat <- newMVar $ f 0
  _ <- afterInput entry $ do
    newval <- spinButtonGetValue entry
    modifyMVar_ dat (return . updateDataValue newval)
    return $ Just newval  
  boxPackStart box entry PackNatural 0
  return [(dat, entry)]
  
configureIntEntry :: (BoxClass b, Num n, Integral d) 
                     => b -> (n -> DataValue) -> (d,d)
                     -> IO [(MVar DataValue,SpinButton)]
configureIntEntry box f (ma, mb) = 
  configureEntry box f (fromIntegral ma) (fromIntegral mb) 1 0
  
createEntry :: BoxClass b => b -> IOPointDataType 
               -> IO [(MVar DataValue,SpinButton)]
createEntry b IOchar = configureIntEntry b DVchar (bounds::(Int8,Int8))
createEntry b IOuchar = configureIntEntry b DVuchar (bounds::(Word8,Word8))
createEntry b IOshort = configureIntEntry b DVshort (bounds::(Int16,Int16))
createEntry b IOushort = configureIntEntry b DVushort (bounds::(Word16,Word16))
createEntry b IOint = configureIntEntry b DVint (bounds::(Int32,Int32))
createEntry b IOuint = configureIntEntry b DVuint (bounds::(Word32,Word32))
createEntry b IOlong = configureIntEntry b DVlong (bounds::(Int64,Int64))
createEntry b IOulong = configureIntEntry b DVulong (bounds::(Word64,Word64))
createEntry b IOfloat = configureEntry b DVfloat (-1e50) 1e50 0.01 2

createEntry b t = createNEntries b (dataTypeBase t) (dataTypeVectorSize t)

createNEntries :: BoxClass b => b -> IOPointDataType -> Int 
                  -> IO [(MVar DataValue,SpinButton)]
createNEntries b t n = do
  vbox <- vBoxNew True 0
  ret <- forM [0..n-1] $ \_ -> createEntry vbox t
  boxPackStart b vbox PackNatural 0
  return $ concat ret

-- -----------------------------------------------------------------------------
