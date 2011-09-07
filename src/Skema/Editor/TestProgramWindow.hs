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
import Graphics.UI.Gtk( widgetShowAll, widgetSetSizeRequest, widgetDestroy )
import Graphics.UI.Gtk.Abstract.Box( Packing(..), boxPackStart )
import Graphics.UI.Gtk.Display.Label( labelNew )
import Graphics.UI.Gtk.Layout.HBox( hBoxNew )
import Graphics.UI.Gtk.Windows.Dialog( 
  ResponseId(..), dialogNew, dialogRun, dialogGetUpper, dialogAddButton )
import Graphics.UI.Gtk.Windows.Window( windowSetDefault )
import Graphics.UI.Gtk.Scrolling.ScrolledWindow( scrolledWindowNew )
import Graphics.UI.Gtk.Buttons.Button( buttonNewFromStock )
import Graphics.UI.Gtk.General.StockItems( stockExecute, stockOk )

-- -----------------------------------------------------------------------------
showTestProgramWindow :: IO ()
showTestProgramWindow = do
  window <- dialogNew
  widgetSetSizeRequest window 640 480
  
  internal <- dialogGetUpper window
  -- head
  lblName <- labelNew $ Just "Test"
  boxPackStart internal lblName PackNatural 0  
  
  -- body
  hbox0 <- hBoxNew True 0
  sw_in <- scrolledWindowNew Nothing Nothing
  sw_out <- scrolledWindowNew Nothing Nothing
  btn_run <- buttonNewFromStock stockExecute
  boxPackStart hbox0 sw_in PackGrow 0
  boxPackStart hbox0 btn_run PackNatural 0
  boxPackStart hbox0 sw_out PackGrow 0  
  boxPackStart internal hbox0 PackGrow 0  
  
  -- buttons
  okButton <- dialogAddButton window stockOk ResponseAccept
  windowSetDefault window $ Just okButton
  
  widgetShowAll window 
  
  _ <- dialogRun window   
  
  widgetDestroy window
  
  return ()
