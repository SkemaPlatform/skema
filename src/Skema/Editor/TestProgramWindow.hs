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
import Graphics.UI.Gtk.Windows.Dialog( dialogNew, dialogRun )

-- -----------------------------------------------------------------------------
showTestProgramWindow :: IO ()
showTestProgramWindow = do
  window <- dialogNew
  widgetSetSizeRequest window 640 480
  
  widgetShowAll window 
  
  _ <- dialogRun window   
  
  widgetDestroy window
  
  return ()
