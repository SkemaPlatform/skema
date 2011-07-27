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
import Control.Concurrent.MVar( newMVar )
import Data.Version( showVersion )
import qualified Data.IntMap as M( fromList )
import Graphics.UI.Gtk
    ( mainQuit, initGUI, mainGUI, onDestroy, castToWindow, widgetShowAll )
import Graphics.UI.Gtk.Glade( xmlNew, xmlGetWidget )
import System.Environment( getProgName, getArgs )
import System.IO.Unsafe( unsafePerformIO )
import System.Locale.SetLocale( Category(..), setLocale )
import Text.I18N.GetText( bindTextDomain, textDomain, getText )
import Paths_skema( getDataFileName, version )
import Skema.Editor.SkemaState( SkemaState(..), emptySkemaState )
import Skema.Editor.Types( Pos2D(..) )
import Skema.Types( IOPointType(..), IOPointDataType(..) )
import Skema.SkemaDoc
    ( SkemaDoc(..), Kernel(..), Node(..), IOPoint(..), NodeArrow(..)
    , emptySkemaDoc, emptyKernel )
import Skema.Editor.MainWindow( prepareMainWindow )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
testDoc :: SkemaDoc
testDoc = emptySkemaDoc {
            library = M.fromList [(0,k1),(1,k2)]
          , nodes = M.fromList [(0,NodeKernel (Pos2D (150,20)) 0),(1,NodeKernel (Pos2D (50,110)) 0)]
          , arrows = [NodeArrow 1 2 0 1] }
    where
      k1 = emptyKernel {
             name = "Adder" 
           , body = "int id = get_global_id(0);\nz[id] = x[id] + y[id];"
           , iopoints = M.fromList [(0,IOPoint "x" IOfloat InputPoint),
                                    (1,IOPoint "y" IOfloat InputPoint),
                                    (2,IOPoint "z" IOfloat OutputPoint)] }
      k2 = emptyKernel {
             name = "Scaler" 
           , body = "int id = get_global_id(0);\nx2[id] = 2*input[id];\nx3[id] = 3*input[id];"
           , iopoints = M.fromList [(0,IOPoint "input" IOint InputPoint),
                                    (1,IOPoint "x2" IOfloat OutputPoint),
                                    (2,IOPoint "x3" IOfloat OutputPoint)] }
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
__ :: String -> String
__ = unsafePerformIO . getText
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
main :: IO ()
main = do
  _ <- setLocale LC_ALL (Just "") 
  _ <- bindTextDomain __MESSAGE_CATALOG_DOMAIN__ (Just __MESSAGE_CATALOG_DIR__)
  _ <- textDomain (Just __MESSAGE_CATALOG_DOMAIN__)
  
  args <- getArgs
  self <- getProgName
  case args of
    []                    -> launch
    ["--help"]            -> usage
    ["--version"]         -> putStrLn (self ++ " " ++ showVersion version)
    _                     -> fail (__"unrecognized flags")
\end{code}

\begin{code}
usage :: IO ()
usage = do
    self <- getProgName
    putStr . unlines $
        [concat [(__"Usage: "), self, " [OPTION]"],
         (__"Options:"),
         (__"  --help                       Print this message"),
         (__"  --version                    Print the version number")]
\end{code}

\begin{code}
launch :: IO ()
launch = do
  _ <- initGUI
  glade <- getDataFileName "skema.glade"
  Just xml <- xmlNew glade
  window <- xmlGetWidget xml castToWindow "main"
  _ <- onDestroy window mainQuit
 
  state <- newMVar $ emptySkemaState { skemaDoc = testDoc }

  prepareMainWindow xml state      
  
  widgetShowAll window 

  mainGUI
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
