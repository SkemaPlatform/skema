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
import Graphics.UI.Gtk
    ( mainQuit, initGUI, mainGUI, onDestroy, castToWindow, widgetShowAll )
import Graphics.UI.Gtk.Glade( xmlNew, xmlGetWidget )
import System.Environment( getProgName, getArgs )
import System.IO.Unsafe( unsafePerformIO )
import System.Locale.SetLocale( Category(..), setLocale )
import Text.I18N.GetText( bindTextDomain, textDomain, getText )
import Paths_skema( getDataFileName, version )
import Skema.Editor.SkemaState( SkemaState(..), emptySkemaState )
import Skema.SkemaDoc( emptySkemaDoc )
import Skema.Editor.MainWindow( prepareMainWindow )
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
        [concat [__"Usage: ", self, " [OPTION]"],
         __"Options:",
         __"  --help                       Print this message",
         __"  --version                    Print the version number"]
\end{code}

\begin{code}
launch :: IO ()
launch = do
  _ <- initGUI
  glade <- getDataFileName "skema.glade"
  Just xml <- xmlNew glade
  window <- xmlGetWidget xml castToWindow "main"
  _ <- onDestroy window mainQuit
 
  state <- newMVar $ emptySkemaState { skemaDoc = emptySkemaDoc }

  prepareMainWindow xml state      
  
  widgetShowAll window 

  mainGUI
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
