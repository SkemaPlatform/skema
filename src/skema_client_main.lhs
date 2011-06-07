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
import Data.Version( showVersion )
import System.Environment( getProgName, getArgs )
import System.Console.GetOpt( 
  ArgOrder(..), OptDescr(..), ArgDescr(..), getOpt, usageInfo )
import System.Exit( exitSuccess )
import Paths_skema( version )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
data Options = Options 
               { skemafile :: !(Maybe FilePath)
               , infiles :: ![FilePath]
               , outfiles :: ![FilePath]
               } deriving( Show) 
\end{code}

\begin{code}
defaultOptions :: Options
defaultOptions = Options Nothing [] []
\end{code}

\begin{code}
options :: [OptDescr (Options -> IO Options)]
options = [Option "h" ["help"] (NoArg showUsage) "show usage"
          ,Option "v" ["version"] (NoArg printVersion) "show program version"
          ,Option "i" ["in"] (ReqArg setInputFile "FILE") "input file"
          ,Option "o" ["out"] (ReqArg setOutputFile "FILE") "output file"]
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
main :: IO ()
main = do
    args <- getArgs
    case getOpt RequireOrder options args of
      ([], [], []) -> do
        _ <- showUsage defaultOptions
        exitSuccess
      (actions, (skmf:[]), []) -> do
        opts <- foldl (>>=) (return defaultOptions) actions
        launch $ opts { skemafile = Just skmf }
      (_,_, errs) -> do
        h <- showHeader
        error $ concat errs ++ usageInfo h options
\end{code}

\begin{code}
showHeader :: IO String
showHeader = do
  self <- getProgName
  return $ "Usage: " ++ self ++ " [Options] skemafile"
\end{code}

\begin{code}
showUsage :: Options -> IO Options
showUsage opt = do
  h <- showHeader
  putStrLn $ usageInfo h options
  return opt
\end{code}

\begin{code}
printVersion :: Options -> IO Options
printVersion opt = do
  self <- getProgName
  putStrLn $ self ++ " " ++ showVersion version
  return opt
\end{code}

\begin{code}
setInputFile :: String -> Options -> IO Options
setInputFile file opts = return $ opts { 
  infiles = infiles opts ++ [file] }
\end{code}

\begin{code}
setOutputFile :: String -> Options -> IO Options
setOutputFile file opts = return $ opts { 
  outfiles = outfiles opts ++ [file] }
\end{code}

\begin{code}
launch :: Options -> IO ()
launch opts = do
  print opts
  return ()
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
