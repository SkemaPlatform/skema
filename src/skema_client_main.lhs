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
import Paths_skema( version )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
main :: IO ()
main = do
    args <- getArgs
    self <- getProgName
    case args of
        []                    -> launch
        ["--help"]            -> usage
        ["--version"]         -> putStrLn (self ++ " " ++ showVersion version)
        _                     -> fail "unrecognized flags"
\end{code}

\begin{code}
usage :: IO ()
usage = do
    self <- getProgName
    putStr . unlines $
        concat ["Usage: ", self, " [OPTION]"] :
        "Options:" :
        "  --help                       Print this message" :
        "  --version                    Print the version number" :
        []
\end{code}

\begin{code}
launch :: IO ()
launch = do
  return ()
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%