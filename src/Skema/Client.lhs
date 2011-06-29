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
module Skema.Client( sendSkema, createRun, sendRunInput ) where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Data.List( stripPrefix )
import Network.HTTP( simpleHTTP, Response(..) )
import Skema.Network( postMultipartData, postFormUrlEncoded )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
sendSkema :: String -> String -> IO (Maybe String)
sendSkema server skmData = do
  rq <- postMultipartData (server ++ "/programs") skmData
  rst <- simpleHTTP rq
  case rst of
    Left _ -> return Nothing
    Right a -> return . stripPrefix "inserted " . rspBody $ a
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
createRun :: String -> String -> IO (Maybe [Int])
createRun server pkey = do
  rq <- postFormUrlEncoded (server ++ "/runs") [("pid",pkey)]
  rst <- simpleHTTP rq
  case rst of
    Left a -> return Nothing
    Right a -> return . Just . read $ rspBody a
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
sendRunInput :: [Int] -> [FilePath] -> IO Bool
sendRunInput ports filenames = do
  if length ports /= length filenames
    then return False
    else return True
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
