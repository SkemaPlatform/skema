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
module Skema.Client( sendSkema, createRun, sendRunInput, getRunOuput ) where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Control.Monad( zipWithM, unless )
import Data.List( stripPrefix )
import Network.HTTP( simpleHTTP, Response(..) )
import Network.Socket
  ( SocketType(..), AddrInfo(..), AddrInfoFlag(..), Family(..), socket, sClose, 
    defaultHints, withSocketsDo, connect, getAddrInfo, defaultProtocol )
import Network.Socket.ByteString( sendAll, recv )
import qualified Data.ByteString.Char8 as BC( readFile, hPut, null )
import System.IO( IOMode(..), hClose, openBinaryFile )
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
createRun :: String -> String -> IO (Maybe ([Int],[Int]))
createRun server pkey = do
  rq <- postFormUrlEncoded (server ++ "/runs") [("pid",pkey)]
  rst <- simpleHTTP rq
  case rst of
    Left _ -> return Nothing
    Right a -> return . Just . read $ rspBody a
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
desiredAddr :: Maybe AddrInfo
desiredAddr = Just $ defaultHints {
  addrSocketType = Stream,
  addrFamily = AF_INET, 
  addrFlags = [AI_PASSIVE]}
\end{code}

\begin{code}
sendFile :: String -> Int -> FilePath -> IO Bool
sendFile server port filename = withSocketsDo $ do
  addrinfos <- getAddrInfo desiredAddr (Just server) (Just . show $ port)
  if null addrinfos
    then do
      return False
    else do
      datafile <- BC.readFile filename
      let serveraddr = head addrinfos
      sock <- socket (addrFamily serveraddr) Stream defaultProtocol
      connect sock (addrAddress serveraddr)
      sendAll sock datafile
      sClose sock
      return True
\end{code}

\begin{code}
getFile :: String -> Int -> FilePath -> IO Bool
getFile server port filename = withSocketsDo $ do
  addrinfos <- getAddrInfo desiredAddr (Just server) (Just . show $ port)
  if null addrinfos
    then do
      return False
    else do
      f <- openBinaryFile filename WriteMode
      let serveraddr = head addrinfos
      sock <- socket (addrFamily serveraddr) Stream defaultProtocol
      connect sock (addrAddress serveraddr)
      getLoop f sock
      sClose sock
      hClose f
      return True
  where
    getLoop f conn = do
      msg <- recv conn 1024
      unless (BC.null msg) $ do
        BC.hPut f msg
        getLoop f conn
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
sendRunInput :: String -> [Int] -> [FilePath] -> IO Bool
sendRunInput server ports filenames = do
  if length ports /= length filenames
    then return False
    else do
      results <- zipWithM (sendFile server) ports filenames
      return . all id $ results
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
getRunOuput :: String -> [Int] -> [FilePath] -> IO Bool
getRunOuput server ports filenames = do
  if length ports /= length filenames
    then return False
    else do
      results <- zipWithM (getFile server) ports filenames
      return . all id $ results  
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
