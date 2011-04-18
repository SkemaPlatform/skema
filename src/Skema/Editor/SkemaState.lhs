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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Skema.Editor.SkemaState
    ( SkemaState(..), XS(..), io, runXS, get, put, trace, whenXS
    , statePutSelectedPos, statePutSelectedPos2, statePutSelectedElem
    , statePutSkemaDoc, stateGet, stateSelectElement ) 
        where
\end{code}

\begin{code}
import Control.Monad( when, liftM, msum )
import Control.Monad.IO.Class( MonadIO(..) )
import Control.Monad.State( MonadState, StateT(..), get, put )
import qualified Data.IntMap as M( assocs )
import System.IO( hPutStrLn, stderr )
import Skema.SkemaDoc( SkemaDoc(..), SelectedElement(..), nodeKernel, selectNodeElement )
\end{code}

\begin{code}
-- | 'SkemaState', the (mutable) editor state.
data SkemaState = SkemaState
    { skemaDoc :: SkemaDoc -- ^ Skema document loaded in editor
    , selectedPos :: !(Double,Double) -- ^ last selected position
    , selectedPos2 :: !(Double,Double) -- ^ additional selected position
    , selectedElem :: Maybe SelectedElement -- ^ current selected element 
    }
\end{code}

\begin{code}
-- | The 'XS' monad 'StateT' transformer over 'IO' encapsulating the editor state
newtype XS a = XS (StateT SkemaState IO a)
    deriving( Monad, MonadIO, MonadState SkemaState )
\end{code}

\begin{code}
io :: MonadIO m => IO a -> m a
io = liftIO
\end{code}

\begin{code}
whenXS :: XS Bool -> XS () -> XS ()
whenXS a f = a >>= \b -> when b f
\end{code}

\begin{code}
trace :: MonadIO m => String -> m ()
trace = io . hPutStrLn stderr
\end{code}

\begin{code}
-- | Run the 'XS' monad, given a chunk of 'XS' monad code, and an initial state Return the result, and final state
runXS :: SkemaState -> XS a -> IO (a, SkemaState)
runXS st (XS a) = runStateT a st
\end{code}

\begin{code}
statePutSelectedPos :: (Double,Double) -> XS ()
statePutSelectedPos pos = get >>= \s -> put $ s{ selectedPos = pos }
\end{code}

\begin{code}
statePutSelectedPos2 :: (Double,Double) -> XS ()
statePutSelectedPos2 pos = get >>= \s -> put $ s{ selectedPos2 = pos }
\end{code}

\begin{code}
statePutSelectedElem :: Maybe SelectedElement -> XS ()
statePutSelectedElem se = get >>= \s -> put $ s{ selectedElem = se }
\end{code}

\begin{code}
statePutSkemaDoc :: SkemaDoc -> XS ()
statePutSkemaDoc sd = get >>= \s -> put $ s{ skemaDoc = sd }
\end{code}

\begin{code}
stateGet :: (SkemaState -> a) -> XS a
stateGet f = liftM f get
\end{code}

\begin{code}
stateSelectElement :: Double -> Double -> XS (Maybe SelectedElement)
stateSelectElement mx my = do
  stDoc <- stateGet skemaDoc
  return $ msum . map (selectNodeElement mx my) . map (\(i,n) -> (i,n,nodeKernel stDoc n)) . M.assocs.nodes $ stDoc
\end{code}