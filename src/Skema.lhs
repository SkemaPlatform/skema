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
module Skema
    ( SkemaState(..), XS(..), io, runXS, get, put, trace, whenXS
    , statePutSelectedPos, statePutSelectedElem, statePutSkemaDoc, stateGet ) 
        where
\end{code}

\begin{code}
import Control.Monad( when, liftM )
import Control.Monad.IO.Class( MonadIO(..) )
import Control.Monad.State( MonadState, StateT(..), get, put )
import System.IO( hPutStrLn, stderr )
import Skema.SkemaDoc( SkemaDoc, SelectedElement )
\end{code}

\begin{code}
data SkemaState = SkemaState
    { skemaDoc :: SkemaDoc
    , selectedPos :: !(Double,Double)
    , selectedElem :: !SelectedElement }
\end{code}

\begin{code}
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
runXS :: SkemaState -> XS a -> IO (a, SkemaState)
runXS st (XS a) = runStateT a st
\end{code}

\begin{code}
statePutSelectedPos :: (Double,Double) -> XS ()
statePutSelectedPos pos = get >>= \s -> put $ s{ selectedPos = pos }
\end{code}

\begin{code}
statePutSelectedElem :: SelectedElement -> XS ()
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
