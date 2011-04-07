\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Skema where
\end{code}

\begin{code}
import Control.Monad( when )
import Control.Monad.IO.Class( MonadIO(..) )
import Control.Monad.State( MonadState, StateT(..) )
import System.IO( hPutStrLn, stderr )
\end{code}

\begin{code}
data SkemaState = SkemaState
    { blah :: !Int }
\end{code}

\begin{code}
newtype XS a = XS (StateT SkemaState IO a)
    deriving( Monad, MonadState SkemaState )
\end{code}

\begin{code}
io :: MonadIO m => IO a -> m a
io = liftIO
\end{code}

\begin{code}
whenX :: XS Bool -> XS () -> XS ()
whenX a f = a >>= \b -> when b f
\end{code}

\begin{code}
trace :: MonadIO m => String -> m ()
trace = io . hPutStrLn stderr
\end{code}
