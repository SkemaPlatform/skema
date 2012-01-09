{- -----------------------------------------------------------------------------
Copyright (C) 2011  Luis Cabellos - Instituto de Fisica de Cantabria
This file is part of Skema.

Skema is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Skema is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Skema.  If not, see <http://www.gnu.org/licenses/>.
-- ----------------------------------------------------------------------------}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Skema.Editor.SkemaState( 
  SkemaState(..), emptySkemaState, XS(..), io, runXS, get, put, trace, whenXS, 
  statePutSelectedPos, statePutSelectedPos2, statePutSelectedElem, 
  statePutSkemaDoc, stateGet, stateSelectElement, stateInsertNewArrow,
  statePutSkemaDocFilename ) 
       where

-- -----------------------------------------------------------------------------
import Control.Monad( when, liftM, msum )
import Control.Monad.IO.Class( MonadIO(..) )
import Control.Monad.State( MonadState, StateT(..), get, put )
import System.IO( hPutStrLn, stderr )
import Skema.Editor.Types( Pos2D(..) )
import Skema.Editor.SkemaDoc( 
  SDKernelID, SDNodeID, SkemaDoc(..), SelectedElement(..), nodeKernel, 
  selectNodeElement, insertNewArrow, emptySkemaDoc, skemaDocGetNodesAssocs )

-- -----------------------------------------------------------------------------
-- | 'SkemaState', the (mutable) editor state.
data SkemaState = SkemaState
    { skemaDoc :: SkemaDoc -- ^ Skema document loaded in editor
    , skemaDocFile :: Maybe FilePath -- ^ Skema document filename
    , selectedPos :: !Pos2D -- ^ last selected position
    , selectedPos2 :: !Pos2D -- ^ additional selected position
    , selectedElem :: Maybe SelectedElement -- ^ current selected element 
    , selectedKernel :: Maybe SDKernelID -- ^ current selected kernel
    }

emptySkemaState :: SkemaState
emptySkemaState = SkemaState emptySkemaDoc Nothing 
                  (Pos2D (0,0)) (Pos2D (0,0)) Nothing Nothing

-- | The 'XS' monad 'StateT' transformer over 'IO' encapsulating the editor state
newtype XS a = XS (StateT SkemaState IO a)
    deriving( Monad, MonadIO, MonadState SkemaState )

instance Functor XS where
  fmap f x = x >>= (return . f)

io :: MonadIO m => IO a -> m a
io = liftIO

whenXS :: XS Bool -> XS () -> XS ()
whenXS a f = a >>= \b -> when b f

trace :: MonadIO m => String -> m ()
trace = io . hPutStrLn stderr

-- | Run the 'XS' monad, given a chunk of 'XS' monad code, and an initial state Return the result, and final state
runXS :: SkemaState -> XS a -> IO (a, SkemaState)
runXS st (XS a) = runStateT a st

statePutSelectedPos :: Pos2D -> XS ()
statePutSelectedPos pos = get >>= \s -> put $ s{ selectedPos = pos }

statePutSelectedPos2 :: Pos2D -> XS ()
statePutSelectedPos2 pos = get >>= \s -> put $ s{ selectedPos2 = pos }

statePutSelectedElem :: Maybe SelectedElement -> XS ()
statePutSelectedElem se = get >>= \s -> put $ s{ selectedElem = se }

statePutSkemaDoc :: SkemaDoc -> XS ()
statePutSkemaDoc sd = get >>= \s -> put $ s{ skemaDoc = sd }

statePutSkemaDocFilename :: Maybe FilePath -> XS ()
statePutSkemaDocFilename fl = get >>= \s -> put $ s{ skemaDocFile = fl }

stateGet :: (SkemaState -> a) -> XS a
stateGet f = liftM f get

stateSelectElement :: Pos2D -> XS (Maybe SelectedElement)
stateSelectElement pos = do
  stDoc <- stateGet skemaDoc
  return $ msum . map (selectNodeElement pos . \(i,n) -> (i,n,nodeKernel stDoc n)) . skemaDocGetNodesAssocs $ stDoc

stateInsertNewArrow :: SDNodeID -> Int -> SDNodeID -> Int -> XS ()
stateInsertNewArrow ki ji kf jf = do
  stDoc <- stateGet skemaDoc
  statePutSkemaDoc $ insertNewArrow stDoc ki ji kf jf

-- -----------------------------------------------------------------------------
