-- -----------------------------------------------------------------------------
-- This file is part of Skema.

-- Skema is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.

-- Skema is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with Skema.  If not, see <http://www.gnu.org/licenses/>.
-- -----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Skema.Editor.Types( 
  -- * Types
  Pos2D(..), RGBColor, Rect(..), Circle(..), 
  -- * Functions
  inside, posx, posy ) 
        where

-- -----------------------------------------------------------------------------
import Control.Applicative( pure )
import Control.Monad( mzero )
import Data.String( fromString )
import Data.Aeson( Value(..), ToJSON(..), FromJSON(..), (.=), (.:), object )

-- -----------------------------------------------------------------------------
type RGBColor = (Double, Double, Double)

-- -----------------------------------------------------------------------------
newtype Pos2D = Pos2D (Double,Double) deriving( Eq, Show )

instance ToJSON Pos2D where
  toJSON (Pos2D (x,y)) = object [
    fromString "x" .= x, fromString "y" .= y
    ]                       

instance FromJSON Pos2D where
  parseJSON (Object v) = do
    x <- v .: fromString "x"
    y <- v .: fromString "y"
    pure $ Pos2D (x,y)
  parseJSON _ = mzero

posx :: Pos2D -> Double
posx (Pos2D val) = fst val

posy :: Pos2D -> Double
posy (Pos2D val) = snd val

instance Num Pos2D where
    (Pos2D (ax,ay)) + (Pos2D (bx,by)) = Pos2D (ax+bx,ay+by)
    (Pos2D (ax,ay)) - (Pos2D (bx,by)) = Pos2D (ax-bx,ay-by)
    (Pos2D (ax,ay)) * (Pos2D (bx,by)) = Pos2D (ax*bx,ay*by)
    abs (Pos2D (x,y)) = Pos2D (abs x, abs y)
    signum (Pos2D (x,y)) = Pos2D (signum x, signum y)
    fromInteger i = Pos2D (fromInteger i,fromInteger i)

-- -----------------------------------------------------------------------------
class Mult a b c | a b -> c where
    (*.) :: a -> b -> c

instance Mult Double Pos2D Pos2D where
    n *. (Pos2D (x,y)) = Pos2D (n*x,n*y)

-- -----------------------------------------------------------------------------
data Rect = Rect
    { rectVertex0 :: !Pos2D, rectVertex1 :: !Pos2D }

data Circle = Circle
    { circleCenter :: !Pos2D, circleRad :: !Double }

-- -----------------------------------------------------------------------------
class Area a where
    -- | Check if a point is inside an area.
    inside :: Double -> Double -> a -> Bool

instance Area Rect where
    -- | Check if a point is inside a rectangular region.
    inside px py rect = inx && iny
        where
          inx = (px >= posx vertex0) && (px < posx vertex1)
          iny = (py >= posy vertex0) && (py < posy vertex1)
          vertex0 = rectVertex0 rect
          vertex1 = rectVertex1 rect

instance Area Circle where
    -- | Check if a point is inside a circle area.
    inside px py circle =  dist < circleRad circle ** 2
        where
          dist = (px - posx center)**2 + (py - posy center)**2
          center = circleCenter circle

-- -----------------------------------------------------------------------------

