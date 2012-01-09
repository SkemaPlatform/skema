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
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Properties( main ) where

-- -----------------------------------------------------------------------------
import Test.QuickCheck( Arbitrary(..), quickCheckResult, arbitrarySizedIntegral )
import Test.QuickCheck.Test( Result, isSuccess )
import Text.Printf( printf )
import System.Exit( exitSuccess, exitFailure )
import Skema.Editor.SkemaDoc()
import Skema.Editor.Util()
import Skema.Editor.MainWindow()
import Skema.Editor.NodeCLWindow()
import Skema.Editor.PFPreviewWindow()
import Skema.Editor.Types( Pos2D, Circle(..), inside, posx, posy )

-- -----------------------------------------------------------------------------
main :: IO ()
main = do
  results <- mapM (\(s,a) -> printf "%-25s: " s >> a) tests
  if all isSuccess results
    then exitSuccess 
    else exitFailure

-- -----------------------------------------------------------------------------
-- Skema.Util tests

instance Arbitrary Pos2D where
  arbitrary = arbitrarySizedIntegral
  shrink = undefined

prop_pos2d_signum :: Pos2D -> Bool
prop_pos2d_signum pos = abs pos * signum pos == pos

-- -----------------------------------------------------------------------------
-- Skema.Editor.Types

prop_inside_circle_center :: Pos2D -> Double -> Bool
prop_inside_circle_center pc rad = rad <= 0 
                                   || inside (posx pc) (posy pc) (Circle pc rad) 

-- -----------------------------------------------------------------------------
tests :: [(String, IO Result)]
tests = [
  ("Skema.Util: pos2D signum", quickCheckResult prop_pos2d_signum),
  ("Skema.Editor.Types: Area Circle center", quickCheckResult prop_inside_circle_center)
 ]

-- -----------------------------------------------------------------------------
