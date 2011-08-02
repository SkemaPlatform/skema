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
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Properties( main ) where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Test.QuickCheck( Arbitrary(..), quickCheck, arbitrarySizedIntegral )
import Text.Printf( printf )
import Skema()
import Skema.SkemaDoc()
import Skema.Editor.Util()
import Skema.Editor.MainWindow()
import Skema.Editor.NodeCLWindow()
import Skema.Editor.PFPreviewWindow()
import Skema.Editor.Types( Pos2D, Circle(..), inside, posx, posy )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
main :: IO ()
main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Skema.Util tests

\begin{code}
instance Arbitrary Pos2D where
  arbitrary = arbitrarySizedIntegral
  shrink = undefined
\end{code}

\begin{code}
prop_pos2d_signum :: Pos2D -> Bool
prop_pos2d_signum pos = abs pos * signum pos == pos
\end{code}

\begin{code}
prop_pos2d_sum :: Pos2D -> Pos2D -> Bool
prop_pos2d_sum pa pb  = pa + pb == pb + pa
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Skema.Editor.Types

\begin{code}
prop_inside_circle_center :: Pos2D -> Double -> Bool
prop_inside_circle_center pc rad = rad <= 0 
                                   || inside (posx pc) (posy pc) (Circle pc rad) 
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
tests :: [(String, IO ())]
tests = [
  ("Skema.Util: pos2D signum", quickCheck prop_pos2d_signum),
  ("Skema.Util: pos2D sum", quickCheck prop_pos2d_sum),
  ("Skema.Editor.Types: Area Circle center", quickCheck prop_inside_circle_center)
 ]
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
