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
module Skema.Util( Rect(..), deg2rad, inside ) where
\end{code}

\begin{code}
data Rect = Rect
    { x0 :: !Double, y0 :: !Double, x1 :: !Double, y1 :: !Double }
\end{code}

\begin{code}
-- | 'deg2rad' converts an arc measure from degree units to radian units
deg2rad :: (Floating a) => a -> a
deg2rad d = d * (pi / 180)
\end{code}

\begin{code}
-- | Check if a point is inside a rectangular region.
inside :: Double -> Double -> Rect -> Bool
inside px py rect = inx && iny
    where
      inx = (px >= x0 rect) && (px < x1 rect)
      iny = (py >= y0 rect) && (py < y1 rect)
\end{code}
