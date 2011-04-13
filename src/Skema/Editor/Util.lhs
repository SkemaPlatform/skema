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
module Skema.Editor.Util
    ( RGBColor, roundedRectanglePath, circlePath, drawFill, drawFillStroke
    , showTextOn )
    where
\end{code}

\begin{code}
import qualified Graphics.Rendering.Cairo as Cr
    ( Render, setSourceRGB, moveTo, newPath, closePath, arc, showText, stroke
    , fill, fillPreserve )
import Skema.Util( deg2rad )
\end{code}

\begin{code}
type RGBColor = (Double,Double,Double)
\end{code}

\begin{code}
roundedRectanglePath :: Double -- ^ x position
                     -> Double -- ^ y position 
                     -> Double -- ^ width
                     -> Double -- ^ height
                     -> Double -- ^ corner curvature radius
                     -> Cr.Render ()
roundedRectanglePath px py wid hei rad = do
  Cr.newPath
  Cr.arc (px+wid-rad) (py+rad) rad (deg2rad $ -90) (deg2rad 0)
  Cr.arc (px+wid-rad) (py+hei-rad) rad (deg2rad 0) (deg2rad 90)
  Cr.arc (px+rad) (py+hei-rad) rad (deg2rad 90) (deg2rad 180)
  Cr.arc (px+rad) (py+rad) rad (deg2rad 180) (deg2rad $ -90)
  Cr.closePath
\end{code}

\begin{code}
circlePath :: Double -- ^ x position
           -> Double -- ^ y position
           -> Double -- ^ radius
           -> Cr.Render ()
circlePath px py rad = Cr.arc px py rad (deg2rad 0) (deg2rad 360)
\end{code}

\begin{code}
drawFill :: RGBColor -- ^ fill color
         -> Cr.Render ()
drawFill (rf,gf,bf) = do
  Cr.setSourceRGB rf gf bf
  Cr.fill
\end{code}

\begin{code}
drawFillStroke :: RGBColor -- ^ fill color
               -> RGBColor -- ^ line color
               -> Cr.Render ()
drawFillStroke (rf,gf,bf) (rl,gl,bl) = do
  Cr.setSourceRGB rf gf bf
  Cr.fillPreserve
  Cr.setSourceRGB rl gl bl
  Cr.stroke
\end{code}

\begin{code}
showTextOn :: Double -- ^ x position
           -> Double -- ^ y position
           -> String -- ^ text
           -> Cr.Render ()
showTextOn px py text = Cr.moveTo px py >> Cr.showText text
\end{code}

