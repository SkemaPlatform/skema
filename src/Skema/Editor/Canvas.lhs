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
module Skema.Editor.Canvas( drawSkemaDoc ) where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import qualified Data.Map as M( elems )
import qualified Graphics.Rendering.Cairo as Cr
    ( Render, FontSlant(..), FontWeight(..), setSourceRGB, setSourceRGBA
    , setLineWidth, setFontSize, moveTo, lineTo, newPath, closePath, arc
    , selectFontFace, showText, stroke, fill, fillPreserve, paint, textExtents
    , textExtentsWidth )
import Skema.Util( deg2rad )
import Skema.SkemaDoc
    ( SkemaDoc(..), Node, nodePosx, nodePosy, nodeHeight, nodeWidth, nodePointRad
    , nodeHeadHeight, nodeLineColor, nodeBoxColor, nodeHeadColor, nodeName )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
drawSkemaDoc :: Double -> Double -> SkemaDoc -> Cr.Render ()
drawSkemaDoc _ _ skdoc = do
    Cr.setSourceRGB 0.45 0.45 0.45
    Cr.paint

    Cr.selectFontFace "arial" Cr.FontSlantNormal Cr.FontWeightNormal
    mapM_ (drawVisualNode skdoc) (M.elems.nodes $ skdoc)
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

\begin{code}
drawVisualNode :: SkemaDoc -> Node -> Cr.Render ()
drawVisualNode skdoc node = do
    let px = nodePosx node
        py = nodePosy node
        wid = nodeWidth node
        hei = nodeHeight node
        headHeight = nodeHeadHeight node
        rad = 4
        sinc = 4
        sinc2 = sinc / 2
        pointRad = nodePointRad node
        linecolor = nodeLineColor node

    Cr.setLineWidth 1

    -- shadow
    Cr.setSourceRGBA 0 0 0 0.2
    roundedRectanglePath (px-sinc) (py+sinc) (wid+sinc*2) hei rad
    Cr.fill
    roundedRectanglePath (px-sinc*0.5) (py+sinc*1.5) (wid+sinc) (hei-sinc) rad
    Cr.fill

    -- box
    roundedRectanglePath px py wid hei rad
    drawFillStroke (nodeBoxColor node) linecolor

    -- header
    Cr.newPath
    Cr.arc (px+wid-rad) (py+rad) rad (deg2rad $ -90) (deg2rad 0)
    Cr.lineTo (px+wid) (py+headHeight)
    Cr.lineTo px (py+headHeight)
    Cr.arc (px+rad) (py+rad) rad (deg2rad 180) (deg2rad $ -90)
    Cr.closePath
    drawFill $ nodeHeadColor node

    -- in/out points
    circlePath px (py+hei-20) pointRad
    drawFillStroke (0.78,0.78,0.16) linecolor

    circlePath px (py+hei-30) pointRad
    drawFillStroke (0.78,0.78,0.16) linecolor

    circlePath (px+wid) (py+20) pointRad
    drawFillStroke (0.16,0.78,0.78) linecolor

    Cr.setSourceRGB 1 1 1
    Cr.setFontSize 10
    showTextOn (px + 2) (py + 11) $ nodeName skdoc node

    Cr.setFontSize 8
    showTextOn (px+6) (py+hei-18) "in x"
    showTextOn (px+6) (py+hei-28) "in y"
    textSize <- Cr.textExtents "out z"
    showTextOn (px+wid-(Cr.textExtentsWidth textSize)-6) (py+22) "out z"
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
