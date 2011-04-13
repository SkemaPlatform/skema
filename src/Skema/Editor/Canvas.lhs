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
drawVisualNode :: SkemaDoc -> Node -> Cr.Render ()
drawVisualNode skdoc node = do
    let px = nodePosx node
        py = nodePosy node
        wid = nodeWidth node
        hei = nodeHeight node
        headHeight = nodeHeadHeight node
        nodename = nodeName skdoc node
        rad = 4
        pointRad = nodePointRad node
        (rline, gline, bline) = nodeLineColor node
        (rbox, gbox, bbox) = nodeBoxColor node
        (rhead, ghead,bhead) = nodeHeadColor node

    -- shadow 1
    Cr.newPath
    Cr.arc (px+wid) (py+2*rad) rad (deg2rad $ -90) (deg2rad 0)
    Cr.arc (px+wid) (py+hei) rad (deg2rad 0) (deg2rad 90)
    Cr.arc px (py+hei) rad (deg2rad 90) (deg2rad 180)
    Cr.arc px (py+2*rad) rad (deg2rad 180) (deg2rad $ -90)
    Cr.closePath
    Cr.setSourceRGBA 0 0 0 0.2
    Cr.fill

    -- shadow 2
    Cr.newPath
    Cr.arc (px+wid-2) (py+2*rad+2) rad (deg2rad $ -90) (deg2rad 0)
    Cr.arc (px+wid-2) (py+hei-2) rad (deg2rad 0) (deg2rad 90)
    Cr.arc (px+2) (py+hei-2) rad (deg2rad 90) (deg2rad 180)
    Cr.arc (px+2) (py+2*rad+2) rad (deg2rad 180) (deg2rad $ -90)
    Cr.closePath
    Cr.setSourceRGBA 0 0 0 0.2
    Cr.fill

    -- box
    Cr.newPath
    Cr.arc (px+wid-rad) (py+rad) rad (deg2rad $ -90) (deg2rad 0)
    Cr.arc (px+wid-rad) (py+hei-rad) rad (deg2rad 0) (deg2rad 90)
    Cr.arc (px+rad) (py+hei-rad) rad (deg2rad 90) (deg2rad 180)
    Cr.arc (px+rad) (py+rad) rad (deg2rad 180) (deg2rad $ -90)
    Cr.closePath
    Cr.setSourceRGB rbox gbox bbox
    Cr.fillPreserve
    Cr.setLineWidth 1
    Cr.setSourceRGB rline gline bline
    Cr.stroke

    -- header
    Cr.newPath
    Cr.arc (px+wid-rad) (py+rad) rad (deg2rad $ -90) (deg2rad 0)
    Cr.lineTo (px+wid) (py+headHeight)
    Cr.lineTo px (py+headHeight)
    Cr.arc (px+rad) (py+rad) rad (deg2rad 180) (deg2rad $ -90)
    Cr.closePath
    Cr.setSourceRGB rhead ghead bhead
    Cr.fill

    -- in/out points
    Cr.setLineWidth 1
    Cr.arc px (py+hei-20) pointRad (deg2rad 0) (deg2rad 360)
    Cr.setSourceRGB 0.78 0.78 0.16
    Cr.fillPreserve
    Cr.setSourceRGB rline gline bline
    Cr.stroke

    Cr.arc px (py+hei-30) pointRad (deg2rad 0) (deg2rad 360)
    Cr.setSourceRGB 0.78 0.78 0.16
    Cr.fillPreserve
    Cr.setSourceRGB rline gline bline
    Cr.stroke

    Cr.arc (px+wid) (py+20) pointRad (deg2rad 0) (deg2rad 360)
    Cr.setSourceRGB 0.16 0.78 0.78
    Cr.fillPreserve
    Cr.setSourceRGB rline gline bline
    Cr.stroke

    Cr.setSourceRGB 1 1 1
    Cr.setFontSize 10
    Cr.moveTo (px + 2) (py + 11)
    Cr.showText nodename

    Cr.setSourceRGB 1 1 1
    Cr.setFontSize 8
    Cr.moveTo (px+6) (py+hei-18)
    Cr.showText "in x"
    Cr.moveTo (px+6) (py+hei-28)
    Cr.showText "in y"
    textSize <- Cr.textExtents "out z"
    Cr.moveTo (px+wid-(Cr.textExtentsWidth textSize)-6) (py+22)
    Cr.showText "out z"
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
