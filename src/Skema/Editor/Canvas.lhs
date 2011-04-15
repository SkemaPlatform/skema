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
import Control.Monad( liftM )
import qualified Data.IntMap as M( elems )
import qualified Graphics.Rendering.Cairo as Cr
    ( Render, FontSlant(..), FontWeight(..), setSourceRGB, setSourceRGBA
    , setLineWidth, setFontSize, lineTo, newPath, closePath, arc
    , selectFontFace, fill, paint, textExtents, textExtentsWidth )
import Skema.SkemaDoc
    ( SkemaDoc(..), Node, IOPoint, nodePosx, nodePosy, nodeHeight, nodeWidth
    , nodePointRad, nodeHeadHeight, nodeHeadColor
    , nodeName, nodeInputPoints, nodeOutputPoints, nodeIOPPosition
    , isInputPoint, iopName )
import Skema.Util( deg2rad, RGBColor )
import Skema.Editor.Util
    ( roundedRectanglePath, circlePath, drawFill, drawFillStroke, showTextOn
    , calcFontHeight )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
class ElemColor a where
    lineColor :: a -> RGBColor
    lineColor = const (0.15,0.15,0.15)
    fillColor :: a -> RGBColor

instance ElemColor Node where
    fillColor = const (0.59,0.59,0.59)

instance ElemColor IOPoint where
    fillColor point
        | isInputPoint point = (0.78,0.78,0.16)
        | otherwise = (0.16,0.78,0.78)
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
      rad = 4
      sinc = 4
      linecolor = lineColor node

  Cr.setLineWidth 1

  -- shadow
  Cr.setSourceRGBA 0 0 0 0.2
  roundedRectanglePath (px-sinc) (py+sinc) (wid+sinc*2) hei rad
  Cr.fill
  roundedRectanglePath (px-sinc*0.5) (py+sinc*1.5) (wid+sinc) (hei-sinc) rad
  Cr.fill

  -- box
  roundedRectanglePath px py wid hei rad
  drawFillStroke (fillColor node) linecolor

  -- header
  Cr.newPath
  Cr.arc (px+wid-rad) (py+rad) rad (deg2rad $ -90) (deg2rad 0)
  Cr.lineTo (px+wid) (py+headHeight)
  Cr.lineTo px (py+headHeight)
  Cr.arc (px+rad) (py+rad) rad (deg2rad 180) (deg2rad $ -90)
  Cr.closePath
  drawFill $ nodeHeadColor node

  Cr.setSourceRGB 1 1 1
  Cr.setFontSize 10
  showTextOn (px + 2) (py + 11) $ nodeName skdoc node

  -- in/out points
  Cr.setFontSize 8
  mapM_ (drawIOPoint node) (zip [0..] $ nodeInputPoints skdoc node)
  mapM_ (drawIOPoint node) (zip [0..] $ nodeOutputPoints skdoc node)
\end{code}

\begin{code}
drawIOPoint :: Node -> (Int, IOPoint) -> Cr.Render ()
drawIOPoint node (ipos,point) = do
  let pointRad = nodePointRad node
      (px,py) = nodeIOPPosition node point ipos
      name = iopName point

  fontHeight <- calcFontHeight

  circlePath px py pointRad
  drawFillStroke (fillColor point) (lineColor point)
  Cr.setSourceRGB 0.2 0.2 0.2
  if (isInputPoint point)
    then do
      showTextOn (px+6) (py+fontHeight*0.5) name
    else do
      textWidth <- liftM Cr.textExtentsWidth $ Cr.textExtents name
      showTextOn (px-textWidth-pointRad-1) (py+fontHeight*0.5) name
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
