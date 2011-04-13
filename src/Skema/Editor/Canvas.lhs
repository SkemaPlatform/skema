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
    ( SkemaDoc(..), Node, nodePosx, nodePosy, nodeHeight, nodeWidth
    , nodePointRad, nodeHeadHeight, nodeLineColor, nodeBoxColor, nodeHeadColor
    , nodeInputColor, nodeOutputColor, nodeName, nodeInputs, nodeOutputs )
import Skema.Util( deg2rad )
import Skema.Editor.Util
    ( roundedRectanglePath, circlePath, drawFill, drawFillStroke, showTextOn
    , calcFontHeight )
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

  Cr.setSourceRGB 1 1 1
  Cr.setFontSize 10
  showTextOn (px + 2) (py + 11) $ nodeName skdoc node

  -- in/out points
  Cr.setFontSize 8
  mapM_ (drawInputPoint node) (zip [0..] $ nodeInputs skdoc node)
  mapM_ (drawOutputPoint node) (zip [0..] $ nodeOutputs skdoc node)
\end{code}

\begin{code}
drawInputPoint :: Node -> (Int, String) -> Cr.Render ()
drawInputPoint node (ipos,inname) = do
  let pos = fromIntegral ipos
      hei = nodeHeight node
      pointRad = nodePointRad node
      px = nodePosx node
      py = (nodePosy node) + hei - 10 - pos*2*(pointRad+1)
      fillcolor = nodeInputColor node
      linecolor = nodeLineColor node

  fontHeight <- calcFontHeight

  circlePath px py pointRad
  drawFillStroke fillcolor linecolor
  Cr.setSourceRGB 0.2 0.2 0.2
  showTextOn (px+6) (py+fontHeight*0.5) inname
\end{code}

\begin{code}
drawOutputPoint :: Node -> (Int, String) -> Cr.Render ()
drawOutputPoint node (ipos,outname) = do
  let pos = fromIntegral ipos
      pointRad = nodePointRad node
      px = (nodePosx node) + (nodeWidth node)
      py = (nodePosy node) + (nodeHeadHeight node) + 10 + pos*2*(pointRad+1)
      fillcolor = nodeOutputColor node
      linecolor = nodeLineColor node

  fontHeight <- calcFontHeight
  textWidth <- liftM Cr.textExtentsWidth $ Cr.textExtents outname

  circlePath px py pointRad
  drawFillStroke fillcolor linecolor
  Cr.setSourceRGB 0.2 0.2 0.2
  showTextOn (px-textWidth-pointRad-1) (py+fontHeight*0.5) outname
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
