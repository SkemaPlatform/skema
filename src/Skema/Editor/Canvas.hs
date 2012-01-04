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
module Skema.Editor.Canvas( drawSkemaDoc, drawSelected ) where

-- -----------------------------------------------------------------------------
import Control.Monad( liftM, when )
import Data.Maybe( isJust, isJust, fromJust )
import qualified Data.IntMap as M( elems )
import qualified Graphics.Rendering.Cairo as Cr( 
  Render, FontSlant(..), FontWeight(..), setSourceRGB, setSourceRGBA, 
  setLineWidth, setFontSize, lineTo, newPath, closePath, arc, moveTo, 
  selectFontFace, fill, paint, textExtents, textExtentsWidth, curveTo, setDash )
import Skema.SkemaDoc( 
  SDKernelID, SkemaDoc(..), NodeArrow(..), Node(..), IOPoint, 
  SelectedElement(..), nodePosx, nodePosy, nodeHeight, nodeWidth, nodePointRad, 
  nodeHeadHeight, nodeHeadColor, nodeName, nodeInputPoints, nodeOutputPoints,
  nodeConstBuffers, arrowPosition, nodeIOPPosition, nodeConstPosition, 
  isInputPoint, iopName, iopDataType )
import Skema.Math( deg2rad )
import Skema.Editor.Types( Pos2D(..), RGBColor, posx, posy )
import Skema.Editor.Util( 
  roundedRectanglePath, circlePath, drawFill, drawFillStroke, showTextOn, 
  calcFontHeight, drawLine )
import Skema.Types( IOPointDataType(..) )

-- -----------------------------------------------------------------------------
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

instance ElemColor IOPointDataType where
    fillColor = const (0.78,0.16,0.78)

-- -----------------------------------------------------------------------------
drawSkemaDoc :: Double -> Double -> SkemaDoc -> Maybe SDKernelID -> Cr.Render ()
drawSkemaDoc _ _ skdoc sel = do
  Cr.setSourceRGB 0.45 0.45 0.45
  Cr.paint

  Cr.selectFontFace "arial" Cr.FontSlantNormal Cr.FontWeightNormal
  when (isJust sel) $ 
    mapM_ drawSelKernel $
    filter ((== fromJust sel).kernelIdx) . M.elems.nodes $ skdoc
  mapM_ (drawVisualNode skdoc) (M.elems.nodes $ skdoc)
  mapM_ (drawArrow skdoc) (arrows skdoc)

drawSelKernel :: Node -> Cr.Render ()
drawSelKernel node = do
  let px = nodePosx node
      py = nodePosy node
      wid = nodeWidth node
      hei = nodeHeight node
      sinc = 4

  -- light
  Cr.setSourceRGBA 1 1 1 0.4
  roundedRectanglePath (px-sinc) (py+sinc) (wid+sinc*2) hei 4
  Cr.fill

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
  mapM_ (drawConstBuffer node) (zip [0..] $ nodeConstBuffers skdoc node)

drawIOPoint :: Node -> (Int, IOPoint) -> Cr.Render ()
drawIOPoint node (ipos,point) = do
  let pointRad = nodePointRad node
      Pos2D (px,py) = nodeIOPPosition node point ipos
      name = iopName point ++ " [" ++ (show.iopDataType) point ++ "]"

  fontHeight <- calcFontHeight

  circlePath px py pointRad
  drawFillStroke (fillColor point) (lineColor point)
  Cr.setSourceRGB 0.2 0.2 0.2
  if isInputPoint point
    then showTextOn (px+6) (py+fontHeight*0.5) name
    else do
      textWidth <- liftM Cr.textExtentsWidth $ Cr.textExtents name
      showTextOn (px-textWidth-pointRad-1) (py+fontHeight*0.5) name

drawConstBuffer :: Node -> (Int, (String, IOPointDataType)) -> Cr.Render ()
drawConstBuffer node (ipos,(_,buffType)) = do
  let pointRad = nodePointRad node
      Pos2D (px,py) = nodeConstPosition node ipos
  circlePath px py pointRad
  drawFillStroke  (fillColor buffType) (lineColor buffType)
  return ()
  
drawSelected :: Double -> Double -> Maybe SelectedElement 
                -> Double -> Double -> Double -> Double -> Cr.Render ()
drawSelected _ _ Nothing _ _ _ _ = return ()
drawSelected _ _ (Just (SeIOP _ _)) ox oy mx my = do
  Cr.setDash [10,5] 0
  Cr.moveTo ox oy
  Cr.lineTo mx my
  drawLine 1 (0.7,0.7,0.7)
drawSelected _ _ _ _ _ _ _ = return ()

drawArrow :: SkemaDoc -> NodeArrow -> Cr.Render ()
drawArrow skdoc arrow = when (isJust outpos && isJust inpos) $ do
                          Cr.moveTo px0 py0
                          Cr.curveTo (px0 + 50) py0 (px3 - 50) py3 px3 py3
                          drawLine 1 (0.7,0.7,0.7)
      where
        outpos = arrowPosition skdoc (outputNode arrow) (outputPoint arrow)
        inpos = arrowPosition skdoc (inputNode arrow) (inputPoint arrow)
        px0 = posx.fromJust $ outpos
        py0 = posy.fromJust $ outpos
        px3 = posx.fromJust $ inpos
        py3 = posy.fromJust $ inpos

-- -----------------------------------------------------------------------------
