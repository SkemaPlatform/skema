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
module Skema.Editor.Util( 
  roundedRectanglePath, drawLine, circlePath, drawFill, drawFillStroke, 
  showTextOn, calcFontHeight )
       where

-- -----------------------------------------------------------------------------
import qualified Graphics.Rendering.Cairo as Cr( 
  Render, setSourceRGB, moveTo, newPath, closePath, arc, showText, stroke, fill, 
  fillPreserve, textExtents, textExtentsHeight, setLineWidth )
import Skema.Editor.Types( RGBColor )
import Skema.Math( deg2rad )

-- -----------------------------------------------------------------------------
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

drawLine :: Double -- ^ width
         -> RGBColor -- ^ fill color
         -> Cr.Render ()
drawLine w (rf,gf,bf) = do
  Cr.setLineWidth w
  Cr.setSourceRGB rf gf bf
  Cr.stroke

circlePath :: Double -- ^ x position
           -> Double -- ^ y position
           -> Double -- ^ radius
           -> Cr.Render ()
circlePath px py rad = Cr.newPath >> Cr.arc px py rad (deg2rad 0) (deg2rad 360)

drawFill :: RGBColor -- ^ fill color
         -> Cr.Render ()
drawFill (rf,gf,bf) = do
  Cr.setSourceRGB rf gf bf
  Cr.fill

drawFillStroke :: RGBColor -- ^ fill color
               -> RGBColor -- ^ line color
               -> Cr.Render ()
drawFillStroke (rf,gf,bf) (rl,gl,bl) = do
  Cr.setSourceRGB rf gf bf
  Cr.fillPreserve
  Cr.setSourceRGB rl gl bl
  Cr.stroke

showTextOn :: Double -- ^ x position
           -> Double -- ^ y position
           -> String -- ^ text
           -> Cr.Render ()
showTextOn px py text = Cr.moveTo px py >> Cr.showText text

calcFontHeight :: Cr.Render Double
calcFontHeight = do
  textSize <- Cr.textExtents "m"
  return $ Cr.textExtentsHeight textSize

-- -----------------------------------------------------------------------------
