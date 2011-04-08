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
module Skema.SkemaDoc where
\end{code}

\begin{code}
import qualified Data.Map as Map( Map, empty, lookup )
import Skema.Util( Rect(..), inside )
\end{code}

\begin{code}
data Position = Position 
    { posx :: !Double
    , posy :: !Double }
\end{code}

\begin{code}
data Node = NodeKernel
    { position :: !Position
    , kernelIdx :: !Int }
    | NodeInt !Int
\end{code}

\begin{code}
nodeName :: SkemaDoc -> Node -> String
nodeName skdoc node = maybe "*noname*" name maybeKernel
    where
      maybeKernel = Map.lookup (kernelIdx node) (library skdoc) 
\end{code}

\begin{code}
nodePointRad :: Node -> Double
nodePointRad = const 4
\end{code}

\begin{code}
nodePosx :: Node -> Double
nodePosx = posx . position
\end{code}

\begin{code}
nodePosy :: Node -> Double
nodePosy = posy . position
\end{code}

\begin{code}
nodeHeight :: Node -> Double
nodeHeight = const 80
\end{code}

\begin{code}
nodeWidth :: Node -> Double
nodeWidth = const 60
\end{code}

\begin{code}
nodeHeadHeight :: Node -> Double
nodeHeadHeight = const 12
\end{code}

\begin{code}
nodeMoveTo :: Double -> Double -> Node -> Node
nodeMoveTo nx ny node = node { position = Position nx ny }
\end{code}

\begin{code}
nodeTranslate :: Double -> Double -> Node -> Node
nodeTranslate dx dy node = node { position = Position nx ny }
    where
      nx = nodePosx node + dx
      ny = nodePosy node + dy
\end{code}

\begin{code}
type RGBColor = (Double, Double, Double)
\end{code}

\begin{code}
nodeLineColor :: Node -> RGBColor
nodeLineColor = const (0.15,0.15,0.15)
\end{code}

\begin{code}
nodeBoxColor :: Node -> RGBColor
nodeBoxColor = const (0.59,0.59,0.59)
\end{code}

\begin{code}
nodeHeadColor :: Node -> RGBColor
nodeHeadColor = const (0.51,0.51,0.56)
\end{code}

\begin{code}
data Kernel = Kernel
    { name :: String }
\end{code}

\begin{code}
data SelectedElement = SE_NOTHING
                     | SE_NODE Int
                       deriving( Show )
\end{code}

\begin{code}
selectNode :: Double -> Double -> (Int,Node) -> SelectedElement
selectNode mx my (k,node)
    | inside mx my (Rect x0 y0 x1 y1) = SE_NODE k
    | otherwise = SE_NOTHING
    where 
      x0 = nodePosx node
      y0 = nodePosy node
      x1 = x0 + nodeWidth node
      y1 = y0 + nodeHeight node
\end{code}

\begin{code}
isSelected :: SelectedElement -> Bool
isSelected SE_NOTHING = False
isSelected _ = True
\end{code}

\begin{code}
data SkemaDoc = SkemaDoc 
    { library :: Map.Map Int Kernel
    , nodes :: Map.Map Int Node }
\end{code}

\begin{code}
emptySkemaDoc = SkemaDoc Map.empty Map.empty
\end{code}
