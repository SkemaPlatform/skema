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
import Data.Maybe( fromJust, isJust )
import Control.Monad( mplus, msum )
import qualified Data.IntMap as M( IntMap, empty, lookup, elems, assocs )
import Skema.Util( RGBColor, Rect(..), inside )
\end{code}

\begin{code}
data Position = Position 
    { posx :: !Double
    , posy :: !Double }
                deriving( Show )
\end{code}

\begin{code}
data Node = NodeKernel
    { position :: !Position
    , kernelIdx :: !Int }
    | NodeInt !Int
            deriving( Show )
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
nodeIOPPosition :: Node -> IOPoint -> Int -> (Double,Double)
nodeIOPPosition node point idx
    | isInputPoint point = nodeInputPosition node idx
    | otherwise = nodeOutputPosition node idx
\end{code}

\begin{code}
nodeInputPosition :: Node -> Int -> (Double, Double)
nodeInputPosition node idx = (px,py)
    where
      idxOffset = (fromIntegral idx)*2*(nodePointRad node + 1)
      px = nodePosx node
      py = (nodePosy node) + (nodeHeight node) - 10 - idxOffset
\end{code}

\begin{code}
nodeOutputPosition :: Node -> Int -> (Double, Double)
nodeOutputPosition node idx = (px,py)
    where
      idxOffset = (fromIntegral idx)*2*(nodePointRad node + 1)
      px = (nodePosx node) + (nodeWidth node)
      py = (nodePosy node) + (nodeHeadHeight node) + 10 + idxOffset
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
nodeHeadColor :: Node -> RGBColor
nodeHeadColor = const (0.51,0.51,0.56)
\end{code}

\begin{code}
data IOPointType = InputPoint
                 | OutputPoint
                   deriving( Show, Eq )

data IOPoint = IOPoint
    { iopName :: !String 
    , iopType :: !IOPointType }
    deriving( Show )
\end{code}

\begin{code}
isInputPoint :: IOPoint -> Bool
isInputPoint = (==InputPoint) . iopType
\end{code}

\begin{code}
data Kernel = Kernel
    { name :: String 
    , iopoints :: M.IntMap IOPoint }
    deriving( Show )
\end{code}

\begin{code}
data SelectedElement = SeNODE !Int
                     | SeIOP !Int !Int
                       deriving( Show, Eq )
\end{code}

\begin{code}
selectNodeElement :: Double -> Double -> (Int,Node,Maybe Kernel) -> Maybe SelectedElement
selectNodeElement mx my (k,node,kernel)
--    | isFullSelected && (isJust pointSelected) = Just $ SeIOP k (fromJust pointSelected)
    | isFullSelected && isBodySelected = Just $ SeNODE k
    | otherwise = Nothing
    where 
      rad = nodePointRad node
      initx = nodePosx node
      inity = nodePosy node
      endx = initx + nodeWidth node
      endy = inity + nodeHeight node
      isFullSelected = inside mx my (Rect (initx-rad) inity (endx+2*rad) endy)
      isBodySelected = (mx >= initx) && (mx < endx) 
      pointSelected =  selectIOPoint mx my (node,kernel)
\end{code}

\begin{code}
selectIOPoint :: Double -> Double -> (Node,Maybe Kernel) -> Maybe IOPoint
selectIOPoint _ _ (_,Nothing) = Nothing
--selectIOPoint mx my (node,Just kernel) = ipoint `mplus` opoint
--    where
--      ipoint = msum [selectPoints mx my node (M.assocs.iopoints $ kernel)]
\end{code}

\begin{code}
selectPoints :: Double -> Double -> Node -> [(Int,String)] -> Maybe IOPoint
selectPoints _ _ _ _ = msum [Nothing]
\end{code}

\begin{code}
selectPoint :: Double -> Double -> Node -> [(Int,String)] -> Maybe IOPoint
selectPoint _ _ _ _ = msum [Nothing]
\end{code}

\begin{code}
data SkemaDoc = SkemaDoc 
    { library :: M.IntMap Kernel
    , nodes :: M.IntMap Node }
\end{code}

\begin{code}
emptySkemaDoc :: SkemaDoc
emptySkemaDoc = SkemaDoc M.empty M.empty
\end{code}

\begin{code}
nodeName :: SkemaDoc -> Node -> String
nodeName skdoc node = maybe "*noname*" name maybeKernel
    where
      maybeKernel = M.lookup (kernelIdx node) (library skdoc) 
\end{code}

\begin{code}
nodeIOPoints :: SkemaDoc -> Node -> [IOPoint]
nodeIOPoints skdoc node = maybe [] (M.elems.iopoints) maybeKernel
    where
      maybeKernel = M.lookup (kernelIdx node) (library skdoc) 
\end{code}

\begin{code}
nodeInputPoints :: SkemaDoc -> Node -> [IOPoint]
nodeInputPoints skdoc = filter isInputPoint . nodeIOPoints skdoc
\end{code}

\begin{code}
nodeOutputPoints :: SkemaDoc -> Node -> [IOPoint]
nodeOutputPoints skdoc = filter (not.isInputPoint) . nodeIOPoints skdoc
\end{code}

\begin{code}
nodeKernel :: SkemaDoc -> Node -> Maybe Kernel
nodeKernel skdoc node = M.lookup (kernelIdx node) (library skdoc) 
\end{code}
