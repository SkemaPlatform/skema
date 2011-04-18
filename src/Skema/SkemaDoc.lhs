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
import Data.List( partition, find )
import Control.Monad( msum, liftM )
import qualified Data.IntMap as M( IntMap, empty, lookup, elems, assocs )
import Skema.Util
    ( Pos2D(..), RGBColor, Rect(..), Circle(..), inside, posx, posy )
\end{code}

\begin{code}
data Node = NodeKernel
    { position :: !Pos2D
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
nodeIOPPosition :: Node -> IOPoint -> Int -> Pos2D
nodeIOPPosition node point idx
    | isInputPoint point = nodeInputPosition node idx
    | otherwise = nodeOutputPosition node idx
\end{code}

\begin{code}
nodeInputPosition :: Node -> Int -> Pos2D
nodeInputPosition node idx = Pos2D (px,py)
    where
      idxOffset = (fromIntegral idx)*2*(nodePointRad node + 1)
      px = nodePosx node
      py = (nodePosy node) + (nodeHeight node) - 10 - idxOffset
\end{code}

\begin{code}
nodeOutputPosition :: Node -> Int -> Pos2D
nodeOutputPosition node idx = Pos2D (px,py)
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
nodeMoveTo nx ny node = node { position = Pos2D (nx, ny) }
\end{code}

\begin{code}
nodeTranslate :: Double -> Double -> Node -> Node
nodeTranslate dx dy node = node { position = Pos2D (nx, ny) }
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
selectNodeElement mx my (k,node,maybeKernel)
    | isFullSelected && (isJust pointSelected) = Just $ SeIOP k (fromJust pointSelected)
    | isFullSelected && isBodySelected = Just $ SeNODE k
    | otherwise = Nothing
    where 
      rad = nodePointRad node
      initx = nodePosx node
      inity = nodePosy node
      endx = initx + nodeWidth node
      endy = inity + nodeHeight node
      isFullSelected = inside mx my (Rect (Pos2D (initx-rad,inity)) (Pos2D (endx+2*rad,endy)))
      isBodySelected = (mx >= initx) && (mx < endx) 
      pointSelected =  selectPoints mx my node points
      points = maybe [] (M.assocs.iopoints) maybeKernel
\end{code}

\begin{code}
selectPoints :: Double -> Double -> Node -> [(Int,IOPoint)] -> Maybe Int
selectPoints mx my node xs = msum . map (selectPoint mx my node) $ ys
    where
      (ins,outs) = partition (isInputPoint.snd) xs
      ys = (zip [0..] ins) ++ (zip [0..] outs)
\end{code}

\begin{code}
selectPoint :: Double -> Double -> Node -> (Int,(Int,IOPoint)) -> Maybe Int
selectPoint mx my node (idx,(j,point))
    | inside mx my (Circle (Pos2D (cx,cy)) rad) = Just j
    | otherwise = Nothing
    where
      Pos2D (cx,cy) = nodeIOPPosition node point idx
      rad = nodePointRad node
\end{code}

\begin{code}
data NodeArrow = NodeArrow 
    { outputNode :: !Int 
    , outputPoint :: !Int
    , inputNode :: !Int
    , inputPoint :: !Int }
\end{code}

\begin{code}
data SkemaDoc = SkemaDoc 
    { library :: M.IntMap Kernel
    , nodes :: M.IntMap Node 
    , arrows :: [NodeArrow] }
\end{code}

\begin{code}
emptySkemaDoc :: SkemaDoc
emptySkemaDoc = SkemaDoc M.empty M.empty []
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

\begin{code}
sortedIndex :: IOPoint -> Int -> [(Int,IOPoint)] -> Maybe Int
sortedIndex iop idx xs = liftM fst $ find findfun $ zip [0..] sames
    where 
      findfun = ((==idx).fst.snd)
      sames = filter (\(_,b)-> (iopType b) == (iopType iop)) xs
\end{code}

\begin{code}
selectedPosition :: SkemaDoc -> SelectedElement -> Pos2D
selectedPosition _ _ = Pos2D (0,0)
\end{code}

\begin{code}
arrowPosition :: SkemaDoc -> Int -> Int -> Maybe Pos2D
arrowPosition skdoc nidx ioidx = do
  node <- M.lookup nidx (nodes skdoc)
  kernel <- M.lookup (kernelIdx node) (library skdoc)
  iop <- M.lookup ioidx (iopoints kernel)
  iopPos <- sortedIndex iop ioidx (M.assocs.iopoints $ kernel)
  return $ nodeIOPPosition node iop iopPos
\end{code}
