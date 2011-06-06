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
import Data.Maybe( fromJust, isJust, mapMaybe )
import Data.List( partition, find )
import Control.Monad( msum, liftM )
import Control.Arrow( (&&&), second )
import qualified Data.IntMap as MI
    ( IntMap, empty, lookup, elems, assocs, fromList )
import qualified Data.Map as M( fromList )
import Skema.Util
    ( Pos2D(..), RGBColor, Rect(..), Circle(..), inside, posx, posy )
import Skema.Types
  ( IOPointType(..), IOPointDataType(..) )
import Skema.ProgramFlow
    ( ProgramFlow(..), PFKernel(..), PFNode(..), PFIOPoint(..), PFArrow(..)
    , emptyProgramFlow )
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
      idxOffset = fromIntegral idx *2*(nodePointRad node + 1)
      px = nodePosx node
      py = nodePosy node + nodeHeight node - 10 - idxOffset
\end{code}

\begin{code}
nodeOutputPosition :: Node -> Int -> Pos2D
nodeOutputPosition node idx = Pos2D (px,py)
    where
      idxOffset = fromIntegral idx *2*(nodePointRad node + 1)
      px = nodePosx node + nodeWidth node
      py = nodePosy node + nodeHeadHeight node + 10 + idxOffset
\end{code}

\begin{code}
nodeHeadHeight :: Node -> Double
nodeHeadHeight = const 12
\end{code}

\begin{code}
nodeMoveTo :: Pos2D -> Node -> Node
nodeMoveTo npos node = node { position = npos }
\end{code}

\begin{code}
nodeTranslate :: Pos2D -> Node -> Node
nodeTranslate dpos node = node { position = npos }
    where
      npos = position node + dpos
\end{code}

\begin{code}
nodeHeadColor :: Node -> RGBColor
nodeHeadColor = const (0.51,0.51,0.56)
\end{code}

\begin{code}
data IOPoint = IOPoint
    { iopName :: !String 
    , iopDataType :: !IOPointDataType
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
    , body :: String
    , iopoints :: MI.IntMap IOPoint }
    deriving( Show )
\end{code}

\begin{code}
emptyKernel :: Kernel
emptyKernel = Kernel "" "" MI.empty
\end{code}

\begin{code}
data SelectedElement = SeNODE !Int
                     | SeIOP { seIOPNode :: !Int
                             , seIOPPoint :: !Int }
                       deriving( Show, Eq )
\end{code}

\begin{code}
isIOPoint :: SelectedElement -> Bool
isIOPoint (SeIOP _ _) = True
isIOPoint _ = False
\end{code}

\begin{code}
selectNodeElement :: Pos2D -> (Int,Node,Maybe Kernel) -> Maybe SelectedElement
selectNodeElement (Pos2D (mx,my)) (k,node,maybeKernel)
    | isFullSelected && isJust pointSelected = Just $ SeIOP k (fromJust pointSelected)
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
      points = maybe [] (MI.assocs.iopoints) maybeKernel
\end{code}

\begin{code}
selectPoints :: Double -> Double -> Node -> [(Int,IOPoint)] -> Maybe Int
selectPoints mx my node xs = msum . map (selectPoint mx my node) $ ys
    where
      (ins,outs) = partition (isInputPoint.snd) xs
      ys = zip [0..] ins ++ zip [0..] outs
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
    deriving( Eq, Show )
\end{code}

\begin{code}
data SkemaDoc = SkemaDoc 
    { library :: MI.IntMap Kernel
    , nodes :: MI.IntMap Node 
    , arrows :: [NodeArrow] }
\end{code}

\begin{code}
emptySkemaDoc :: SkemaDoc
emptySkemaDoc = SkemaDoc MI.empty MI.empty []
\end{code}

\begin{code}
nodeName :: SkemaDoc -> Node -> String
nodeName skdoc node = maybe "*noname*" name maybeKernel
    where
      maybeKernel = MI.lookup (kernelIdx node) (library skdoc) 
\end{code}

\begin{code}
nodeIOPoints :: SkemaDoc -> Node -> [IOPoint]
nodeIOPoints skdoc node = maybe [] (MI.elems.iopoints) maybeKernel
    where
      maybeKernel = MI.lookup (kernelIdx node) (library skdoc) 
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
nodeKernel skdoc node = MI.lookup (kernelIdx node) (library skdoc) 
\end{code}

\begin{code}
sortedIndex :: IOPoint -> Int -> [(Int,IOPoint)] -> Maybe Int
sortedIndex iop idx xs = liftM fst $ find findfun $ zip [0..] sames
    where 
      findfun = (==idx).fst.snd
      sames = filter (\(_,b)-> iopType b == iopType iop) xs
\end{code}

\begin{code}
selectedPosition :: SkemaDoc -> SelectedElement -> Pos2D
selectedPosition _ _ = Pos2D (0,0)
\end{code}

\begin{code}
arrowPosition :: SkemaDoc -> Int -> Int -> Maybe Pos2D
arrowPosition skdoc nidx ioidx = do
  node <- MI.lookup nidx (nodes skdoc)
  kernel <- MI.lookup (kernelIdx node) (library skdoc)
  iop <- MI.lookup ioidx (iopoints kernel)
  iopPos <- sortedIndex iop ioidx (MI.assocs.iopoints $ kernel)
  return $ nodeIOPPosition node iop iopPos
\end{code}

\begin{code}
arrowIOPointType :: SkemaDoc -> Int -> Int -> Maybe IOPointType
arrowIOPointType skdoc nidx ioidx = do
  node <- MI.lookup nidx (nodes skdoc)
  kernel <- MI.lookup (kernelIdx node) (library skdoc)
  iop <- MI.lookup ioidx (iopoints kernel)
  return $ iopType iop
\end{code}

\begin{code}
insertValidArrow :: SkemaDoc -> NodeArrow -> SkemaDoc
insertValidArrow skdoc narrow = skdoc { arrows = narrow : oldArrows }
    where
      oldArrows = arrows skdoc
\end{code}

\begin{code}
insertNewArrow :: SkemaDoc -> Int -> Int -> Int -> Int -> SkemaDoc
insertNewArrow skdoc ki ji kf jf 
    | validArrow skdoc ki ji kf jf = maybe skdoc (insertValidArrow skdoc) narrow
    | otherwise = skdoc
    where 
      narrow = createArrow skdoc ki ji kf jf
\end{code}

\begin{code}
isSameArrow :: Int -> Int -> Int -> Int -> NodeArrow -> Bool
isSameArrow pn0 pp0 pn1 pp1 (NodeArrow inode ipoint enode epoint) 
    | (inode==pn0) && (ipoint==pp0) && (enode==pn1) && (epoint==pp1) = True
    | (inode==pn1) && (ipoint==pp1) && (enode==pn0) && (epoint==pp0) = True
    | otherwise = False
\end{code}

\begin{code}
isSameArrowPoint :: Int -> Int -> Int -> Int -> NodeArrow -> Bool
isSameArrowPoint pn0 pp0 pn1 pp1 (NodeArrow _ _ enode epoint)
    | (enode==pn0) && (epoint==pp0) = True
    | (enode==pn1) && (epoint==pp1) = True
    | otherwise = False
\end{code}

\begin{code}
hasInputArrowPoint :: Int -> Int -> NodeArrow -> Bool
hasInputArrowPoint pn pp (NodeArrow _ _ enode epoint)
    | (enode==pn) && (epoint==pp) = True
    | otherwise = False
\end{code}

\begin{code}
validArrow :: SkemaDoc -> Int -> Int -> Int -> Int -> Bool
validArrow skdoc ki ji kf jf 
    | ki == kf = False
    | (not . null) samepoints = False
    | isJust initType && isJust finalType = fromJust initType /= fromJust finalType
    | (not . null) samearrows = False
    | otherwise = True
    where
      samepoints = filter (isSameArrowPoint ki ji kf jf) (arrows skdoc)
      samearrows = filter (isSameArrow ki ji kf jf) (arrows skdoc)
      initType = arrowIOPointType skdoc ki ji
      finalType = arrowIOPointType skdoc kf jf
\end{code}

\begin{code}
createArrow :: SkemaDoc -> Int -> Int -> Int -> Int -> Maybe NodeArrow
createArrow skdoc ki ji kf jf = do
  point1 <- arrowIOPointType skdoc ki ji
  point2 <- arrowIOPointType skdoc kf jf
  let ((kin,jin),(kout,jout)) = sortArrow point1 (ki,ji) point2 (kf,jf)
  Just $ NodeArrow kin jin kout jout
\end{code}

\begin{code}
sortArrow :: IOPointType -> a -> IOPointType -> a -> (a,a)
sortArrow OutputPoint initial _ end = (initial,end)
sortArrow _ initial _ end = (end,initial)
\end{code}

\begin{code}
findInputArrow :: SkemaDoc -> Int -> Int -> Maybe NodeArrow
findInputArrow skdoc nidx ioidx = if (not.null) samearrows
                               then Just $ head samearrows
                               else Nothing
    where
      samearrows = filter (hasInputArrowPoint nidx ioidx) (arrows skdoc)
\end{code}

\begin{code}
deleteArrow :: SkemaDoc -> NodeArrow -> SkemaDoc
deleteArrow skdoc arr = skdoc { arrows = filteredArrows }
    where
      filteredArrows = filter (/=arr) (arrows skdoc)
\end{code}

\begin{code}
extractProgramFlow :: SkemaDoc -> ProgramFlow
extractProgramFlow skdoc = emptyProgramFlow
                           { pfKernels = dkernels
                           , pfNodes = dnodes
                           , pfArrows = darrows
                           }
    where
      dkernels = M.fromList . map (name &&& toPFKernel) . MI.elems . library $ skdoc
      dnodes = MI.fromList . map (second fromJust) . filter (isJust . snd ) . map (second $ toPFNode (library skdoc)) . MI.assocs . nodes $ skdoc
      darrows = mapMaybe (toPFArrow skdoc) . arrows $ skdoc
\end{code}

\begin{code}
toPFKernel :: Kernel -> PFKernel
toPFKernel kernel = PFKernel (body kernel) kios
    where
      kios = M.fromList . map (iopName &&& toPFIOPoint) . MI.elems . iopoints $ kernel
\end{code}

\begin{code}
toPFNode :: MI.IntMap Kernel -> Node -> Maybe PFNode
toPFNode kernels node = do
  kernel <- MI.lookup (kernelIdx node) kernels
  Just $ PFNode (name kernel)
\end{code}

\begin{code}
toPFIOPoint :: IOPoint -> PFIOPoint
toPFIOPoint p = PFIOPoint (iopDataType p) (iopType p)
\end{code}

\begin{code}
toPFArrow :: SkemaDoc -> NodeArrow -> Maybe PFArrow
toPFArrow skdoc arrow = do
  nodeO <- MI.lookup (outputNode arrow) (nodes skdoc)
  nodeI <- MI.lookup (inputNode arrow) (nodes skdoc)
  kernelO <- MI.lookup (kernelIdx nodeO) (library skdoc) 
  kernelI <- MI.lookup (kernelIdx nodeI) (library skdoc) 
  pointO <- MI.lookup (outputPoint arrow) (iopoints kernelO)
  pointI <- MI.lookup (inputPoint arrow) (iopoints kernelI)
  return $ PFArrow (outputNode arrow, iopName pointO) (inputNode arrow, iopName pointI)
\end{code}
