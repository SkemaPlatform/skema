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
{-# LANGUAGE OverloadedStrings #-}
module Skema.Editor.SkemaDoc( 
  -- Types
  SDKernelID, SDNodeID, SkemaDoc(..), NodeArrow(..), Kernel(..), Node(..), 
  SelectedElement(..), IOPoint(..), 
  -- SkemaDoc Functions
  skemaDocDeleteKernel, skemaDocUpdateKernel, skemaDocDeleteNode,   
  skemaDocGetKernelsAssocs, skemaDocGetNodesAssocs, 
  skemaDocSetNodesAssocs, skemaDocInsertKernel,
  -- Node Functions
  nodePosx, nodePosy, nodeHeight, nodeWidth, nodePointRad, nodeHeadHeight,
  nodeHeadColor, nodeName, nodeInputPoints, nodeOutputPoints, nodeConstBuffers,
  nodeIOPPosition, nodeConstPosition, nodeKernel, nodeTranslate, 
  -- Other Functions
  arrowPosition, findInputArrow, deleteArrow, selectNodeElement, insertNewArrow, 
  emptySkemaDoc, extractProgramFlow, isOutputPoint, isIOPoint, isInputPoint, 
  minimalKernel, arrowIOPointType,
  ) where

-- -----------------------------------------------------------------------------
import Data.Maybe( fromJust, isJust, mapMaybe )
import Data.List( partition, find )
import Control.Applicative( pure, (<$>), (<*>) )
import Control.Monad( msum, liftM )
import Control.Arrow( (&&&), second )
import qualified Data.IntMap as MI( 
  IntMap, empty, lookup, elems, assocs, fromList, insert, keys, delete, 
  filter )
import qualified Data.Map as M( Map, fromList, assocs, empty )
import Data.Aeson( 
  Value(..), FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), (.!=) )
import Data.Aeson.Types( typeMismatch )
import Skema.Editor.Types( 
  Pos2D(..), RGBColor, Rect(..), Circle(..), inside, posx, posy )
import Skema.Types( IOPointType(..), IOPointDataType(..), isSameBaseType )
import Skema.ProgramFlow( 
  ProgramFlow(..), PFKernel(..), PFNode(..), PFIOPoint(..), PFArrow(..), 
  PFConstBuffer(..),
  emptyProgramFlow )
import Skema.Util( isAcyclicGraph )
import Skema.SIDMap( 
  SID(..), SIDMap, sidMapAssocs, sidMapFromList, sidMapLookup )

-- -----------------------------------------------------------------------------
newtype SDKernelID = SDKernelID Int deriving( Show, Eq )
instance SID SDKernelID where
  toInt (SDKernelID a) = a
  fromInt = SDKernelID
  
newtype SDNodeID = SDNodeID Int deriving( Show, Eq )
instance SID SDNodeID where
  toInt (SDNodeID a) = a
  fromInt = SDNodeID  

data Node = NodeKernel
    { position :: !Pos2D
    , kernelIdx :: ! SDKernelID }
          deriving( Show )

-- -----------------------------------------------------------------------------
nodePointRad :: Node -> Double
nodePointRad = const 4

nodePosx :: Node -> Double
nodePosx = posx . position

nodePosy :: Node -> Double
nodePosy = posy . position

nodeHeight :: Node -> Double
nodeHeight = const 80

nodeWidth :: Node -> Double
nodeWidth = const 60

nodeIOPPosition :: Node -> IOPoint -> Int -> Pos2D
nodeIOPPosition node point idx
    | isInputPoint point = nodeInputPosition node idx
    | otherwise = nodeOutputPosition node idx

nodeInputPosition :: Node -> Int -> Pos2D
nodeInputPosition node idx = Pos2D (px,py)
    where
      idxOffset = fromIntegral idx *2*(nodePointRad node + 1)
      px = nodePosx node
      py = nodePosy node + nodeHeight node - 10 - idxOffset

nodeOutputPosition :: Node -> Int -> Pos2D
nodeOutputPosition node idx = Pos2D (px,py)
    where
      idxOffset = fromIntegral idx *2*(nodePointRad node + 1)
      px = nodePosx node + nodeWidth node
      py = nodePosy node + nodeHeadHeight node + 10 + idxOffset

nodeConstPosition :: Node -> Int -> Pos2D
nodeConstPosition node idx = Pos2D (px,py)
    where
      idxOffset = fromIntegral idx *2*(nodePointRad node)
      px = nodePosx node + 20 + idxOffset
      py = nodePosy node + nodeHeight node

nodeHeadHeight :: Node -> Double
nodeHeadHeight = const 12

nodeTranslate :: Pos2D -> Node -> Node
nodeTranslate dpos node = node { position = npos }
    where
      npos = position node + dpos

nodeHeadColor :: Node -> RGBColor
nodeHeadColor = const (0.51,0.51,0.56)

-- -----------------------------------------------------------------------------
data IOPoint = IOPoint
    { iopName :: !String 
    , iopDataType :: !IOPointDataType
    , iopType :: !IOPointType }
    deriving( Show, Eq )

isInputPoint :: IOPoint -> Bool
isInputPoint = (==InputPoint) . iopType

isOutputPoint :: IOPoint -> Bool
isOutputPoint = (==OutputPoint) . iopType

-- -----------------------------------------------------------------------------
data Kernel = Kernel
              { name :: String
              , body :: String
              , extra :: String
              , iopoints :: MI.IntMap IOPoint
              , constBuffers :: M.Map String IOPointDataType
                -- ^ list of const buffers
              , workItems :: Maybe Int
  }
            deriving( Show, Eq )

emptyKernel :: Kernel
emptyKernel = Kernel "" "" "" MI.empty M.empty Nothing

minimalKernel :: SIDMap Kernel -> Kernel
minimalKernel kns = emptyKernel { 
  body="int id = get_global_id(0);\n",
  extra="",
  name=newName,
  iopoints=MI.fromList $ zip [0..] 
           [IOPoint "x" IOfloat InputPoint, 
            IOPoint "y" IOfloat OutputPoint]}
  where
    newName = head $ dropWhile (`elem` oldNames) wantedNames
    oldNames = map name $ MI.elems kns
    wantedNames = zipWith (++) (repeat "NewKernel") (map show ([0..]::[Int]))

-- -----------------------------------------------------------------------------
data SelectedElement = SeNODE ! SDNodeID
                     | SeIOP { seIOPNode :: ! SDNodeID
                             , seIOPPoint :: !Int }
                       deriving( Show, Eq )

isIOPoint :: SelectedElement -> Bool
isIOPoint (SeIOP _ _) = True
isIOPoint _ = False

selectNodeElement :: Pos2D -> (SDNodeID,Node,Maybe Kernel) -> Maybe SelectedElement
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

selectPoints :: Double -> Double -> Node -> [(Int,IOPoint)] -> Maybe Int
selectPoints mx my node xs = msum . map (selectPoint mx my node) $ ys
    where
      (ins,outs) = partition (isInputPoint.snd) xs
      ys = zip [0..] ins ++ zip [0..] outs

selectPoint :: Double -> Double -> Node -> (Int,(Int,IOPoint)) -> Maybe Int
selectPoint mx my node (idx,(j,point))
    | inside mx my (Circle (Pos2D (cx,cy)) rad) = Just j
    | otherwise = Nothing
    where
      Pos2D (cx,cy) = nodeIOPPosition node point idx
      rad = nodePointRad node

-- -----------------------------------------------------------------------------
data NodeArrow = NodeArrow 
    { outputNode :: ! SDNodeID
    , outputPoint :: !Int
    , inputNode :: ! SDNodeID
    , inputPoint :: !Int }
    deriving( Eq, Show )

-- -----------------------------------------------------------------------------
data SkemaDoc = SkemaDoc 
    { library :: SIDMap Kernel
    , nodes :: SIDMap Node 
    , arrows :: [NodeArrow] }
    deriving( Show )

emptySkemaDoc :: SkemaDoc
emptySkemaDoc = SkemaDoc MI.empty MI.empty []

-- -----------------------------------------------------------------------------
skemaDocGetKernelsAssocs :: SkemaDoc -> [(SDKernelID,Kernel)]
skemaDocGetKernelsAssocs = sidMapAssocs . library

skemaDocGetNodesAssocs :: SkemaDoc -> [(SDNodeID,Node)]
skemaDocGetNodesAssocs = sidMapAssocs . nodes

skemaDocSetNodesAssocs :: SkemaDoc -> [(SDNodeID,Node)] -> SkemaDoc
skemaDocSetNodesAssocs skdoc ns = skdoc { nodes = sidMapFromList ns }

-- -----------------------------------------------------------------------------
skemaDocInsertKernel :: SkemaDoc -> Kernel -> SkemaDoc
skemaDocInsertKernel skdoc kernel = skdoc { library = MI.insert newKey kernel oldLibrary }
  where
    oldLibrary = library skdoc
    keys = MI.keys oldLibrary
    newKey = if null keys then 1 else 1 + maximum keys

skemaDocDeleteKernel :: SkemaDoc -> SDKernelID -> SkemaDoc
skemaDocDeleteKernel skdoc idx@(SDKernelID i) = skdoc { 
  library = MI.delete i $ library skdoc,
  nodes = MI.filter ((/=idx) . kernelIdx) $ nodes skdoc,
  arrows = filter checkArrow $ arrows skdoc
  }
  where
    deletedNodes = map (SDNodeID . fst) . filter ((==idx) . kernelIdx . snd) . MI.assocs $ nodes skdoc
    checkArrow arr = (outputNode arr `notElem` deletedNodes)
                     && (inputNode arr `notElem` deletedNodes)

skemaDocUpdateKernel :: SkemaDoc -> SDKernelID -> Kernel -> SkemaDoc
skemaDocUpdateKernel skdoc idx@(SDKernelID i) krn = maybe skdoc changeOldKernel maybeOldKrn
  where
    maybeOldKrn = MI.lookup i $ library skdoc
    affectedNodes = map (SDNodeID . fst) . filter ((==idx) . kernelIdx . snd) . MI.assocs $ nodes skdoc
    changeOldKernel oldKrn = skdoc {
      library = MI.insert i krn (MI.delete i $ library skdoc),
      arrows = if iopoints oldKrn /= iopoints krn
               then filter checkArrow $ arrows skdoc 
               else arrows skdoc
      }
    checkArrow arr = (outputNode arr `notElem` affectedNodes) 
                     && (inputNode arr `notElem` affectedNodes)

nodeName :: SkemaDoc -> Node -> String
nodeName skdoc node = maybe "*noname*" name maybeKernel
    where
      SDKernelID idx = kernelIdx node
      maybeKernel = MI.lookup idx (library skdoc) 

skemaDocDeleteNode :: SkemaDoc -> SDNodeID -> SkemaDoc
skemaDocDeleteNode skdoc idx@(SDNodeID i) = skdoc { 
  nodes = MI.delete i $ nodes skdoc,
  arrows = filter checkArrow $ arrows skdoc
  }
  where
    checkArrow arr = (outputNode arr /= idx) && (inputNode arr /= idx)

nodeIOPoints :: SkemaDoc -> Node -> [IOPoint]
nodeIOPoints skdoc node = maybe [] (MI.elems.iopoints) maybeKernel
  where
    maybeKernel = sidMapLookup (kernelIdx node) (library skdoc) 

nodeInputPoints :: SkemaDoc -> Node -> [IOPoint]
nodeInputPoints skdoc = filter isInputPoint . nodeIOPoints skdoc

nodeOutputPoints :: SkemaDoc -> Node -> [IOPoint]
nodeOutputPoints skdoc = filter (not.isInputPoint) . nodeIOPoints skdoc

nodeConstBuffers :: SkemaDoc -> Node -> [(String, IOPointDataType)]
nodeConstBuffers skdoc node = maybe [] (M.assocs . constBuffers) maybeKernel
  where
    maybeKernel = sidMapLookup (kernelIdx node) (library skdoc)     

nodeKernel :: SkemaDoc -> Node -> Maybe Kernel
nodeKernel skdoc node = sidMapLookup (kernelIdx node) $ library skdoc

sortedIndex :: IOPoint -> Int -> [(Int,IOPoint)] -> Maybe Int
sortedIndex iop idx xs = liftM fst $ find findfun $ zip [0..] sames
    where 
      findfun = (==idx).fst.snd
      sames = filter (\(_,b)-> iopType b == iopType iop) xs

lookupIOPoint :: SkemaDoc -> SDNodeID -> Int -> Maybe IOPoint
lookupIOPoint skdoc nidx ioidx = do
  node <- sidMapLookup nidx (nodes skdoc)
  kernel <- sidMapLookup (kernelIdx node) (library skdoc)
  MI.lookup ioidx (iopoints kernel)

arrowPosition :: SkemaDoc -> SDNodeID -> Int -> Maybe Pos2D
arrowPosition skdoc nidx ioidx = do
  node <- sidMapLookup nidx (nodes skdoc)
  kernel <- sidMapLookup (kernelIdx node) (library skdoc)
  iop <- MI.lookup ioidx (iopoints kernel)
  iopPos <- sortedIndex iop ioidx (MI.assocs.iopoints $ kernel)
  return $ nodeIOPPosition node iop iopPos

arrowIOPointType :: SkemaDoc -> SDNodeID -> Int -> Maybe IOPointType
arrowIOPointType skdoc nidx ioidx = fmap iopType 
                                    $ lookupIOPoint skdoc nidx ioidx

arrowIOPointDataType :: SkemaDoc -> SDNodeID -> Int -> Maybe IOPointDataType
arrowIOPointDataType skdoc nidx ioidx = fmap iopDataType 
                                        $ lookupIOPoint skdoc nidx ioidx

insertValidArrow :: SkemaDoc -> NodeArrow -> SkemaDoc
insertValidArrow skdoc narrow = skdoc { arrows = narrow : oldArrows }
    where
      oldArrows = arrows skdoc

insertNewArrow :: SkemaDoc -> SDNodeID -> Int -> SDNodeID -> Int -> SkemaDoc
insertNewArrow skdoc ki ji kf jf 
    | validArrow skdoc ki ji kf jf = maybe skdoc (insertValidArrow skdoc) narrow
    | otherwise = skdoc
    where 
      narrow = createArrow skdoc ki ji kf jf

isSameArrow :: SDNodeID -> Int -> SDNodeID -> Int -> NodeArrow -> Bool
isSameArrow pn0 pp0 pn1 pp1 (NodeArrow inode ipoint enode epoint) 
    | (inode==pn0) && (ipoint==pp0) && (enode==pn1) && (epoint==pp1) = True
    | (inode==pn1) && (ipoint==pp1) && (enode==pn0) && (epoint==pp0) = True
    | otherwise = False

isSameArrowPoint :: SDNodeID -> Int -> SDNodeID -> Int -> NodeArrow -> Bool
isSameArrowPoint pn0 pp0 pn1 pp1 (NodeArrow _ _ enode epoint)
    | (enode==pn0) && (epoint==pp0) = True
    | (enode==pn1) && (epoint==pp1) = True
    | otherwise = False

hasInputArrowPoint :: SDNodeID -> Int -> NodeArrow -> Bool
hasInputArrowPoint pn pp (NodeArrow _ _ enode epoint)
    | (enode==pn) && (epoint==pp) = True
    | otherwise = False

validArrow :: SkemaDoc -> SDNodeID -> Int -> SDNodeID -> Int -> Bool
validArrow skdoc ki ji kf jf 
    | ki == kf = False
    | (not . null) samepoints = False
    | not sameBase = False
    | not sameType = False
    | (not . null) samearrows = False
    | hasCycles = False
    | otherwise = True
    where
      samepoints = filter (isSameArrowPoint ki ji kf jf) (arrows skdoc)
      samearrows = filter (isSameArrow ki ji kf jf) (arrows skdoc)
      initType = arrowIOPointType skdoc ki ji
      finalType = arrowIOPointType skdoc kf jf
      initDataType = arrowIOPointDataType skdoc ki ji
      finalDataType = arrowIOPointDataType skdoc kf jf
      sameBase = isSameBaseType (fromJust initDataType) (fromJust finalDataType)
      sameType = fromJust initType /= fromJust finalType
      edges = map (outputNode &&& inputNode) . arrows $ skdoc
      ((kin,_),(kout,_)) = sortArrow (fromJust initType) (ki,ji) (fromJust finalType) (kf,jf)
      hasCycles = not . isAcyclicGraph $ ((kin,kout):edges)

createArrow :: SkemaDoc -> SDNodeID -> Int -> SDNodeID -> Int -> Maybe NodeArrow
createArrow skdoc ki ji kf jf = do
  point1 <- arrowIOPointType skdoc ki ji
  point2 <- arrowIOPointType skdoc kf jf
  let ((kin,jin),(kout,jout)) = sortArrow point1 (ki,ji) point2 (kf,jf)
  Just $ NodeArrow kin jin kout jout

sortArrow :: IOPointType -> a -> IOPointType -> a -> (a,a)
sortArrow OutputPoint initial _ end = (initial,end)
sortArrow _ initial _ end = (end,initial)

findInputArrow :: SkemaDoc -> SDNodeID -> Int -> Maybe NodeArrow
findInputArrow skdoc nidx ioidx = if (not.null) samearrows
                               then Just $ head samearrows
                               else Nothing
    where
      samearrows = filter (hasInputArrowPoint nidx ioidx) (arrows skdoc)

deleteArrow :: SkemaDoc -> NodeArrow -> SkemaDoc
deleteArrow skdoc arr = skdoc { arrows = filteredArrows }
    where
      filteredArrows = filter (/=arr) (arrows skdoc)

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

toPFKernel :: Kernel -> PFKernel
toPFKernel kernel = PFKernel (body kernel) (extra kernel) 
                    kios kcst (workItems kernel)
    where
      kios = M.fromList . map (iopName &&& toPFIOPoint) . MI.elems . iopoints $ kernel
      kcst = fmap PFConstBuffer $ constBuffers kernel

toPFNode :: MI.IntMap Kernel -> Node -> Maybe PFNode
toPFNode kernels node = do
  kernel <- sidMapLookup (kernelIdx node) kernels
  Just $ PFNode (name kernel)

toPFIOPoint :: IOPoint -> PFIOPoint
toPFIOPoint p = PFIOPoint (iopDataType p) (iopType p)

toPFArrow :: SkemaDoc -> NodeArrow -> Maybe PFArrow
toPFArrow skdoc arrow = do
  let nidxO = outputNode arrow
      nidxI = inputNode arrow
  nodeO <- sidMapLookup nidxO (nodes skdoc)
  nodeI <- sidMapLookup nidxI (nodes skdoc)
  kernelO <- sidMapLookup (kernelIdx nodeO) (library skdoc) 
  kernelI <- sidMapLookup (kernelIdx nodeI) (library skdoc) 
  pointO <- MI.lookup (outputPoint arrow) (iopoints kernelO)
  pointI <- MI.lookup (inputPoint arrow) (iopoints kernelI)
  return $ PFArrow (fromSID nidxO, iopName pointO) (fromSID nidxI, iopName pointI)

-- -----------------------------------------------------------------------------
instance ToJSON IOPoint where
  toJSON iop = object [
    "iopName" .= iopName iop,
    "iopDataType" .= iopDataType iop,
    "iopType" .= iopType iop
    ]
  
instance FromJSON IOPoint where
  parseJSON (Object v) = IOPoint <$>
                         v .: "iopName" <*>
                         v .: "iopDataType" <*>
                         v .: "iopType"
  parseJSON v = typeMismatch "IOPoint" v

instance ToJSON Kernel where
  toJSON krn = object [
    "kernelName" .= name krn, 
    "kernelBody" .= body krn,
    "kernelExtra" .= extra krn,
    "kernelIO" .= iopoints krn,
    "kernelWorkItems" .= workItems krn, 
    "kernelConst" .= constBuffers krn ]

instance FromJSON Kernel where
  parseJSON (Object v) = Kernel <$>
                         v .: "kernelName" <*>
                         v .: "kernelBody" <*>
                         (v .:? "kernelExtra" .!= "") <*>
                         v .: "kernelIO" <*>
                         (v .:? "kernelConst" .!= M.empty) <*>
                         v .: "kernelWorkItems"
  parseJSON v = typeMismatch "Kernel" v

instance ToJSON Node where
  toJSON node = object [
    "position" .= position node,
    "kernel" .= (toInt . kernelIdx) node
    ]

instance FromJSON Node where
  parseJSON (Object v) = do
    pos <- v .: "position"
    kid <- v .: "kernel"
    pure $ NodeKernel pos (fromInt kid)
  parseJSON v = typeMismatch "Node" v

instance ToJSON NodeArrow where
  toJSON arr = object [
    "outputNode" .= (toInt . outputNode) arr,
    "outputPoint" .= outputPoint arr,
    "inputNode" .= (toInt . inputNode) arr,
    "inputPoint" .= inputPoint arr
    ]
               
instance FromJSON NodeArrow where
  parseJSON (Object v) = do
    onode <- v .: "outputNode"
    opoint <- v .: "outputPoint"
    inode <- v .: "inputNode"
    ipoint <- v .: "inputPoint"
    pure $ NodeArrow (fromInt onode) opoint (fromInt inode) ipoint
  parseJSON v = typeMismatch "NodeArrow" v

instance ToJSON SkemaDoc where
  toJSON doc = object [
    "kernels" .= library doc,
    "nodes" .= nodes doc,
    "arrows" .= arrows doc
    ]

instance FromJSON SkemaDoc where
  parseJSON (Object v) = SkemaDoc <$>
                         v .: "kernels" <*>
                         v .: "nodes" <*>
                         v .: "arrows"  
  parseJSON v = typeMismatch "SkemaDoc" v

-- -----------------------------------------------------------------------------
