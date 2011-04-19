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
module Skema.ProgramFlow
    ( PFIOPoint(..), PFKernel(..), ProgramFlow(..), PFNode(..)
    , emptyProgramFlow ) where
\end{code}

\begin{code}
import qualified Data.IntMap as M( IntMap, empty )
import Text.JSON( Result(..), JSON(..), JSValue(..), toJSString, makeObj )
import Skema.Types( IOPointType(..) )
\end{code}

\begin{code}
data PFIOPoint = PFIOPoint
    { pfIOPName :: !String
    , pfIOPType :: !IOPointType }
    deriving( Show )
\end{code}

\begin{code}
instance JSON IOPointType where
    showJSON InputPoint = showJSON "InputPoint"
    showJSON OutputPoint = showJSON "OutputPoint"
    readJSON _ = Error "not implemented"
\end{code}

\begin{code}
instance JSON PFIOPoint where
    showJSON pfiop = makeObj 
                     [ ("name", showJSON . pfIOPName $ pfiop)
                     , ("type", showJSON . pfIOPType $ pfiop)]
    readJSON _ = Error "not implemented"
\end{code}

\begin{code}
data PFKernel = PFKernel
    { pfkName :: !String 
    , pfkBody :: !String
    , pfkIOPoints :: M.IntMap PFIOPoint }
    deriving( Show )
\end{code}

\begin{code}
instance JSON PFKernel where
    showJSON pfk = makeObj 
                   [ ("name", showJSON . pfkName $ pfk)
                   , ("body", showJSON . pfkBody $ pfk)
                   , ("io", showJSON . pfkIOPoints $ pfk)]
    readJSON _ = Error "not implemented"
\end{code}

\begin{code}
data PFNode = PFNode
    { pfnIndex :: !Int }
    deriving( Show )
\end{code}

\begin{code}
instance JSON PFNode where
    showJSON pfn = makeObj
                   [ ("idx", showJSON . pfnIndex $ pfn )]
    readJSON _ = Error "not implemented"
\end{code}

\begin{code}
data ProgramFlow = ProgramFlow
    { pfKernels :: M.IntMap PFKernel
    , pfNodes :: M.IntMap PFNode }
    deriving( Show )
\end{code}

\begin{code}
instance JSON ProgramFlow where
    showJSON pfn = makeObj
                   [ ("kernels", showJSON . pfKernels $ pfn)
                   , ("nodes", showJSON . pfNodes $ pfn)]
    readJSON _ = Error "not implemented"
\end{code}

\begin{code}
emptyProgramFlow :: ProgramFlow
emptyProgramFlow = ProgramFlow M.empty M.empty
\end{code}
