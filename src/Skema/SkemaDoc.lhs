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
data Position = Position 
    { posx :: !Double
    , posy :: !Double }
\end{code}

\begin{code}
data VisualNode = VisualNode
    { position :: !Position }
\end{code}

\begin{code}
nodePointRad :: VisualNode -> Double
nodePointRad = const 4
\end{code}

\begin{code}
currentHeight :: VisualNode -> Double
currentHeight = const 80
\end{code}

\begin{code}
currentWidth :: VisualNode -> Double
currentWidth = const 60
\end{code}

\begin{code}
type RGBColor = (Double, Double, Double)
\end{code}

\begin{code}
nodeLineColor :: VisualNode -> RGBColor
nodeLineColor = const (0.15,0.15,0.15)
\end{code}

\begin{code}
nodeBoxColor :: VisualNode -> RGBColor
nodeBoxColor = const (0.59,0.59,0.59)
\end{code}

\begin{code}
nodeHeadColor :: VisualNode -> RGBColor
nodeHeadColor = const (0.51,0.51,0.56)
\end{code}

\begin{code}
data SkemaDoc = SkemaDoc 
    { nodes :: [VisualNode] }
\end{code}
