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
module Skema.Types( IOPointType(..), IOPointDataType(..) ) where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Text.JSON( JSON(..), Result(..), showJSON )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
data IOPointType = InputPoint
                 | OutputPoint
                   deriving( Show, Eq )
\end{code}

\begin{code}
data IOPointDataType = IOchar | IOuchar
                     | IOshort | IOushort
                     | IOint | IOuint
                     | IOlong | IOulong
                     | IOfloat
                     | IOchar2 | IOuchar2
                     | IOshort2 | IOushort2
                     | IOint2 | IOuint2
                     | IOlong2 | IOulong2
                     | IOfloat2
                     | IOchar4 | IOuchar4
                     | IOshort4 | IOushort4
                     | IOint4 | IOuint4
                     | IOlong4 | IOulong4
                     | IOfloat4
                     | IOchar8 | IOuchar8
                     | IOshort8 | IOushort8
                     | IOint8 | IOuint8
                     | IOlong8 | IOulong8
                     | IOfloat8
                     | IOchar16 | IOuchar16
                     | IOshort16 | IOushort16
                     | IOint16 | IOuint16
                     | IOlong16 | IOulong16
                     | IOfloat16
                     deriving( Show, Eq )
\end{code}

\begin{code}
instance JSON IOPointType where
    showJSON InputPoint = showJSON "InputPoint"
    showJSON OutputPoint = showJSON "OutputPoint"
    readJSON _ = Error "not implemented"
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
