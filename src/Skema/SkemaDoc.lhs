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
data SkemaDoc = SkemaDoc 
    { nodes :: [VisualNode] }
\end{code}
