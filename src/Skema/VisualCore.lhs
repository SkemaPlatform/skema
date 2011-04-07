\begin{code}
module Skema.VisualCore where
\end{code}

\begin{code}
data VisualFlow = VisualFlow 
    { current :: !Int
    , nodes :: [Int] 
    , arrows :: [Int] }
\end{code}
