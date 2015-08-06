\begin{figure}

\begin{center}

<<alsace_recours, echo=FALSE, fig.height=5>>=
  main <- paste0("Titre_principal", anc)
@

\captionof{figure}{\Sexpr{main}.}\label{fig:}%
\end{center}

\end{figure}



Si on ne veut pas faire flotter

\begin{center}
\includegraphics{foo}
\captionof{figure}{caption text}
\label{fig:nonfloat}
\end{center}