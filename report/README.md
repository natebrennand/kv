
# Kayvee



## Building

The makefile will fulfill all the build steps.
`pdflatex` is a required dependency.

```bash
make
```



## Citations


Search for the paper here: http://dblp.uni-trier.de/db/

Paste the bibtex into `bibliography.bib`.




## Images

Include a picture with the following command:

```latex
\begin{figure*}[ht]
  \centering
  \caption{CAPTION}
  \includegraphics[width=1.0\textwidth]{images/<FILENAME (no extension)>}
\end{figure*}
```

Remove the `*` if you wish to keep the picture within a single column.



