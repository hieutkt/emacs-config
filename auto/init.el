(TeX-add-style-hook
 "init"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("vietnam" "utf8")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "inputenc"
    "fontenc"
    "graphicx"
    "grffile"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "textcomp"
    "amssymb"
    "capt-of"
    "hyperref"
    "vietnam")
   (LaTeX-add-labels
    "sec:org23e560a"
    "sec:org63638fc"
    "sec:orge429291"
    "sec:orgf215310"
    "sec:org6ffa110"
    "sec:orgd4123fd"
    "sec:org30a80d7"
    "sec:org0495569"
    "sec:org95bdb85"
    "sec:org5893ee7"
    "sec:orge2d5ca7"
    "sec:org62f68f0"
    "sec:org9e77466"
    "sec:org938d6f2"
    "sec:org4d9d70e"
    "sec:orgc235fbe"
    "sec:org43a5ee7"
    "sec:org81f5ecc"
    "sec:orgf3e9e5d"
    "sec:org7862d04"))
 :latex)

