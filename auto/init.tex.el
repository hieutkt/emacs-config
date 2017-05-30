(TeX-add-style-hook
 "init.tex"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("vietnam" "utf8")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
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
    "sec:org690d6c7"
    "sec:org2bc431c"
    "sec:orga1406a5"
    "sec:org07d6173"
    "sec:org3138334"
    "sec:org7067328"
    "sec:orgc5265f3"
    "sec:org9b006be"
    "sec:orgae9be09"
    "sec:orgea9703b"
    "sec:org4de48f9"
    "sec:orgf80261e"
    "sec:org7f2fc02"
    "sec:org426cdea"
    "sec:org6786810"
    "sec:org284bdbd"
    "sec:org7762497"
    "sec:org5623759"
    "sec:orgf27e1e9"
    "sec:org99d5d21"
    "sec:orge983708"
    "sec:org8f7830e"
    "sec:org4b41f0c"
    "sec:org0024228"
    "sec:org6b0ddb3"))
 :latex)

