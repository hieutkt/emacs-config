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
    "sec:org3ef80d1"
    "sec:org9fb20ef"
    "sec:org02182ad"
    "sec:orgc3ea522"
    "sec:org271f9f0"
    "sec:orgc03d443"
    "sec:org7e571a2"
    "sec:org035855f"
    "sec:orgbbcdd10"
    "sec:orgdcd692d"
    "sec:org39877f3"
    "sec:orge205089"
    "sec:orgcc31bf0"
    "sec:orga2d7370"
    "sec:orgc7cf1c0"
    "sec:orgf1b1384"
    "sec:org496447f"
    "sec:org6ada814"
    "sec:org047d50a"
    "sec:org07a031f"
    "sec:org304f1a9"
    "sec:orge77f68c"
    "sec:org752ba4d"
    "sec:orgb2f277f"
    "sec:orgf57b66a"
    "sec:org9415def"))
 :latex)

