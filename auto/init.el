(TeX-add-style-hook
 "init"
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
    "sec:org50a49ee"
    "sec:orgd46d012"
    "sec:orgc4d3cd2"
    "sec:orgfc0e3dd"
    "sec:org8e52687"
    "sec:org1631a6f"
    "sec:orgcd0c8bd"
    "sec:org49ddabe"
    "sec:org0ce0bbc"
    "sec:org20f80e2"
    "sec:org6cbb8af"
    "sec:orgfe4900a"
    "sec:org79106ff"
    "sec:org3ea002b"
    "sec:orgd776d71"
    "sec:org44a1fcb"
    "sec:org4a064f8"
    "sec:orge5b7755"
    "sec:org609035b"
    "sec:orgcb261cd"
    "sec:org6064bd5"))
 :latex)

