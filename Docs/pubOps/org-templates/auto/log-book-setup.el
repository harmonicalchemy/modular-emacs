(TeX-add-style-hook
 "log-book-setup"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("footmisc" "hang" "flushmargin" "stable" "multiple") ("geometry" "margin=1.25in") ("tcolorbox" "most")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "parskip"
    "calc"
    "titlesec"
    "roboto"
    "palatino"
    "footmisc"
    "manyfoot"
    "relsize"
    "breakcites"
    "biblatex"
    "geometry"
    "setspace"
    "tcolorbox")
   (LaTeX-add-environments
    '("vplace" LaTeX-env-args ["argument"] 0)
    '("margintable" LaTeX-env-args ["argument"] 0)
    '("marginfigure" LaTeX-env-args ["argument"] 0)
    '("mem@margin@float" LaTeX-env-args ["argument"] 1)
    '("verse" LaTeX-env-args ["argument"] 0))
   (LaTeX-add-xcolor-definecolors
    "block-gray"))
 :latex)

