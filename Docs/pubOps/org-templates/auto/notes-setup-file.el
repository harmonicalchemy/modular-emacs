(TeX-add-style-hook
 "notes-setup-file"
 (lambda ()
   (TeX-add-symbols
    "oldsection"
    "oldparagraph"
    "oldsubparagraph")
   (LaTeX-add-environments
    '("vplace" LaTeX-env-args ["argument"] 0)
    '("margintable" LaTeX-env-args ["argument"] 0)
    '("marginfigure" LaTeX-env-args ["argument"] 0)
    '("mem@margin@float" LaTeX-env-args ["argument"] 1)
    '("verse" LaTeX-env-args ["argument"] 0)))
 :plain-tex)

