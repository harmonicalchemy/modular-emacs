(TeX-add-style-hook
 "melog"
 (lambda ()
   (TeX-add-symbols
    '("printchaptertitle" 1))
   (LaTeX-add-environments
    '("vplace" LaTeX-env-args ["argument"] 0)
    '("margintable" LaTeX-env-args ["argument"] 0)
    '("marginfigure" LaTeX-env-args ["argument"] 0)
    '("mem@margin@float" LaTeX-env-args ["argument"] 1)
    '("verse" LaTeX-env-args ["argument"] 0)))
 :latex)

