;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   [modular-emacs]:~/.emacs.d/lisp/modules/09-4-org-export-conf.el
;;
;;   This is a sub module of 09-org-mode-pkg-conf.el which adds extra tools for
;;   exporting org-mode with specific features to many different output
;; publishing formats...
;; Consider this file to be your:
;;     CONTROL PANEL TO A POWERFUL EMACS ORG MODE PUBOPS ENGINE
;;
;; YOU NOW HAVE IN YOUR HANDS PUBLISHING ABILITIES WAY BEYOND SCRIVENER!
;;       (All Encapsulated Within Future Proof Plain Text Files) 
;;
;;   Override this file by placing a copy of it into "my-modules" then change
;;   settings below *within your cloned copy* to suit your own particular
;;   org mode export needs...
;;

;;
;; Change Log: (descending chronological order)
;;

;; 2022-009-18 - Alisha Awen, siren1@disroot.org
;;   Removed some of the Babel Languages which were causing problems
;;   Still troubleshooting this...
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;;  Ensure Export Agents are "Locked & Loaded" %^)

(require 'ox-md)
(require 'ox-latex)
(require 'tex)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Emacs AucTeX Configuration:
;;  LaTeX Export Options:
;;  Note: New Configs Added 2022-008-31
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;  put caption below in tables:
(setq org-export-latex-table-caption-above nil)  
(setq org-latex-table-caption-above nil)
;;  don't export tags:
(setq org-export-with-tags nil)    
(setq org-export-latex-listings t)
(setq org-latex-listings 'listings)
(setq org-latex-prefer-user-labels t)

(add-to-list 'org-latex-packages-alist '("" "listings"))

(setq TeX-auto-save t)
(setq TeX-fold-mode 1)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq reftex-plug-into-AUCTeX t)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Babel LaTeX Configuration:
;;  Note: New Configs Added 2022-008-31
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(setq org-src-preserve-indentation t)
(setq org-confirm-babel-evaluate nil)
(setq org-babel-python-command "python") ;; Use Defined "python" for Current OS...

;;;
;; Set default compile to PDF:

(setq TeX-PDF-mode t)

;;;
;; LaTeX Mode Hook tweaks:

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;;;
;;  Hide the Emphasis Markup:
;;  (e.g., /.../ for italics, *...* for bold, etc.)
;;
;;  Disable this form if you want to see them...
;;
;;  TIP: The invisible characters are easy enough to remove
;;       as there will be a small space in its place...
;;       You can backspace over it to remove it and then
;;       you will be able to see and remove the other one as well...
;;
;;       ALSO: I bound space-b (xah-fly-keys) which runs
;;       (org-toggle-link-display) which exposes invisible
;;       brackets around links, as well as the emphasis markup
;;       which is the easiest way to see them when you need to...

(setq org-hide-emphasis-markers t)

;;;
;;  Position tags right after last char of headline:
;;  (This is mostly to fix problems with variable-pitch
;;  It does not seem to be a problem with fixed-pitch)
;;  NOTE: I disabled this thing... It was causing an
;;        error on load!  I need to look into this
;;        later and fix it, or get rid of it...
;;        I am not even sure if I need it as I am
;;        not using Emacs in global scaled mode, or
;;        what ever that's called... lol...
;;        There may be more to this than I realized! %^)
;;        I will get to it later when I start messing with org tags...
;(org-tags-column 0)

;;;
;;  speed keys for quick navigation:

(setq org-use-speed-commands 1)

;;;
;;  set maximum indentation for org-mode description lists:

(setq org-list-description-max-indent 5)

;;;
;;  prevent org-mode demoting heading also shifting text inside sections:

(setq org-adapt-indentation nil)

;;;
;;  Inline Images:
;;  Set Inline Images here to nil so that they will display with their original
;;  sizes...
;;  NOTE:  To keep very large images to a reasonable size within org-mode, and
;;         also for exporting to HTML, LaTeX, etc.  Set the following properties
;;         on the inline image link within the file where it is placed...
;;         You only need to do this for HUGE files though...
;;         Example: (adjust Name, Figure, and Sizes as needed)
;;
;;                #+NAME: fig:figure name
;;                #+CAPTION: figure name
;;                #+ATTR_ORG: :width 200/250/300/400/500/600
;;                #+ATTR_LATEX: :width 2.0in
;;                #+ATTR_HTML: :width 200/250/300/400/500/600px
;;                [[file:./file.png]]

(setq org-image-actual-width nil)

;;;
;;  Automatically Refresh Inline Images:
;;  REF: http://emacs.stackexchange.com/questions/3302/live-refresh-of-inline-images-with-org-display-inline-images

(defun shk-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(add-hook 'org-babel-after-execute-hook 'shk-fix-inline-images)

;;  syntax highlight code blocks:
(setq org-src-fontify-natively t)

;;;
;;  Make sure TEXINPUTS is set to: elpa/auctex-nn.nn.n/latex
;;  require 'preview below should set this as long as auctex is installed...

(unless (require 'latex nil t)
  (message "MELPA package 'latex not yet installed..."))

(require 'preview)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Custom LaTeX Configurations for Export to PDF
;;  These are NEW as of: 2022-008-29 
;;  Trying out Gene Ting-Chun Kao's:
;;  "Emacs Org Mode export to pdf" Repository:
;;  .../my-modules/orgmode-latex-templates
;;  The code below comes from her repo above

(setq org-latex-pdf-process
      '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))

;(setq org-latex-pdf-process '("latexmk -bibtex -f %f"))

(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ENABLED ORG LATEX CLASSES:

(with-eval-after-load 'ox-latex
(add-to-list
 'org-latex-classes
 '("ethz"
   "\\documentclass[a4paper,11pt,titlepage]{memoir}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\usepackage{mathpazo}
\\usepackage{color}
\\usepackage{enumerate}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\tolerance=1000
      [NO-DEFAULT-PACKAGES]
      [PACKAGES]
      [EXTRA]
\\linespread{1.1}
\\hypersetup{pdfborder=0 0 0}"
   ("\\chapter{%s}" . "\\chapter*{%s}")
   ("\\section{%s}" . "\\section*{%s}")
   ("\\subsection{%s}" . "\\subsection*{%s}")
   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
   ("\\paragraph{%s}" . "\\paragraph*{%s}")
   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; ~~~~~~~~~~~~~~
;; ARTICLE CLASS:

(add-to-list
 'org-latex-classes
 '("article"
   "\\documentclass[11pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\usepackage{mathpazo}
\\usepackage{color}
\\usepackage{enumerate}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\tolerance=1000
      [NO-DEFAULT-PACKAGES]
      [PACKAGES]
      [EXTRA]
\\linespread{1.1}
\\hypersetup{pdfborder=0 0 0}"
   ("\\section{%s}" . "\\section*{%s}")
   ("\\subsection{%s}" . "\\subsection*{%s}")
   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
   ("\\paragraph{%s}" . "\\paragraph*{%s}")))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  EBOOK CLASS: (use memoir)

(add-to-list
 'org-latex-classes
 '("ebook"
   "\\documentclass[13pt, oneside]{memoir}
\\setstocksize{9in}{6in}
\\settrimmedsize{\\stockheight}{\\stockwidth}{*}
\\setlrmarginsandblock{2cm}{2cm}{*} % Left and right margin
\\setulmarginsandblock{2cm}{2cm}{*} % Upper and lower margin
\\checkandfixthelayout
% Much more laTeX code omitted
"
   ("\\chapter{%s}" . "\\chapter*{%s}")
   ("\\section{%s}" . "\\section*{%s}")
   ("\\subsection{%s}" . "\\subsection*{%s}"))))

;;  END: Custom LaTeX Configurations for Export to PDF
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Org Babel Active Language Configurations:
;;       t = enable     nil = disable
;;
;;  Prerequisites:
;;    ABC requires: abc-mode and the following
;;    external programs:
;;      abcm2ps (Install with Macports on MacOS)
;;      This can be installed on Fedora (by exact name above)
;;      I have not checked Debian yet.. But probably the same...
;;      ps2pdf (bundled with GhostScript)
;;  Usage:
;;     Babel adds some new elements to code blocks.
;;     The basic structure becomes:
;;
;;       #+BEGIN_SRC language  org-switches header-arguments
;;       ,body
;;       #+END_SRC
;;
;;     Compile Babel Code blocks in the standard way using
;;     C-c C-c while the cursor is within the code block.
;;     For Example:
;;       C-c C-c within an ABC block will compile the block.
;;       into nice music notation...
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;  Enable Extra Custom Org-Babel Language:
;;   (Add New Custom Language Configs HERE)


;;; Activate External Language Execution:
;;

(require 'org)
(require 'ob)
(require 'ob-html)
(require 'ob-lisp)
(require 'ob-asymptote)
(require 'ob-abc)

;; Enable normal tab behaviour for SRC Block language Modes:
(setq org-src-tab-acts-natively t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((abc . t)
   (asymptote . t)
   (awk . t)
   (calc . t)
   (C . t)
;   (cpp . t)
   (ditaa . t)
   (dot . t)
   (lisp . t)
   (emacs-lisp . t)
   (clojure . t)
   (gnuplot . t)
   (haskell . nil)
   (java . t)
   (js . t)
   (latex . t)
;   (ledger . t)    ;Hopefully this will add support for hledger too...
   (lilypond . t)
   (lua . nil)
   (makefile . t)
   (ocaml . nil)
   (octave . t)
   (org . t)
   (perl . t)
   (python . t)
   (ruby . t)
   (sass . t)
   (css . t)
   (sed . t)
   (screen . nil)
   (shell . t)
   (sql . nil)
   (sqlite . t)))
;;
;;; END: Activate External Language Execution

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   END: [modular-emacs]:~/.emacs.d/lisp/modules/09-4-org-export-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
