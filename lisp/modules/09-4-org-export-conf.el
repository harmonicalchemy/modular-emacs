;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  [modular-emacs]:~/.emacs.d/lisp/modules/09-4-org-export-conf.el
;;
;;  This is a sub module of 09-org-mode-pkg-conf.el which adds extra tools for
;;  exporting org-mode with specific features to many different output
;;  publishing formats...
;;
;;  Consider this file to be your:
;;     CONTROL PANEL TO A POWERFUL EMACS ORG MODE PUBOPS ENGINE
;;
;;  YOU NOW HAVE IN YOUR HANDS PUBLISHING ABILITIES WAY BEYOND SCRIVENER!
;;       (All Encapsulated Within Future Proof Plain Text Files) 
;;
;;  Override this file by placing a copy of it into "my-modules" then change
;;  settings below *within your cloned copy* to suit your own particular
;;  org mode exporting needs...
;;

;;
;; Change Log: (descending chronological order)
;;

;; 2022-012-19 - Alisha Awen, siren1@disroot.org
;;   Finally getting LaTeX PDF Book Formats looking GOOD!
;;   Nothing fancy but it is all working as intended...
;;   Two enabled DEFAULT LaTeX Classes have been defined:
;;   - "fictbook" (For Fiction (Chapter Numbering only - NO TOC)
;;
;;   - "refbook" for Reference Manuals, Technical Books, Tutorials etc.
;;               (With Chapter & Sub-Topic Section Numbering AND TOC)

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

;;  Put Caption Below In Tables:

(setq org-export-latex-table-caption-above nil)  
(setq org-latex-table-caption-above nil)

;;  Don'T Export Tags:

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
;; Set Default Compile To PDF:

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
;;  Position Tags Right After Last Char Of Headline:
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
;;  Speed Keys For Quick Navigation:

(setq org-use-speed-commands 1)

;;;
;;  Set Maximum Indentation For Org-Mode Description Lists:

(setq org-list-description-max-indent 5)

;;;
;;  Prevent Org-Mode Demoting Heading Also Shifting Text Inside Sections:

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

;;;
;;  Syntax Highlight Code Blocks:

(setq org-src-fontify-natively t)

;;;
;;  Make Sure TEXINPUTS is Set To: elpa/auctex-nn.nn.n/latex
;;  "require 'preview" works as long as auctex is installed...

(unless (require 'latex nil t)
  (message "MELPA package 'latex not yet installed..."))

(require 'preview)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  BEGIN: Enable Frame Modifications from eLisp
;;         Source Code Blocks:
;;  REF: https://emacs.stackexchange.com/questions/42096/running-elisp-within-an-orgmode-code-block
;;
;;  The following lisp code defines the new emacs-lisp source block parameter
;;  :keep-windows. You can modify the window configuration through the source
;;  block if you set this parameter to t.
;;
;;  This replaces: save-window-excursion in org-babel-execute:emacs-lisp by a
;;  newly defined macro save-window-excursion-if. The new macro needs a predicate
;;  as new first argument. The window configuration is only stored if that
;;  predicate is non-nil. That is where the new source block parameter
;; :keep-windows is tested.

;; The function replace-in-fundef below needs access to the source file:
;;  ob-emacs-lisp.el.

(require 'ob-emacs-lisp)

;;;
;;  FUNCTION: Transform TREE by TRAFO

(defun transform-tree (tree trafo)
  "Transform TREE by TRAFO."
  (let ((next tree))
    (while next
      (let ((this next))
    (setq next (cdr next))
    (if (consp (car this))
        (transform-tree (car this) trafo)
      (funcall trafo this)))))
  tree)

;;;
;;  FUNCTION: Replace in Function

(defun replace-in-fundef (fun sym &rest replacement)
  "In function FUN perform REPLACEMENT."
  (setq fun (or
         (condition-case err
         (let* ((pos (find-function-noselect fun t))
            (buf (car pos))
            (pt (cdr pos)))
           (with-current-buffer buf
             (save-excursion
               (goto-char pt)
               (read buf))))
         (error nil))
         (and (symbolp fun) (symbol-function fun))
         fun))
  (transform-tree fun
   (lambda (this)
     (when (eq (car this) sym)
       (let ((copy-repl (cl-copy-list replacement)))
     (setcdr (last copy-repl) (cdr this))
     (setcdr this (cdr copy-repl))
     (setcar this (car copy-repl)))))))

;;;
;;  MACRO: Save Window Excursion If PRED Is non-nil

(defmacro save-window-excursion-if (pred &rest body)
  "Act like `save-window-excursion' if PRED is non-nil."
  (declare (indent 1) (debug t))
  (let ((c (make-symbol "wconfig")))
    `(let ((,c (and ,pred (current-window-configuration))))
       (unwind-protect (progn ,@body)
         (when ,c (set-window-configuration ,c))))))


(advice-remove 'org-babel-execute:emacs-lisp #'ad-org-babel-execute:emacs-lisp)

;; Make Sure We Have Access To The Source Code Of `org-babel-execute:emacs-lisp'

(find-function-noselect 'org-babel-execute:emacs-lisp t)

;; (defun ad-org-babel-execute:emacs-lisp ...):

(eval
 (replace-in-fundef
  'org-babel-execute:emacs-lisp
  'org-babel-execute:emacs-lisp
  'ad-org-babel-execute:emacs-lisp))

;; USE: `save-window-excursion-if' in `ad-org-babel-execute:emacs-lisp':

(declare-function 'ad-org-babel-execute:emacs-lisp " ")
(eval
 (replace-in-fundef
  'ad-org-babel-execute:emacs-lisp
  'save-window-excursion
  'save-window-excursion-if
  '(null (member (cdr (assoc :keep-windows params)) '("yes" "t")))))

;; Replace `org-babel-execute:emacs-lisp':

(advice-add 'org-babel-execute:emacs-lisp :override #'ad-org-babel-execute:emacs-lisp)

;; USAGE: The Above COMPLICATED Configuration (Funcs,Macro,etc...) allows you to
;;        make source blocks within a .org file that alter frame geometry etc...
;;        This is doing by specifying :keep-windows as follows:
;;
;;    #+BEGIN_SRC elisp :results silent :keep-windows t
;;       PUT Your Window or Frame Mod Code Here...
;;    #+END_SRC
;;
;; END: Enable Frame Modifications from eLisp
;;      Source Code Blocks:
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  BEGIN: Custom LaTeX Configurations for
;;         Export to PDF...
;;
;;  These are NEW as of: 2022-008-29 
;;  Trying out Gene Ting-Chun Kao's:
;;  "Emacs Org Mode export to pdf" Repository:
;;  .../my-modules/orgmode-latex-templates
;;  The code below comes from her repo above
;;
;;  Update: 2022-011-19 - Added some more LaTeX eBook styles
;;          which I am configuring first for Fiction eBooks
;;          and later when that is working well, I will modify
;;          that to make other eBook styles for Modular Emacs...
;;
;;  THESE Earlier PDF Process Configs were tried and then COMMENTED OUT:
;;
;;   (setq org-latex-pdf-process '("latexmk -bibtex -f %f"))
;;
;;   (setq org-latex-pdf-process
;;    '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))

;; USE Lualatex Preview: (Since 2022-011-23)
;; Ref: https://stackoverflow.com/questions/41568410/configure-org-mode-to-use-lualatex

(setq org-latex-pdf-process
  '("lualatex -shell-escape -interaction nonstopmode %f"
    "lualatex -shell-escape -interaction nonstopmode %f"))

(setq luamagick '(luamagick :programs ("lualatex" "convert")
       :description "pdf > png"
       :message "you need to install lualatex and imagemagick."
       :use-xcolor t
       :image-input-type "pdf"
       :image-output-type "png"
       :image-size-adjust (1.0 . 1.0)
       :latex-compiler ("lualatex -interaction nonstopmode -output-directory %o %f")
       :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O")))

(add-to-list 'org-preview-latex-process-alist luamagick)

(setq org-preview-latex-default-process 'luamagick)

(with-eval-after-load 'ox-latex

  ;; ~~~~~~~~~~~~~~
  ;; ETHZ CLASS: (Initial Trial NOW DISABLED  2022-011-21)

  ;; (add-to-list
  ;;  'org-latex-classes
  ;;  '("ethz"
  ;;    "\\documentclass[a4paper,11pt,titlepage]{memoir}
  ;; \\usepackage[utf8]{inputenc}
  ;; \\usepackage[T1]{fontenc}
  ;; \\usepackage{fixltx2e}
  ;; \\usepackage{graphicx}
  ;; \\usepackage{longtable}
  ;; \\usepackage{float}
  ;; \\usepackage{wrapfig}
  ;; \\usepackage{rotating}
  ;; \\usepackage[normalem]{ulem}
  ;; \\usepackage{amsmath}
  ;; \\usepackage{textcomp}
  ;; \\usepackage{marvosym}
  ;; \\usepackage{wasysym}
  ;; \\usepackage{amssymb}
  ;; \\usepackage{hyperref}
  ;; \\usepackage{mathpazo}
  ;; \\usepackage{color}
  ;; \\usepackage{enumerate}
  ;; \\definecolor{bg}{rgb}{0.95,0.95,0.95}
  ;; \\tolerance=1000
  ;;       [NO-DEFAULT-PACKAGES]
  ;;       [PACKAGES]
  ;;       [EXTRA]
  ;; \\linespread{1.1}
  ;; \\hypersetup{pdfborder=0 0 0}"
  ;;    ("\\chapter{%s}" . "\\chapter*{%s}")
  ;;    ("\\section{%s}" . "\\section*{%s}")
  ;;    ("\\subsection{%s}" . "\\subsection*{%s}")
  ;;    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ;;    ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ;;    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; ~~~~~~~~~~~~~~
  ;; HAP DEFAULT ARTICLE CLASS: (Initial Trial NOW DISABLED  2022-011-21)

  ;; (add-to-list
  ;;  'org-latex-classes
  ;;  '("article"
  ;;    "\\documentclass[11pt,a4paper]{article}
  ;; \\usepackage[utf8]{inputenc}
  ;; \\usepackage[T1]{fontenc}
  ;; \\usepackage{fixltx2e}
  ;; \\usepackage{graphicx}
  ;; \\usepackage{longtable}
  ;; \\usepackage{float}
  ;; \\usepackage{wrapfig}
  ;; \\usepackage{rotating}
  ;; \\usepackage[normalem]{ulem}
  ;; \\usepackage{amsmath}
  ;; \\usepackage{textcomp}
  ;; \\usepackage{marvosym}
  ;; \\usepackage{wasysym}
  ;; \\usepackage{amssymb}
  ;; \\usepackage{hyperref}
  ;; \\usepackage{mathpazo}
  ;; \\usepackage{color}
  ;; \\usepackage{enumerate}
  ;; \\definecolor{bg}{rgb}{0.95,0.95,0.95}
  ;; \\tolerance=1000
  ;;       [NO-DEFAULT-PACKAGES]
  ;;       [PACKAGES]
  ;;       [EXTRA]
  ;; \\linespread{1.1}
  ;; \\hypersetup{pdfborder=0 0 0}"
  ;;    ("\\section{%s}" . "\\section*{%s}")
  ;;    ("\\subsection{%s}" . "\\subsection*{%s}")
  ;;    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ;;    ("\\paragraph{%s}" . "\\paragraph*{%s}")))

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;;  HAP DEFAULT EBOOK CLASS: (Initial Trial NOW DISABLED  2022-011-21)
  ;;  (ebook - use memoir)

  ;; (add-to-list
  ;;  'org-latex-classes
  ;;  '("ebook"
  ;;    "\\documentclass[13pt, oneside]{memoir}
  ;; \\setstocksize{9in}{6in}
  ;; \\settrimmedsize{\\stockheight}{\\stockwidth}{*}
  ;; \\setlrmarginsandblock{2cm}{2cm}{*} % Left and right margin
  ;; \\setulmarginsandblock{2cm}{2cm}{*} % Upper and lower margin
  ;; \\checkandfixthelayout
  ;; % Much more laTeX code omitted"
  ;;    ("\\chapter{%s}" . "\\chapter*{%s}")
  ;;    ("\\section{%s}" . "\\section*{%s}")
  ;;    ("\\subsection{%s}" . "\\subsection*{%s}")))


  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;;  HAP DEFAULT REFERENCE EBOOK CLASS:
  ;;  (refbook - use memoir)
  ;;  For Reference Manuals, Technical Books, Tutorials, etc...
  ;;  This class/style was initially inspired by:
  ;;     Org-Docs/10-RTFM/MemoirChapStyles.pdf
  ;;
  ;; REMOVED: \\usepackage{afterpage}  (this does not seem to be needed)
  
  (add-to-list
   'org-latex-classes
   '("refbook"
     "\\documentclass[openleft,oneside,showtrims]{memoir}
\\usepackage{calc}

\\setlength{\\headwidth}{\\textwidth}
\\addtolength{\\headwidth}{.382\\foremargin}

\\renewcommand\\afterchapternum{}

\\renewcommand\\afterchaptertitle{%
\\ifnum \\c@secnumdepth>\\m@ne%
\\ifNoChapNum\\else\\par\\nobreak\\vskip\\afterchapskip\\fi%
\\fi}

\\setlength\\beforechapskip{15pt}
\\renewcommand\\printchapternonum{\\global\\NoChapNumtrue}
\\renewcommand{\\chaptitlefont}{\\raggedleft\\normalfont\\Huge\\bfseries}
\\makeatletter

\\AtBeginDocument{
    \\nonzeroparskip
    \\frontmatter
}

\\chapterstyle{lyhne}"
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;;  HAP DEFAULT FICTION EBOOK CLASS:
  ;;  (fictbook - use memoir)
  ;;  For Fiction Books or Docs NOT requiring a TOC 
  ;;  or Sub-Topic Section Numbering...
  ;;  This class/style was initially inspired by:
  ;;     Org-Docs/10-RTFM/MemoirChapStyles.pdf
  ;;
  ;; REMOVED: \\usepackage{afterpage}  (this does not seem to be needed)

  (add-to-list
   'org-latex-classes
   '("fictbook"
     "\\documentclass[openleft,oneside,showtrims]{memoir}
\\usepackage{calc}

\\setlength{\\headwidth}{\\textwidth}
\\addtolength{\\headwidth}{.382\\foremargin}

\\renewcommand\\afterchapternum{}

\\renewcommand\\afterchaptertitle{%
\\ifnum \\c@secnumdepth>\\m@ne%
\\ifNoChapNum\\else\\par\\nobreak\\vskip\\afterchapskip\\fi%
\\fi}

\\setlength\\beforechapskip{15pt}
\\renewcommand\\printchapternonum{\\global\\NoChapNumtrue}
\\renewcommand{\\chaptitlefont}{\\raggedleft\\normalfont\\Huge\\bfseries}
\\makeatletter

\\AtBeginDocument{
    \\nonzeroparskip
    \\frontmatter
}

\\chapterstyle{lyhne}"
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;;  END: Custom LaTeX Configurations for Export to PDF
;;       (CUSTOMIZED org-latex-classes LIST)
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
