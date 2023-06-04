; -*- coding: utf-8; lexical-binding: t; -*-
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; File:        ~/.emacs.d/lisp/modules/09-4-org-export-conf.el
;; Ref:         <https://github.com/harmonicalchemy/modular-emacs>
;; Author:      Alisha Awen
;; Maintainer:  Alisha Awen
;; Created:     2018-011-13
;; Updated:     2023-002-10

;; This File is NOT Part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;

;;;
;; INFO:
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
;;  settings below (within your cloned copy) to suit your own particular
;;  org mode exporting needs...
;;

;;;
;; CHANGE LOG: (descending chronological order)
;;

;; 2023-002-10 - Alisha Awen, siren1@disroot.org
;;   New LaTeX Classes and MODS of Existing class definitions
;;   were performed between Dec 2022 and now...  Lots of
;;   changes to how Skeleton Template .org files get published
;;   Still working on ALL of this and some files are still
;;   trying to use the old setupfiles etc... TBC...
;;   Next step is to move LaTeX code out of here as much as
;;   possible and instead use \\input .../filename.tex where
;;   all the real LaTeX code lives and looks like it should
;;   look without having to escape all backslashes etc...
;;   Embedding LaTeX code into eLisp looks wicked ugly to me
;;   and it is HARD to read... 

;; 2022-012-24 - Alisha Awen, siren1@disroot.org
;;   General Clean Up of all code below...
;;   (all of my LaTeX configuration and testing
;;   got this file all spaghetti like... ;-)

;; 2022-012-19 - Alisha Awen, siren1@disroot.org
;;   Finally getting LaTeX PDF Book Formats looking GOOD!
;;   Nothing fancy but it is all working as intended...
;;   Two enabled DEFAULT LaTeX Classes have been defined:
;;
;;     "fictbook" (For Fiction (Chapter Numbering only - NO TOC)
;;
;;     "refbook" for Reference Manuals, Technical Books, Tutorials etc.
;;               (With Chapter & Sub-Topic Section Numbering AND TOC)

;; 2022-009-18 - Alisha Awen, siren1@disroot.org
;;   Removed some of the Babel Languages which were causing problems
;;   Still troubleshooting this... (btw, now fixed in 2023)
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;;  Ensure Export Agents are "Locked & Loaded" %^)

(require 'ox-md)
(require 'ox-latex)
(require 'tex)

;; ;; The function replace-in-fundef below needs access to the source file:
;; ;;  ob-emacs-lisp.el.

;; (require 'ob-emacs-lisp)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  DEFAULT AucTeX - LaTeX Configs:
;;  Note: New Configs Added 2022-012-24
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(setq
 org-src-preserve-indentation t

 ;;  Don'T Export Tags:
 org-export-with-tags nil

 ;;  Put Caption Below In Tables:
 org-export-latex-table-caption-above nil
 org-latex-table-caption-above nil

 ;;  Hide the Emphasis Markup:
 ;;  (e.g., /.../ for italics, *...* for bold, etc.)
 org-hide-emphasis-markers t

 ;; Turn ON Source Block Syntax highlighting
 org-src-fontify-natively t

 org-src-tab-acts-natively t
 org-latex-prefer-user-labels t

 ;; Don't Prompt Before Running Code In Org
 org-confirm-babel-evaluate nil

 ;; DEFINE ORG LATEX PDF PROCESS:

 org-latex-pdf-process '("lualatex -shell-escape -interaction nonstopmode %f")
 ;; org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
 ;;                         "xelatex -interaction nonstopmode %f") ;; for multiple passes
 ;; Previous Settings tried:
 ;; org-latex-pdf-process '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f")
 ;; org-latex-pdf-process '("latexmk -bibtex -f %f")

 org-babel-python-command "python") ;; Use Defined "python" for Current OS...


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Set Default Compile To PDF:

(setq org-latex-listings 't)
(setq TeX-PDF-mode t)
(setq TeX-auto-save t)
(setq TeX-fold-mode 1)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq reftex-plug-into-AUCTeX t)

;;;
;; LaTeX Mode Hook tweaks:

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

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
;;  Make Sure TEXINPUTS is Set To: elpa/auctex-nn.nn.n/latex
;;  "require 'preview" works as long as auctex is installed...

(unless (require 'latex nil t)
  (message "MELPA package 'latex not yet installed..."))

(require 'preview)



;; ;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ;;  BEGIN: Enable Frame Modifications from eLisp
;; ;;         Source Code Blocks:
;; ;;  REF: https://emacs.stackexchange.com/questions/42096/running-elisp-within-an-orgmode-code-block
;; ;;
;; ;;  The following lisp code defines the new emacs-lisp source block parameter
;; ;;  :keep-windows. You can modify the window configuration through the source
;; ;;  block if you set this parameter to t.
;; ;;
;; ;;  This replaces: save-window-excursion in org-babel-execute:emacs-lisp by a
;; ;;  newly defined macro save-window-excursion-if. The new macro needs a predicate
;; ;;  as new first argument. The window configuration is only stored if that
;; ;;  predicate is non-nil. That is where the new source block parameter
;; ;; :keep-windows is tested.

;; ;;;
;; ;;  FUNCTION: Transform TREE by TRAFO

;; (defun transform-tree (tree trafo)
;;   "Transform TREE by TRAFO."
;;   (let ((next tree))
;;     (while next
;;       (let ((this next))
;;     (setq next (cdr next))
;;     (if (consp (car this))
;;         (transform-tree (car this) trafo)
;;       (funcall trafo this)))))
;;   tree)

;; ;;;
;; ;;  FUNCTION: Replace in Function

;; (defun replace-in-fundef (fun sym &rest replacement)
;;   "In function FUN perform REPLACEMENT."
;;   (setq fun (or
;;          (condition-case err
;;          (let* ((pos (find-function-noselect fun t))
;;             (buf (car pos))
;;             (pt (cdr pos)))
;;            (with-current-buffer buf
;;              (save-excursion
;;                (goto-char pt)
;;                (read buf))))
;;          (error nil))
;;          (and (symbolp fun) (symbol-function fun))
;;          fun))
;;   (transform-tree fun
;;    (lambda (this)
;;      (when (eq (car this) sym)
;;        (let ((copy-repl (cl-copy-list replacement)))
;;      (setcdr (last copy-repl) (cdr this))
;;      (setcdr this (cdr copy-repl))
;;      (setcar this (car copy-repl)))))))

;; ;;;
;; ;;  MACRO: Save Window Excursion If PRED Is non-nil

;; (defmacro save-window-excursion-if (pred &rest body)
;;   "Act like `save-window-excursion' if PRED is non-nil."
;;   (declare (indent 1) (debug t))
;;   (let ((c (make-symbol "wconfig")))
;;     `(let ((,c (and ,pred (current-window-configuration))))
;;        (unwind-protect (progn ,@body)
;;          (when ,c (set-window-configuration ,c))))))


;; (advice-remove 'org-babel-execute:emacs-lisp #'ad-org-babel-execute:emacs-lisp)

;; ;; Make Sure We Have Access To The Source Code Of `org-babel-execute:emacs-lisp'

;; (find-function-noselect 'org-babel-execute:emacs-lisp t)

;; ;; (defun ad-org-babel-execute:emacs-lisp ...):

;; (eval
;;  (replace-in-fundef
;;   'org-babel-execute:emacs-lisp
;;   'org-babel-execute:emacs-lisp
;;   'ad-org-babel-execute:emacs-lisp))

;; ;; USE: 'save-window-excursion-if' in 'ad-org-babel-execute:emacs-lisp':

;; (declare-function 'ad-org-babel-execute:emacs-lisp " ")
;; (eval
;;  (replace-in-fundef
;;   'ad-org-babel-execute:emacs-lisp
;;   'save-window-excursion
;;   'save-window-excursion-if
;;   '(null (member (cdr (assoc :keep-windows params)) '("yes" "t")))))

;; ;; Replace `org-babel-execute:emacs-lisp':

;; (advice-add 'org-babel-execute:emacs-lisp :override #'ad-org-babel-execute:emacs-lisp)

;; ;; USAGE: The Above COMPLICATED Configuration (Funcs,Macro,etc...) allows you to
;; ;;        make source blocks within a .org file that alter frame geometry etc...
;; ;;        This is doing by specifying :keep-windows as follows:
;; ;;
;; ;;    #+BEGIN_SRC elisp :results silent :keep-windows t
;; ;;       PUT Your Window or Frame Mod Code Here...
;; ;;    #+END_SRC
;; ;;
;; ;; END: Enable Frame Modifications from eLisp
;; ;;      Source Code Blocks:
;; ;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  CUSTOM Imagemagick LaTeX Configuration
;;         For: Export to PDF...
;;
;;   USE Lualatex Preview: (Since 2022-011-23)
;;
;;   REF:
;;     stackoverflow.com/questions/41568410/configure-org-mode-to-use-lualatex

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



;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  BEGIN: CUSTOM LaTeX CONFIGURATIONS for EXPORT to PDF
;;
;; NOTE: The following packages ARE included by DEFAULT during
;;       LaTeX Export of ALL .org files...
;;
;;       The Following Values are Stored in Variable:
;;       org-latex-default-packages-alist
;;       (As Of 2022-012-28):
;;
;;            [utf8]{inputenc}      PKG INCLUDED
;;               NOTE: LaTeX Compiler WARNS & IGNORES this for UTF8
;;            [T1]{fontenc}         PKG INCLUDED
;;            {graphicx}            PKG INCLUDED
;;            %{longtable}          PKG OMITTED
;;            %{wrapfig}            PKG OMITTED
;;            %{rotating nil}       PKG OMITTED
;;            {normalem ulem}       PKG INCLUDED
;;            {amsmath}             PKG INCLUDED
;;            {amssymb}             PKG INCLUDED
;;            %{capt-of}            PKG OMITTED
;;            %{hyperref}           PKG OMITTED
;;
;;      You Can DISABLE this by using the following macro-like placeholders:
;;
;;         Note: The Org Manual warns you to leave theis alone...
;;               If you mess with these it will force you to do MUCH MORE
;;               CUSTOM LaTeX Work on your own... (no more "it just works")
;;
;;         [NO-DEFAULT-PACKAGES] - DO NOT Include ANY DEFAULT PACKAGES
;;       
;;      In Addition, you can use these macro-like Placeholders as well:
;;
;;         [DEFAULT-PACKAGES]      \\usepackage Statements For Default Packages
;;         [PACKAGES]              \\usepackage Statements For Packages
;;         [NO-PACKAGES]           Do Not Include The Packages
;;         [EXTRA]                 INCLUDE: #+LaTeX_HEADER Directives
;;         [NO-EXTRA]              DO NOT Include #+LaTeX_HEADER Directives
;;         [BEAMER-HEADER-EXTRA]   INCLUDE: Beamer EXTRA Headers
;;
;; UPDATE: 2022-012-20 - Disabled (commented out) all previous
;;         LaTeX eBook styles which were being evaluated...
;;         Two New LaTeX eBook styles were created below:
;;
;;            "fictbook" (For Fiction (Chapter Numbering only - NO TOC)
;;
;;            "refbook" for Reference Manuals, Technical Books, Tutorials etc.
;;                      (With Chapter & Sub-Topic Section Numbering AND TOC)
;;
;;            "logbook" for LOG NOTEBOOKS...
;;                      (With Chapter & Sub-Topic Section Numbering AND TOC)
;;
;;         These new styles are based on memoir class...
;;         They are currently the only active styles for Modular Emacs...
;;         The code for: refbook started out as a clone of fictbook...
;;         fictbook and refbook can and may dirverge at later stages...

(with-eval-after-load 'ox-latex

  ;; ~~~~~~~~~~~~~~ UNUSED CLASSES: ~~~~~~~~~~~~~~~~~~~~~~~~~
  ;;  HAP DEFAULT EBOOK CLASS: (Initial Trial NOW DISABLED  2022-011-21)
  ;;  From: Gene Ting-Chun Kao
  ;;             PhD Researcher @ ETH Zurich BRG & NCCR dfab
  ;;             Research in Computational Design & Fabrication,
  ;;             Visual Computing
  ;; REF:
  ;;   https://github.com/GeneKao/orgmode-latex-templates

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
  ;;
  ;; ~~~~~~~~~~~~~~ END UNUSED CLASSES ~~~~~~~~~~~~~~~~~~~~~~

  
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; BEGIN: (CUSTOMIZED org-latex-classes LIST)
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; EXCERPT DOCUMENT CLASS:
  ;;
  ;; AUTHOR:   Alisha Awen
  ;; REF:      Harmonic Alchemy Productions - Modular Emacs
  ;; UPDATED:  2023-006-04
  ;;
  ;; PURPOSE:
  ;;
  ;;       This is currently being developed to be used with:
  ;;       HAP_PDF-Example-Figure-Template.org A NEW LaTeX configuration
  ;;       for simple PDF excerpts particularly suited for figures or
  ;;       examples that may or may not be originally from a larger
  ;;       Document or Book Project...
  ;;
  ;;       These are most commonly created to serve in support of a
  ;;       larger project, But Publishable as a Simple STAND ALONE PDF
  ;;       when that's the only thing needed to look up while deep inside
  ;;       a project with many docs open. You know what I mean! %^)
  ;;
  ;; REMOVED:
  ;;       This Code Snippet (lisp form) NO LONGER Requires:
  ;;       ~/.emacs.d/Docs/TeX/pdf-excerpt-setup.tex
  ;;       \\input{~/.emacs.d/Docs/TeX/pdf-excerpt-setup.tex}"
  ;;
  ;;       All LaTeX Configuration is NOW done in SETUPFILE:
  ;;       ~/.emacs.d/Docs/pubOps/org-templates/pdf-excerpt-setup.org
  ;;       Which selects this defined class and everything "just works"
  ;;       as expected... (at least if I haven't mucked things up! LOL)

  (add-to-list
   'org-latex-classes
   '("excerpt"
     "\\documentclass[12pt]{article}
                  [NO-DEFAULT-PACKAGES]
                  [PACKAGES]
                  [EXTRA]"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")))
  ;; ~~~~~~~~~~~~~ END: EXCERPT DOCUMENT CLASS ~~~~~~~~~~~~~~~

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; SIMPLE DOCUMENT CLASS:
  ;;
  ;; From: Alisha Awen
  ;; REF:  Harmonic Alchemy Productions - Modular Emacs
  ;;
  ;; This Code Snippet Requires:
  ;;       ~/.emacs.d/Docs/pubOps/org-templates/simple-doc-setup.tex
  ;;
  ;; TODO:
  ;;       This is currently being developed to be used
  ;;       with HAP_Simple-Document-Skeleton.org
  ;;       A LaTeX configuration for short works like
  ;;       README docs, abstracts, short papers, and
  ;;       quick documentation for things not needing
  ;;       a more complex format... bug fixes and refinements
  ;;       are currently ongoing...

  (add-to-list
   'org-latex-classes
   '("simple"
     "\\documentclass[12pt, a4paper]{article}
                  [NO-DEFAULT-PACKAGES]
                  [PACKAGES]
                  [EXTRA]
      \\input{~/.emacs.d/Docs/TeX/simple-doc-setup.tex}"

     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")))
  ;; ~~~~~~~~~~~~~ END: SIMPLE DOCUMENT CLASS ~~~~~~~~~~~~~~~

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; GENERIC DOCUMENT CLASS:
  ;;
  ;; From: Gene Ting-Chun Kao - PhD Researcher @ ETH Zurich
  ;;       BRG & NCCR dfab Research in Computational Design
  ;;       & Fabrication, Visual Computing
  ;;
  ;; REF:
  ;;   https://github.com/GeneKao/orgmode-latex-templates
  ;;
  ;; The code below may be quite modified from Gene's original
  ;; example code... Using his as a starter was better (for me)
  ;; than starting with the default article setup from WORG %^)
  ;;
  ;; TODO:
  ;;       This has not been fully implemented or tested
  ;;       since making big changes to Org-Mode Export
  ;;       configuration... It needs to be tried again later.

  (add-to-list
   'org-latex-classes
   '("generic"
     "\\documentclass[11pt,a4paper]{article}

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
  ;; ~~~~~~~~~~~~~ END: GENERIC DOCUMENT CLASS ~~~~~~~~~~~~~~~

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; ORG-NOTES DOCUMENT CLASS:
  ;; (for exporting Org NOTES to a Finer Quality LaTeX PDF)
  ;;
  ;; REF:
  ;;   https://github.com/mclearc/org-latex-classes
  ;;   https://www.colinmclear.net/posts/teaching-notes/
  ;;   https://github.com/mclear-tools/dotemacs/blob/master/cpm-setup-teaching.el

  (add-to-list
   'org-latex-classes
   '("org-notes"
     "\\documentclass[12pt]{article}
  [NO-DEFAULT-PACKAGES]
  [EXTRA]
  \\input{~/.emacs.d/Docs/TeX/notes-setup-file.tex}"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;; ~~~~~~~~~~~~ END: ORG-NOTES DOCUMENT CLASS: ~~~~~~~~~~~~

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; TUFTE-BOOK DOCUMENT CLASS:
  ;; (for writing classy books)
  ;;
  ;; REF:
  ;;   https://damitr.org/2014/01/09/latex-tufte-class-in-org-mode/
  ;;   https://tufte-latex.github.io/tufte-latex/
  ;;   https://github.com/Tufte-LaTeX/tufte-latex
  ;;
  ;; TODO:
  ;;       This has not been fully implemented or tested
  ;;       since making big changes to Org-Mode Export
  ;;       configuration... It needs to be tried again later.

  (add-to-list
   'org-latex-classes
   '("tuftebook"
     "\\documentclass{tufte-book}\n
  \\usepackage{color}
  \\usepackage{amssymb}
  \\usepackage{gensymb}
  \\usepackage{nicefrac}
  \\usepackage{units}"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;; ~~~~~~~~~~~ END: TUFTE-BOOK DOCUMENT CLASS: ~~~~~~~~~~~~

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; TUFTE-HANDOUT DOCUMENT CLASS:
  ;; (for writing classy handouts and papers)
  ;;
  ;; REF:
  ;;   https://damitr.org/2014/01/09/latex-tufte-class-in-org-mode/
  ;;   https://tufte-latex.github.io/tufte-latex/
  ;;   https://github.com/Tufte-LaTeX/tufte-latex
  ;;
  ;; TODO:
  ;;       This has not been fully implemented or tested
  ;;       since making big changes to Org-Mode Export
  ;;       configuration... It needs to be tried again later.

  (add-to-list
   'org-latex-classes
   '("tuftehandout"
     "\\documentclass{tufte-handout}
  \\usepackage{color}
  \\usepackage{amssymb}
  \\usepackage{amsmath}
  \\usepackage{gensymb}
  \\usepackage{nicefrac}
  \\usepackage{units}"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;; ~~~~~~~~~~ END: TUFTE-HANDOUT DOCUMENT CLASS: ~~~~~~~~~~

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;;  HAP REFERENCE BOOK DOCUMENT CLASS:
  ;;  (refbook - use memoir)
  ;;  For Reference Manuals, Technical Books, Tutorials, etc...
  ;;  This class/style was initially inspired by: .../MemoirChapStyles.pdf
  ;;  This is a morph of: "lyhne" & "EQ" Styles

  (add-to-list
   'org-latex-classes
   '("refbook"
     
     "\\documentclass[openleft,oneside,showtrims]{memoir}
             [NO-DEFAULT-PACKAGES]
             [PACKAGES]
             [EXTRA]
      \\input{~/.emacs.d/Docs/TeX/ref-book-setup.tex}"

     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;; ~~~~~~~ END: HAP REFERENCE BOOK DOCUMENT CLASS: ~~~~~~~~

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;;  HAP AUDIO DRAMA PRODUCTION DOCUMENT CLASS:
  ;;  (audio-production - use memoir)
  ;;  For Audio Drama or Podcast Production Projects...
  ;;  This class/style was initially inspired by:
  ;;  .../latex-samples/MemoirChapStyles/MemoirChapStyles.pdf
  ;;  This is a morph of: "lyhne" & "EQ" Styles
  ;;
  ;; REMOVED: \\usepackage{afterpage}  (this does not seem to be needed)

  (add-to-list
   'org-latex-classes
   '("audio-production"
     "\\documentclass[openleft,oneside,showtrims]{memoir}

  \\usepackage{calc}

  \\setstocksize{9in}{6in}
  \\settrimmedsize{\\stockheight}{\\stockwidth}{*}
  \\setlrmarginsandblock{2cm}{2cm}{*}       %% Left and right margin
  \\setulmarginsandblock{2cm}{2cm}{*}       %% Upper and lower margin
  \\checkandfixthelayout

  \\setlength{\\headwidth}{\\textwidth}
  \\addtolength{\\headwidth}{.382\\foremargin}
  \\renewcommand\\afterchapternum{}
  \\setlength\\beforechapskip{15pt}
  \\renewcommand\\printchapternonum{\\global\\NoChapNumtrue}
  \\renewcommand{\\chaptitlefont}{\\raggedleft\\normalfont\\Huge\\bfseries}

  \\makeatletter
  \\renewcommand\\afterchaptertitle{%
  \\ifnum \\c@secnumdepth>\\m@ne%
  \\ifNoChapNum\\else\\par\\nobreak\\vskip\\afterchapskip
  \\fi%
  \\fi
  }
  \\makeatother

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
  ;; ~~~ END: HAP AUDIO DRAMA PRODUCTION DOCUMENT CLASS: ~~~~

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; HAP BLUE BOX FICTION BOOK DOCUMENT CLASS:
  ;; (blueboxbook - uses memoir)
  ;; For Fiction Books or Docs NOT requiring a TOC
  ;; or Sub-Topic Section Numbering...
  ;; This class/style was initially inspired by: .../MemoirChapStyles.pdf
  ;; This Style is based on: "BlueBox".
  ;; Frontmatter (Preface, TOC, etc.) Page Numbers Are Lower Case Roman...
  ;; Chapter/Appendix Page Numbers Are Normal Integers...
  ;;
  ;; OTHER Font Spec Configs To Try Out:
  ;;
  ;;  \\usepackage[T1]{fontenc} %% Set This first for MODERN fonts...
  ;;
  ;;  %% DISABLED FONT SPECS (for testing, eval, or troubleshooting)
  ;;  %\\usepackage{lmodern} %% Latin Modern Roman
  ;;  %\\usepackage[scaled=.92]{helvet}%. Sans serif - Helvetica
  ;;
  ;;  \\usepackage{tgtermes} %% TeX Gyre Termes (Nimbus Roman with Math)
  ;;  \\usepackage{tgheros}  %% TeX Gyre Heros (URW Nimbus Sans - extended)
  ;;
  ;;  %\\setmainfont[Ligatures=TeX]{Roman}
  ;;  %\\setsansfont[Ligatures=TeX]{Arial}

  (add-to-list
   'org-latex-classes
   '("blueboxbook"

     "\\documentclass[openleft,oneside,showtrims]{memoir}
             [NO-DEFAULT-PACKAGES]
             [PACKAGES]
             [EXTRA]
      \\input{~/.emacs.d/Docs/TeX/fiction-book-setup.tex}"

     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  ;; ~~~~ END: HAP BLUE BOX FICTION BOOK DOCUMENT CLASS: ~~~~

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;;  HAP LOGBOOK DOCUMENT CLASS:
  ;;  For LOG NOTEBOOKS or Possibly Lab Notes etc...
  ;;  (logbook - uses memoir doc class)
  ;;  This class/style was initially inspired by:
  ;;  mirrors.concertpass.com/tex-archive/info/latex-samples/MemoirChapStyles/MemoirChapStyles.pdf
  ;;  This Class Defines a new Modular Emacs Logbook Chapter Style (logbook)
  ;;  LaTeX Configuration is done in Input File: log-book-setup.tex

  (add-to-list
   'org-latex-classes
   '("logbook"
     "\\documentclass[12pt, openleft, oneside, showtrims]{memoir}

         [EXTRA]
         \\input{~/.emacs.d/Docs/TeX/log-book-setup.tex}"
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;; ~~~~~~~~~~~ END: HAP LOGBOOK DOCUMENT CLASS ~~~~~~~~~~~~

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;;  HAP LAB NOTEBOOK DOCUMENT CLASS:
  ;;  (labbook) Uses report Class
  ;;
  ;; TODO: This has not been fully
  ;;       implemented or tested yet...

  (add-to-list
   'org-latex-classes
   '("labbook"
     "\\documentclass[a4paper,12pt]{report}

  \\renewcommand{\\chaptername}{EXPERIMENT}
  \\makeatletter

  \\renewcommand{\\maketitle}{
    \\begin{titlepage}
      \\begin{center}
        \\vspace*{6em}
        \\Huge \\textbf{\\@title} \\\\
        \\vspace{4em}
        \\Large \\textbf{\\@date} \\\\
        \\bigskip
        \\Large \\textbf{\\@author} \\\\
        \\bigskip
        \\large Harmonic Alchemy Productions \\\\
      \\end{center}
    \\end{titlepage}
  }
  \\makeatother

  \\usepackage[margin=0.7in]{geometry}"
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;; ~~~~~~~~ END: HAP LAB NOTEBOOK DOCUMENT CLASS ~~~~~~~~~

  );  END: CUSTOM LaTeX CONFIGURATIONS for EXPORT to PDF
  ;;       (with-eval-after-load 'ox-latex)
  ;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  BEGIN: Org Babel Active Language Configuration:
;;             t = enable     nil = disable
;;
;;  Prerequisites:
;;
;;    ABC requires: abc-mode and the following
;;    external programs:
;;      abcm2ps (Install with Macports on MacOS)
;;      This can be installed on Fedora (by exact name above)
;;      I have not checked Debian yet.. But probably the same...
;;      ps2pdf (bundled with GhostScript)
;;
;;  Usage:
;;
;;     Babel adds some new elements to code blocks.
;;     The basic structure becomes:
;;
;;        #+BEGIN_SRC language org-switches header-arguments
;;           body
;;        #+END_SRC
;;
;;     Compile Babel Code blocks in the standard way using
;;     C-c C-c while the cursor is within the code block.
;;
;;     Example:
;;
;;        C-c C-c within an ABC block will compile the block
;;        into finely styled LaTeX music notation...

;; Activate External Language Execution:

(require 'org)
(require 'ob)
(require 'ob-html)
(require 'ob-js)
(require 'ob-lisp)
(require 'ob-asymptote)
(require 'ob-abc)
(require 'ob-lilypond)

;; ENABLE Extra Custom Org-Babel Language:
;;   (Add New Custom Language Configs HERE)

(eval-after-load "org"
  (progn
    (org-babel-do-load-languages
     'Org-babel-load-languages
     '((abc . t)
       (lilypond . t)
       (sclang . t) ;; SuperCollider Language
       (asymptote . t)
       (dot . t)
       (gnuplot . t)
       (C . t)
       (cpp . t)
       (ditaa . t)
       (lisp . t)
       (elisp . t)
       (emacs-lisp . t)
       (clojure . t)
       (haskell . t)
       (java . t)
       (js . t)
       (latex . t)
       (ledger . t)
       (calc . t)
       (hledger . t)
       (lua . t)
       (makefile . t)
       (ocaml . t)
       (octave . t)
       (org . t)
       (perl . t)
       (python . t)
       (ruby . t)
       (php . t)
       (sass . t)
       (css . t)
       (awk . t)
       (sed . t)
       (screen . t)
       (shell . t)
       (conf . t)
       (sql . t)
       (sqlite . t)))))

;; END: Org Babel Active Language Configuration:
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   END: [modular-emacs]:~/.emacs.d/lisp/modules/09-4-org-export-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
