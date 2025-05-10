; -*- coding: utf-8; lexical-binding: t; -*-
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; File:        ~/.emacs.d/lisp/modules/09-4-org-export-conf.el
;; Ref:         <https://github.com/harmonicalchemy/modular-emacs>
;; Author:      Alisha Awen - harmonicalchemy@pm.me
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
;;     CONTROL PANEL TO A POWERFUL EMACS ORG MODE PUBOPS ENGINE!
;;
;;     (lol CURRENTLY this is a MESS! LaTeX is CRAZY MAN! SO... Add org-mode
;;      to that crazy mix! And... OMG!!! BUT We will get there! %^)
;;      Next adventure shall be Lua... yet another language... Please No... No!
;;
;;  Override this file by placing a copy of it into "my-modules" then change
;;  settings below (within your cloned copy) to suit your own particular
;;  org mode exporting needs... Yeah... You can scramble your own brains on
;;  LaTeX too... evil grin...

;;;
;; CHANGE LOG: (descending chronological order)
;;

;; 2025-004-05 - Alisha Awen, harmonicalchemy@pm.me 
;;   Many of my LaTeX problems are getting solved, Its starting
;;   to work (memoir class) but I got them all looking the same
;;   still a lot of compile warnings etc... Once those compile
;;   warnings are much less, I will start working in the chapter
;;   styles etc. and different fonts/formatting etc. for different
;;   book and document types defined within this file...
;;   ALSO: I got Graphviz DOT stuff working in source code blocks
;;   rendering to the results: blocks as svg files (fantastic!)
;;   MY PROBLEMS: That took weeks to figure out with lots of
;;   visits to: StackExchange... Org-Mode Forums, LaTeX Forums, etc.
;;   TURNED OUT TO BE SIMPLE:  I needed to simply (require 'ob-dot)
;;   and then everything worked great!
;;
;;   NOTE: I ALSO reinstalled Graphviz with MacPorts and fiddled
;;   a lot in MacPorts trying to fix things... Currently I see
;;   MacPorts Graphviz package is installed and I have DOT EXE in
;;   /opt/local/bin... I also installed Graphviz via PIP3 as a 
;;   python module which is currently set to Pip313 via MacPorts...
;;   I HOPE this does not cause any problems... Otherwise I may
;;   have to remove that... (some programs may need it though)

;; 2025-003-23 - Alisha Awen, harmonicalchemy@pm.me
;;   Attempting to fix MANY LaTeX problems, It gets worse
;;   before it gets better... But this is #1 Priority now
;;   for Modular Emacs... Everything else hangs on good publishing!
;;   MOST of the code below has been rewritten and re-organized... Som
;;   of that code was removed because it is for .tex file editing NOT
;;   Org Mode (except within code blocks of course) but that is covered
;;   by setting it properly within: 05-pubOps-pkg-conf.el (where .tex
;;   and other pub%Ops like modes are configured)...
;;
;;   LaTeX by itself is cryptic enough with crazy macros stepping over
;;   each others careful typeset work... Adding escape characters to all
;;   that mess is WAK! I MEAN WAK!!! What are you guys trying to do???
;;   Its all garbled... LOL I VOW to keep all EXTRA LaTeX packages, rules,
;;   etc. within a .tex include file (or as inline org-mode LaTeX source
;;   code blocks... and or possibly startup directives in the files etc..
;;   BUT NO TeX source code embedded within a Lisp form... OMG.. NO please!
;;   I am keeping all those add-to Latex Class Blocks below as SHORT AS POSSIBLE...
;;   turning all the work (i.e., POST Org-Mode processing, PRE \begin{document}
;;   to -> my .Tex include files... THAT is the PLAN...  Running into roadblocks!

;; 2023-010-18 - Alisha Awen, harmonicalchemy@pm.me
;;   Added HAP AUDIO DRAMA PRODUCTION DOCUMENT CLASS and cleaned up some of the
;;   code... All DOC Classes from now on will get MOST of the custom latex
;;   directives done within a .tex include file... Much better for visual purposes
;;   and keeping embedded .TeX code OUT of Lisp (Which is UGLY MIXED UP SPAGHETTI
;;   CODE IMHO)...

;; 2023-006-15 - Alisha Awen, harmonicalchemy@pm.me
;;   General Clean up of Code... Removed as much LaTeX code that is embedded 
;;   within lisp forms below, WHICH IS REALLY UGLY IMHO... Originally I was 
;;   using .tex include files, but NOW I am using org-mode SETUPFILES with 
;;   #+LATEX directives instead...

;;
;;   Using SETUPFILES are Neat and well documented... The commands
;;   do NOT have to be escaped in the org-mode directives which accepts them as
;;   native LaTeX statements with LaTeX syntax, comments, etc... THIS ALL MAKES
;;   MUCH BETTER SENSE doing it this way, not to mention portability... This is
;;   all in line with Knuth's original idea of "literate programming"... EMACS
;;   ORG-MODE IS "LITERATE PROGRAMMING"

;; 2023-002-10 - Alisha Awen, harmonicalchemy@pm.me
;;   New LaTeX Classes and MODS of Existing class definitions were performed
;;   between Dec 2022 and now...  Lots of changes to how Skeleton Template .org
;;   files get published Still working on ALL of this and some files are still
;;   trying to use the old setupfiles etc... TBC... Next step is to move LaTeX
;;   code out of here as much as possible and instead use \\input .../filename.tex
;;   where all the real LaTeX code lives and looks like it should look without
;;   having to escape all backslashes etc... Embedding LaTeX code into eLisp
;;   looks wicked ugly to me and it is HARD to read...

;; 2022-012-24 - Alisha Awen, harmonicalchemy@pm.me
;;   General Clean Up of all code below... (all of my LaTeX configuration and
;;   testing got this file all spaghetti like... ;-)

;; 2022-012-19 - Alisha Awen, harmonicalchemy@pm.me
;;   Finally getting LaTeX PDF Book Formats looking GOOD! Nothing fancy but it
;;   is all working as intended... Two enabled DEFAULT LaTeX Classes have been
;;   defined:
;;
;;     "fictbook" (For Fiction (Chapter Numbering only - NO TOC)
;;
;;     "refbook" for Reference Manuals, Technical Books, Tutorials etc.
;;               (With Chapter & Sub-Topic Section Numbering AND TOC)

;; 2022-009-18 - Alisha Awen, harmonicalchemy@pm.me
;;   Removed some of the Babel Languages which were causing problems Still
;;   troubleshooting this... (btw, now fixed in 2023)
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  INLINE IMAGES:
;;  Set Inline Images here to nil so that they will display with their original
;;  sizes...
;;
;;  NOTE: To keep very large images to a reasonable size within org-mode, and
;;        also for exporting to HTML, LaTeX, etc.  Set the following properties
;;        on the inline image link within the file where it is placed...
;;        You only need to do this for HUGE files though...
;;        Example: (adjust Name, Figure, and Sizes as needed)
;;
;;            #+NAME: fig:figure name
;;            #+CAPTION: figure name
;;            #+ATTR_ORG: :width 200/250/300/400/500/600
;;            #+ATTR_LATEX: :width 2.0in
;;            #+ATTR_HTML: :width 200/250/300/400/500/600px
;;            [[file:./media/file.png]]

(setq org-image-actual-width nil)

;; TODO: Does this work?  Turn OFF to find out eh? Not now... later when you are not busy...
(setq image-background-override-color "white")

;;;
;;  AUTOMATICALLY REFRESH INLINE IMAGES:
;;  REF: http://emacs.stackexchange.com/questions/3302/live-refresh-of-inline-images-with-org-display-inline-images
;; This Stack Exchange Answer is Still valid Org-Mode code after 10 years!

(defun shk-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(add-hook 'org-babel-after-execute-hook 'shk-fix-inline-images)

;;;
;;  LaTeX PREVIEW in Org-Mode BUFFERS:
;;  NOTE: Make Sure TEXINPUTS is Set To: elpa/auctex-nn.nn.n/latex
;;  "preview" works as long as "auctex" is installed...

(unless (require 'latex nil t)
  (message "MELPA package 'latex not yet installed..."))
(require 'preview)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  CUSTOM Imagemagick LaTeX CONFIGURATION
;;         For: PREVIEW and Export to PDF...
;;
;;   USE LUALATEX for PREVIEW:
;;   REF: stackoverflow.com/questions/41568410/configure-org-mode-to-use-lualatex
;;        NOTE: I am NOT using lualatex as primary or-latex-pdf-process as this
;;        StackOverflow tip shows...
;;
;;   To SHOW a LaTeX PREVIEW In Org-Mode USE: "C-c C-x C-l"

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
(require 'ob-shell)
(require 'ob-html)
(require 'ob-js)
(require 'ob-lisp)
(require 'ob-asymptote)
(require 'ob-abc)
(require 'ob-lilypond)
(require 'ob-dot)

;; Don't Prompt Before Running Code In Org
(setq org-confirm-babel-evaluate nil)

;; Use Defined "python" for Current OS...
;; I have both python and python3 defined
;; to the SAME thing (latest Py3xx) on my system...
(setq org-babel-python-command "python")

;; Set Lilypond EXE for Babel to USE:
;; Use CURRENT lilypond python10 exe from (Macports: )
(setq org-babel-lilypond-ly-command "/opt/local/bin/lilypond")
(setq org-babel-lilypond-commands "/opt/local/bin/lilypond")

;; ENABLE Extra Custom Org-Babel Language:
;;   (Add New Custom Language Configs HERE)

(eval-after-load 'org
  (progn
    (org-babel-do-load-languages
     'Org-babel-load-languages
     '((shell . t) 
       (conf . t)
       (dot . t)
       (abc . t) 
       (lilypond . t) 
       (sclang . t) 
       (asymptote . t) 
       (gnuplot . t)
       (ditaa . t)
       (emacs-lisp . t)
       (lisp . t)
       (elisp . t)
       (clojure . t)
       (haskell . t)
       (C . t)
       (cpp . t)
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
       (sql . t)
       (sqlite . t)))))

;; END: Org Babel Active Language Configuration:
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  DEFAULT AucTeX - LaTeX Configs:
;;  Note: MOST Configs CHANGED 2025-003-24
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;;  Ensure Export Agents are "Locked & Loaded" %^)

(require 'ox-md)
(require 'ox-latex)
(require 'tex)

;; DISABLING THIS FOR NOW.. Let org-mode do its thing here...
;(setq org-latex-prefer-user-labels t)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; ORG LATEX to PDF PROCESSING:
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;; ADD MINTED to ORG LaTeX Packages alist for IMPROVED SOURCE CODE TYPESETS:
;; org-latex-packages-alist is normally nil so there will be no conflicts
;; with other packages.... We also need to set: org-latex-listings...
;;
;; NOTE: DO NOT INCLUDE minted EXPLICITLY Using: 
;;       #+LATEX_HEADER within any org file or setup file!
;;       (otherwise YOU WILL have inclusions that CONFLICT!)
;;

;; PREFER minted for source code export in LaTeX.
(setq org-latex-listings (quote minted))

;; LOAD "minted" with option to use our BUILD dir here...
;; NOW ORG PDF LaTeX OUTPUT reports an error: (but PDF is Fine)
;; The minted folder was placed within the ./build dir as well... (no problem)
;; PACKAGE MINTED ERROR: Package option "outputdir" is no longer needed with
;; minted v3+; The output directory is automatically detected for TeX Live 2024+,
;; and  the environment variable TEXMF_OUTPUT_DIRECTORY can be set manually in
;; other cases... WTF?
   ;(setq org-latex-packages-alist '(("outputdir=./build" "minted" nil)))
;; Try again without output dir:

(setq org-latex-packages-alist '(("" "minted" t ("xelatex"))))
;; You MAY need this too... (from "help" C-h on the var)
;(add-to-list 'org-latex-packages-alist '("" "listings"))
;(add-to-list 'org-latex-packages-alist '("" "color"))

;;;
;; DEFINE ORG LATEX PDF PROCESS:
;;
;;  org-latex-pdf-process is a list of shell commands... 
;;  The SECOND setq form below does the following:
;;
;;   - CREATE the `build' subdirectory if it is not present...
;;   - RUN latexmk with the proper options (in particular -shell-escape which is
;;     necessary in order to allow the LaTeX processor to run an external program,
;;     like pygmentize in the case of minted; and -output-directory to allow all the
;;     artifacts to be sent there)...
;;   - FINALLY, MOVE the .pdf file to the parent directory of the build subdirectory
;;     so that the exporter will be able to find it and not complain...

;; PREFEX `xelatex' as the first LaTeX PROCESSOR to use:
(setq org-latex-compiler "xelatex")

;; %latex below is replaced by the value of org-latex-compiler, (above form)
;; Therefore, EXPLICITLY USE: "xelatex" as the LaTeX PROCESSOR...

(setq org-latex-pdf-process
      '("mkdir -p build" "latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o/build %f" "mv %o/build/%b.pdf %O"))

;; (COMMENTED OUT - REPLACED BY ABOVE)
;(setq org-latex-pdf-process (list "latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o %f"))

;; (COMMENTED OUT - REPLACED BY ABOVE)
;(setq org-latex-pdf-process
;      '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"
;        "latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"
;        "latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))

;; PREVIOUS SETTINGS TRIED:
;;   "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" . . .
;;   "luatex -shell-escape -interaction nonstopmode %f" . . .
;;   "luatex -shell-escape -interaction nonstopmode -output-directory %o %f" . . .
;;   "xelatex -interaction nonstopmode %f" . . .
;;   "latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f" . . .
;;   "latexmk -bibtex -f %f" . . .
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  BEGIN: CUSTOM LaTeX CONFIGURATIONS for EXPORT to PDF
;;
;; NOTE: The Following Packages ARE NEEDED & INCLUDED by DEFAULT For
;;       LaTeX Export of .org files...
;;
;;       These Package Values are Stored in the Variable:
;;              org-latex-default-packages-alist
;;
;;
;; CURRENT org-latex-default-packages-alist STATE: 2025-003-21
;;
;;    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;     OPTION:   PACKAGE:  SNIPPET:  COMPILER:    PURPOSE:
;;    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;     AUTO      inputenc    t       pdflatex     font and char. AUTO=[utf8]? 
;;     T1        fontenc     t       pdflatex     font and character selection
;;               graphicx    t       ALL          including images
;;               longtable   nil     ALL          multi page tables
;;               wrapfig     nil     ALL          figure placement
;;               rotating    nil     ALL          figures & tables
;;     normalem  ulem        t       ALL          underline & strike-through
;;               amsmath     t       ALL          subscript, superscript & math
;;               amssymb     t       ALL          various symbols (org-entities)
;;               capt-of     nil     ALL          captions outside of floats
;;               hyperref    nil     ALL          cross references

;; You MAY GLOBALLY DISABLE these default packages by using the following
;; macro-like placeholder:    [NO-DEFAULT-PACKAGES]
;; (i.e., DO NOT Include ANY DEFAULT PACKAGES)

;; NOTE: The Org Manual WARNS you to NOT disable these packages because
;;       They are NEEDED for Default Org-Mode LaTeX Processing...
;;       i.e., If you muck with these it will force you to do MUCH MORE
;;       CUSTOM LaTeX Work on your own... (NO MORE "it just works")

;; THE FOLLOWING CODE SNIPPET CAN BE USED AS A TEMPLATE To UPDATE
;; org-latex-default-packages-alist TO REMOVE ANY PACKAGES that CONFLICT
;; with PACKAGES in YOUR PREAMBLE... (and you have been warned LOL)

;; TODO: What does AUTO mean? Is inputenc set to utf8??? VERIFY...
;;       ANSWER: (I believe) is YES... I got UTF8 set ALL OVER THE PLACE... LOL
;;               (SO... Simply Ignore those DUMB LaTeX Warnings...)
;;               I can see all the ODD utf8 glyphs rendering/type-setting fine)
;;               (i.e., as long as the language package for them exists)

;(setq org-latex-default-packages-alist
;      '(("AUTO"     "inputenc"      t ("pdflatex")) 
;	 ("T1"        "fontenc"      t ("pdflatex"))
;	 (""          "graphicx"     t)
;        (""          "longtable"  nil)
;        (""          "wrapfig"    nil)
;        (""          "rotating"   nil)
;        ("normalem"  "ulem"         t)
;        (""          "amsmath"      t)
;        (""          "amssymb"      t)
;        (""          "capt-of"    nil)
;        (""          "hyperref"   nil)))

;;       
;; IN ADDITION: These macro-like Placeholders can be used as well:
;;
;;       [DEFAULT-PACKAGES]      USE Package Statements For Default Packages
;;       [PACKAGES]              USE Package Statements For Packages
;;       [NO-PACKAGES]           DO NOT Include The Packages
;;       [EXTRA]                 INCLUDE: #+LaTeX_HEADER Directives
;;       [NO-EXTRA]              DO NOT Include #+LaTeX_HEADER Directives
;;       [BEAMER-HEADER-EXTRA]   INCLUDE: Beamer EXTRA Headers
;;
;; UPDATE 2022-012-20: TWO NEW LaTeX eBook STYLES were CREATED below:
;;
;;       "fictbook" (For Fiction (Chapter Numbering only - NO TOC)
;;
;;       "refbook" for Reference Manuals, Technical Books, Tutorials etc.
;;                 (With Chapter & Sub-Topic Section Numbering AND TOC)
;;
;;       "logbook" for LOG NOTEBOOKS...
;;                 (With Chapter & Sub-Topic Section Numbering AND TOC)
;;
;; These new styles are based on memoir class...
;; The code for: refbook started out as a clone of fictbook...
;; fictbook and refbook will diverge over time...
;;
;; UPDATE 2025-003-24: EVERYTHING IS CHANGING NOW... lol
;; BUT we are getting there...
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ADJUST hyperref options to use color links (not ugly BOXES)
;; NOTE: THIS IS GLOBAL FOR ALL ORG-MODE -> PDF exports (and HTML) WHEN 
;; the CLASS DEFINITIONS BELOW are loaded with the [DEFAULT-PACKAGES]
;; placeholder... This List is no longer org-mode DEFAULT
;; NOW it is HAP Modular Emacs DEFAULT... Simply comment this form
;; if you wish to get BACK the ORG MODE Out-of-Box DEFAULT...

(setq org-latex-default-packages-alist
      '(("AUTO"             "inputenc"     t ("pdflatex")) 
	("LGR, T1"          "fontenc"      t ("pdflatex"))
	(""                 "graphicx"     t)
        (""                 "longtable"  nil)
        (""                 "wrapfig"    nil)
        (""                 "rotating"   nil)
        ("normalem"         "ulem"         t)
        (""                 "amsmath"      t)
        (""                 "amssymb"      t)
        (""                 "capt-of"    nil)
        ("colorlinks=true" "hyperref"   nil)))
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  BEGIN: *** ORG LATEX CLASS DEFINITIONS LIST ***
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(with-eval-after-load 'ox-latex

  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nil))

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; BEGIN: (CUSTOMIZED org-latex-classes LIST)
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
  ;; NOTE:
  ;;  PLEASE EXCUSE MY NON STANDARD eLisp formatting below...
  ;;  (i.e. nested closing parenthesis on lines by themselves)
  ;;  I did this here BECAUSE I wanted to keep each LaTeX CLASS
  ;;  Definition looking like a separate block alone rather
  ;;  than PART of a really LONG LIST (including my comments
  ;;  LOL)... I mean L O N G List... So please excuse my non
  ;;  standard styling here... Thank you! I promise to say 12
  ;;  Hail Marys as a penance for my wrong doings... (Jeez..
  ;;  where did I put that rosary?) Since childhood I have
  ;;  kept that cursed thing...
  ;;
  ;;  One GOOD elisp thing I did do was REMOVE as MUCH of the
  ;;  embedded LaTeX code (within eLisp forms below) which,
  ;;  IMHO is UGLIER THAN HELL ITSELF!!! (and I have been
  ;;  there so I know) I created .TEX files which are \\input
  ;;  in the forms immediately to keep as many UGLY cryptic
  ;;  escape characters out of here as possible...
  ;;
  ;;  I like the results... much SIMPLER to read and you KNOW
  ;;  where that raw LaTeX code is that does all the magic...
  ;;  (i.e., elsewhere) like it should be...
  ;;
  ;;  Once the code below is all set you can leave it alone...
  ;;  These are placeholder ENTRY POINTS from Org-Mode to LaTeX
  ;;  processing... That's it.. 
  
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  ;; EXCERPT DOCUMENT CLASS: 
  ;;
  ;; AUTHOR:   Alisha Awen
  ;; REF:      Harmonic Alchemy Productions - Modular Emacs
  ;; UPDATED:  2023-006-04
  ;;
  ;; PURPOSE: 
  ;;
  ;;  This is currently being developed to be used with:
  ;;  HAP_PDF-Example-Figure-Template.org A NEW LaTeX configuration
  ;;  for simple PDF excerpts particularly suited for figures or
  ;;  examples that may or may not be originally from a larger
  ;;  Document or Book Project...
  ;;
  ;;  These are most commonly created to serve in support of a
  ;;  larger project, But Publishable as a Simple STAND ALONE PDF
  ;;  when that's the only thing needed to look up while deep inside
  ;;  a project with many docs open. You know what I mean! %^)

  (add-to-list
   'org-latex-classes
   '("excerpt"
      "\\documentclass[12pt, a5paper]{article}
         [NO-DEFAULT-PACKAGES]
         [PACKAGES]
         [EXTRA]
       \\input{~/.emacs.d/Docs/TeX/pdf-excerpt-setup.tex}"

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
  ;;  This is currently being developed to be used with:
  ;;  HAP_Simple-Document-Skeleton.org (A LaTeX configuration for
  ;;  short works like README docs, abstracts, short papers, and
  ;;  quick documentation for things not needing a more complex format...
  ;;  bug fixes and refinements are currently ongoing... 

  (add-to-list
   'org-latex-classes
   '("simple"
      "\\documentclass[12pt, a4paper]{article}
         [DEFAULT-PACKAGES]
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
  ;; NOTE: This class definition was copied from the ref
  ;;       BELOW... I have not created any .tex file to
  ;;       \\input{...} yet... This class is not being used
  ;;       currently... If I develop it further you will see
  ;;       this NOTE change...
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
  ;;  This has not been fully implemented or tested
  ;;  since making big changes to Org-Mode Export
  ;;  configuration... It needs to be tried again later.

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
  ;; NOTE: This class definition was copied from the ref BELOW...
  ;;       It is NOT currently being USED...
  ;;       If I develop it further you will see this NOTE change...
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
  \\input{/Users/sirena128/.emacs.d/Docs/TeX/notes-setup-file.tex}"
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
  ;; NOTE: This class definition was copied from the ref BELOW...
  ;;       It is NOT currently being USED...
  ;;       If I develop it further you will see this NOTE change...
  ;;
  ;; REF:
  ;;   https://damitr.org/2014/01/09/latex-tufte-class-in-org-mode/
  ;;   https://tufte-latex.github.io/tufte-latex/
  ;;   https://github.com/Tufte-LaTeX/tufte-latex
  ;;
  ;; TODO:
  ;;  This has not been fully implemented or tested since making
  ;;  big changes to Org-Mode Export configuration... It needs
  ;;  to be tried again later...

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
  ;; NOTE: This class definition was copied from the ref BELOW...
  ;;       It is NOT currently being USED...
  ;;       If I develop it further you will see this NOTE change...
  ;;
  ;; REF:
  ;;   https://damitr.org/2014/01/09/latex-tufte-class-in-org-mode/
  ;;   https://tufte-latex.github.io/tufte-latex/
  ;;   https://github.com/Tufte-LaTeX/tufte-latex
  ;;
  ;; TODO:
  ;;  This has not been fully implemented or tested since making
  ;;  big changes to Org-Mode Export configuration... It needs
  ;;  to be tried again later...

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
  ;;  TEST REF BOOK DOCUMENT CLASS: (uses stock memoir)
  ;;
  ;;  THIS IS A DEFAULT MEMOIR LaTeX Class out-of-box 
  ;;  CONFIGURATION... (for TESTING PURPOSES) 
  ;;
  ;;  This is for TROUBLESHOOTING Customized Memoir Class
  ;;  configurations for Reference Manuals, Technical Books,
  ;;  Tutorials, etc... 
  ;;
  ;;  THIS LATEX CLASS is Associated with Org-Mode SETUPFILE:
  ;;  ~/.emacs.d/Docs/pubOps/org-templates/test-memoir-setup.org
  ;;  THERE ARE NO EXTERNAL \\input .tex files for this one... 
  ;;
  ;;  Call this setup file from .org files that normally use
  ;;  CUSTOMIZED Setupfiles for Memoir Class... (i.e., replace
  ;;  the SETUPFILE directive at the top of the file to the
  ;;  test-memoir-setup.org SETUPFILE above...
  ;;  USE THIS as an A-B TEST to see what BREAKS)

  (add-to-list
   'org-latex-classes
   '( "memoir"
      "\\documentclass{memoir}
          [DEFAULT-PACKAGES] 
          [PACKAGES] 
          [EXTRA]" 
     ("\\part{%s}" . "\\part*{%s}")
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;; ~~~~~~~ END: TEST MEMOIR REF BOOK DOCUMENT CLASS: ~~~~~~~~

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;;  HAP REFERENCE BOOK DOCUMENT CLASS:
  ;;  (refbook - uses memoir class)
  ;;  For Reference Manuals, Technical Books, Tutorials, etc...
  ;;
  ;;  THIS LATEX CLASS is Associated Org-Mode SETUPFILE:
  ;;  ~/.emacs.d/Docs/pubOps/org-templates/ref-book-setup.org
  ;;  AND ANY .org file that includes ref-book-setup.org...
  ;;  \documentclass[letterpaper, 12pt, oneside, onecolumn, openany]{memoir}

  (add-to-list
   'org-latex-classes
   '( "refbook"
      "\\documentclass[letterpaper,12pt,oneside,onecolumn,openany]{memoir} 
          [DEFAULT-PACKAGES] 
          [PACKAGES] 
       \\input{~/.emacs.d/Docs/TeX/ref-book-setup.tex} 
          [EXTRA]" 
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
  ;;  MemoirChapStyles.pdf (from CTAN)

  (add-to-list
   'org-latex-classes
   '("audio-production"
     "\\documentclass[letterpaper,12pt,oneside,onecolumn,openany]{memoir}
          [DEFAULT-PACKAGES]
          [PACKAGES]
       \\input{~/.emacs.d/Docs/TeX/audio-drama-setup.tex}
          [EXTRA]"
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
  ;;  MemoirChapStyles.pdf
  ;;  This Class Defines a new Modular Emacs Logbook Chapter Style (logbook)
  ;;  LaTeX Configuration is done in Input File: log-book-setup.tex

  (add-to-list
   'org-latex-classes
   '("logbook"
     "\\documentclass[ledgerpaper,12pt,oneside,onecolumn,openany]{memoir} 
         [DEFAULT-PACKAGES] 
         [PACKAGES] 
       \\input{/Users/sirena128/.emacs.d/Docs/TeX/log-book-setup-3.tex} 
         [EXTRA]" 
     ("\\part{%s}" . "\\part*{%s}") 
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

  ) ;; END: (with-eval-after-load 'ox-latex) SORRY! I know... Not proper eLisp Styling... 

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  END: *** ORG LATEX CLASS DEFINITIONS LIST ***

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   END: [modular-emacs]:~/.emacs.d/lisp/modules/09-4-org-export-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
