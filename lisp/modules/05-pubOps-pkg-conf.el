;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/05-pubOps-pkg-conf.el
;;
;; This module adds extra tools for writing, viewing, and publishing to give you
;; a jump start at using Emacs as a great publishing platform.  Just as good or
;; in-my-opinion, better than using something like Open Office, MS Word, or even
;; something fancier like Scrivener!
;;
;; using vmd-mode in combination with olivetti, etc. provide you with both
;; WYSIWYG live (like standard word processors) but also very powerful macro
;; commands, ease of fast typing everything (no mouse needed) etc.  Which is
;; why I strongly feel Emacs is the best publishing tool out there for anything!

;;
;; Change Log: (descending chronological order)
;;

;; 2023-002-10 - Alisha Awen, siren1@disroot.org
;;   Removed DEFT I am NOT using it and it is a PAIN in the ASS!
;;   Sorry about that Jason Blevins... You did a lot great of work
;;   and contributed to Emacs but this thing drives me nuts!
;;   Also, DUDE!... Please Update Your Code... the cl (old common lisp lib)
;;   is depreciated! I am tired of seeing that in my startup logs...
;;
;; 2022-009-27 - Alisha Awen, siren1@disroot.org
;;   Changed Harmonic Alchemy Modular Emacs TO: v3.5...
;;   No Longer Using mmd-mode (IMHO it is not worth the effort)...
;;   Removed VMD-MODE completely. (VMD has critical security problem)
;;   I removed ALL NODE JS from my machines and removed its requirement
;;   from HAP Modular Emacs as well...  No more HODE JS Period...
;;   The Only Reason NODE was installed was for VMD actually.  Not
;;   developing JS apps so NODE and all the FLUF it comes with is NOT
;;   needed here...  (if you do JS Dev and need it you probably already
;;   have it)
;;
;;   Also commented out Pandoc Code to see if that may have been
;;   causing any problems... IF it is, it may ALSO be removed completely
;;   later... Pandoc as an external tool works great...
;;   Not sure if Emacs configuration with pandoc works well at this point...
;;   MORE RESEARCH NEEDED...
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;; Create repositories cache for pubOps extras, if required:

(when (not package-archive-contents)
  (package-refresh-contents))

;;;
;; Declare a list of required packages for writers, bloggers, publishers, etc.:
;; NOTE: Removed: markdown-mode+ (no longer valid)

(defvar me--req-pubops-packages
  '(markdown-mode
    markdown-toc
;    deft         ;; I am removing this... It is a Pain and causes Errors!
    auctex
    pandoc-mode
    fountain-mode
    graphviz-dot-mode
    csv-mode))

;;;
;; Install required packages:

(mapc (lambda (p)
        (package-install p))
      me--req-pubops-packages)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Fountain Mode Tweaks:

(custom-set-faces
 '(fountain ((t (
                 :height 130
                 :family "Courier Prime")))))

(custom-set-faces
 '(fountain-dialog ((t (
                        :height 130
                        :family "Courier Prime"
                        :inherit
                        (font-lock-string-face))))))

;;;
;;  ME Fountain Mode Hook Function:

(defun me_fountain-mode-hook ()
  "Customize some things to do when fountain mode starts up"
  (interactive)
  (imenu-add-menubar-index)
  (fountain-completion-update)
  (fountain-outline-hide-custom-level)
  (olivetti-mode)
  ;; Add olivetti mode options for fountain mode here:
  (olivetti-set-width 80))

(add-hook 'fountain-mode-hook 'me_fountain-mode-hook)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; mmd-mode - Multimarkdown extensions to markdown-mode:
;; Reference: GitHub:jmquigley/mmd-mode
;;
;; NOTE: This Package is not being used anymore... I may
;;       Remove this commented-out section alltogether soon.
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; (add-to-list 'load-path "{~/.emacs.d/lisp/my-modules/mmd-mode}")
;; (require 'mmd-mode)

;; (add-to-list 'auto-mode-alist '("\\.md\\'" . mmd-mode))
;; (add-to-list 'auto-mode-alist '("\\.mdwn\\'" . mmd-mode))
;; (add-to-list 'auto-mode-alist '("\\.mdt\\'" . mmd-mode))
;; (add-to-list 'auto-mode-alist '("\\.mmd\\'" . mmd-mode))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; 2023-002-10: REMOVED FROM MODULAR_EMACS - No longer using this...
;; Use Deft mode for quickly browsing, filtering, and
;; editing directories of plain text notes...
;; Deft was created by Jason Blevins. What a clever dude who
;; besides being an Emacs wizard. He is also a wizard in the
;; fine art of statistics! And looks to also dabble with
;; economics, money, crypotcurrency, etc...
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;(require 'deft)

;;;;
;; 2023-002-10: REMOVED FROM MODULAR_EMACS - No longer using this...
;; Enable/edit these next several lines to Customize items in
;; the deft group to change default functionality...
;;
;; Adjust path to the place where much of your Markdown docs live:
;; Change the placeholder paths below to a location where you keep your library
;; of markdown files. If you don't have something like that set up already,
;; create a place for that now.  DEFT will give you a nice for navigating your
;; markdown and/or org docs...  If you use Harmonic Alchemy .DOTFILES, You are
;; all set with the definition below as you should (by folloing instructions
;; within the .DOTFILES readime, will have ~/.MD properly defined for your
;; environment for DEFT to work with.... The options below work on both MacOS
;; and Linux systemsa...  *Your Mileage may Vary depending on your use-case
;; however not something that could be more easily remedied with simple
;; symlinks set up within your $HOME directory on each different platform
;; you run Harmonic Alchemy Modular Emacs on...
;;;;

;;;
;; 2023-002-10: REMOVED FROM MODULAR_EMACS - No longer using this...
;; Virtual Path to My-Docs on All Platforms:
;; NOTE: For new OS installs, make sure ~/.MD is defined (either as directory
;;       or a symlink to where your docs actually live...)

;(setq deft-directory "~/.MD")

;(setq deft-recursive t)
;(setq deft-use-filename-as-title t)
;(setq deft-file-naming-rules
;      '((noslash . "-")
;        (nospace . "-")))
;(setq deft-markdown-mode-title-level 1)
;(setq deft-org-mode-title-prefix t)

;;;
;; 2023-002-10: REMOVED FROM MODULAR_EMACS - No longer using this...
;; Associate file types for deft to process:
;; The default is "txt" "md" "org".  I have added Tex (LaTex files)
;; You may add more here if you work with other formats.

;(setq deft-extensions '("txt" "md" "tex" "org" "fountain"))

;;;
;; 2023-002-10: REMOVED FROM MODULAR_EMACS - No longer using this...
;; Set deft to do regexp search instead of incremental:

;(setq deft-org-mode-title-prefix nil)

;;;
;; 2023-002-10: REMOVED FROM MODULAR_EMACS - No longer using this...
;; I don't want auto save because I open many notes for viewing only.
;; (Not to edit them but to read as reference, etc.  If this is not
;; disabled, I end up with a bunch of un-tracked files in my git
;; repository that holds these notes!

;(setq deft-auto-save-interval 0)

;;;
;; 2023-002-10: REMOVED FROM MODULAR_EMACS - No longer using this...
;; Also, I had to add this deft-mode-hook function because lines were wrapping.
;; I tried everything else to no avail... This is a band-aid.  I need
;; to consult with Jason Blevins about this one %^)

;(defun me_deft-init ()
;  (setq truncate-lines t))

;(add-hook 'deft-mode-hook 'me_deft-init)


;;;; ~~~~~~~~~~~~~~~~~~~~~
;;   Pandoc Mode:
;;;; ~~~~~~~~~~~~~~~~~~~~~

;;;
;; NOTE: On Mac OS, Depending on how you installed PanDoc,
;; you may need one of the following: (I enabled the MacPorts form for my setup)

;;;; ~~~~~~~~~~~~
;; Mac OS Case:

(when ME--DARWIN
  ;; You may need to enable the first form if you used HomeBrew
  ;; to install Pandoc...  If you enable the first form, be sure to comment
  ;; out (disable) the second form below it and visa-versa!!
  ;; Your Pandoc was installed by HomeBrew.  Ensure Emacs gets the path...
  ;(setq pandoc-binary "/usr/local/bin/pandoc"))
  ;; Your Pandoc was installed by MacPorts.  Ensure Emacs gets the path...
  (setq pandoc-binary "/opt/local/bin/pandoc"))


;;;; ~~~~~~~~~~~~
;; Linux Case:

(when ME--LINUX
  (setq pandoc-binary "~/.local/bin/pandoc"))

;;;
;; Add Pandoc Mode to all Markdown Files:
;; Ref: https://joostkremers.github.io/pandoc-mode/

(add-hook 'markdown-mode-hook 'pandoc-mode)


;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   Graphviz-dot-mode Customizations:
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;; I only changed the default output from .png to .svg...
;; That is my personal prefrence... Comment out if you like .png better...
;; Also add other custom features here to this hook as well if you like...
;; If there are any custom keys set for graphviz-dot-mode, you will find
;; those defined within 10-key-bindings.el...

(defun me_graphviz-tweaks ()
  (setq graphviz-dot-preview-extension "svg"))

(add-hook 'graphviz-dot-mode-hook 'me_graphviz-tweaks)

;;;
;; Asymptote Configuration - Asymptote is a Vector Graphics Language 
;; asy-mode - Enables Editing Source Code and Processing for Asymptote...
;; (So far the path below is for Fedora as installed by DNF)

(add-to-list 'load-path "/usr/share/asymptote")
(autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode.el" "hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
(add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   END: [modular-emacs]:~/.emacs.d/lisp/modules/05-pubOps-pkg-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
