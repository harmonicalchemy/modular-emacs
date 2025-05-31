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
;; NOTE: Currently NOT using markdown mode or any of its features... I use external app
;; to view markdown... And I DON'T use it directly... I use ORG mode for EVERYTHING now...
;; Fountain is considered a variant of markdown... I do USE that... Org Mode supports it
;; in source code blocks... Getting Fountain script to LaTeX is still an ongoing research task...

(defvar me--req-pubops-packages
  '(markdown-mode
    auctex
    fountain-mode
    graphviz-dot-mode
    csv-mode
    simple-httpd))

;;  NOTE: pandoc-mode  (Was Removed because it requires DASH and other "MODERN SHIT"
;;        I don't even think I use pandoc-mode in emacs anyway... so this is fantastic!

;;;
;; Install required packages:

(mapc (lambda (p)
        (package-install p))
      me--req-pubops-packages)
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  DEFAULT LaTeX Mode SETTINGS:

(setq TeX-PDF-mode t)
(setq TeX-auto-save t)
(setq TeX-fold-mode 1)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq reftex-plug-into-AUCTeX t)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  LaTeX MODE HOOK TWEAKS:

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

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

;;;; ~~~~~~~~~~~~~~~~~~~~~
;;   Pandoc Mode:
;;
;;   NOTE: pandoc-mode Was Removed because it requires DASH and other "MODERN SHIT"
;;        I don't even think I use pandoc-mode in emacs anyway... so this is fantastic!
;;;; ~~~~~~~~~~~~~~~~~~~~~

;;;
;; NOTE: On Mac OS, Depending on how you installed PanDoc,
;; you may need one of the following: (I enabled the MacPorts form for my setup)

;;;; ~~~~~~~~~~~~
;; Mac OS Case:

;; (when ME--DARWIN
;;   ;; You may need to enable the first form if you used HomeBrew
;;   ;; to install Pandoc...  If you enable the first form, be sure to comment
;;   ;; out (disable) the second form below it and visa-versa!!
;;   ;; Your Pandoc was installed by HomeBrew.  Ensure Emacs gets the path...
;;   ;(setq pandoc-binary "/usr/local/bin/pandoc"))
;;   ;; Your Pandoc was installed by MacPorts.  Ensure Emacs gets the path...
;;   (setq pandoc-binary "/opt/local/bin/pandoc"))


;;;; ~~~~~~~~~~~~
;; Linux Case:

;; (when ME--LINUX
;;   (setq pandoc-binary "~/.local/bin/pandoc"))

;;;
;; Add Pandoc Mode to all Markdown Files:
;; Ref: https://joostkremers.github.io/pandoc-mode/

;; (add-hook 'markdown-mode-hook 'pandoc-mode)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Graphviz-dot-mode Customizations:
;;
;; I only changed the default output from .png to .svg...
;; That is my personal prefrence... Comment out if you like .png better...
;; Also add other custom features here to this hook as well if you like...
;; If there are any custom keys set for graphviz-dot-mode, you will find
;; those defined within 10-key-bindings.el...

;; DISABLED for DEBUGGING  2025-004-04: 
;(defun me_graphviz-tweaks ()
;  (setq graphviz-dot-preview-extension "svg"))
;
;(add-hook 'graphviz-dot-mode-hook 'me_graphviz-tweaks)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Asymptote Configuration:
;;
;; Asymptote is a Vector Graphics Language 
;; asy-mode - Enables Editing Source Code and Processing for Asymptote...
;; (So far the path below is for Fedora as installed by DNF)

(add-to-list 'load-path "/opt/local/share/asymptote")
(autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode.el" "hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
(add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   END: [modular-emacs]:~/.emacs.d/lisp/modules/05-pubOps-pkg-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
