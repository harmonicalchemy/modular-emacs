;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;  [Modular-Emacs]:~/.emacs.d/lisp/modules/13-key-bindings.el

;;
;; Change Log: (descending chronological order)
;;

;; 2022-009-18 - Alisha Awen, siren1@disroot.org
;;   Changed Harmonic Alchemy Modular Emacs TO: v3.5...
;;   Added F1 key to invoke "wo-man" (Emacs Without Man Pages - Info Style)
;;   (stupid name but it works to get all those MAN pages into Emacs)
;;   I don't have any CUSTOM configurations for WOMAN yet...
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Map Modular Emacs General Keys:

(defvar me--my-keyboard-bindings
  '(("C-c 0" . deft)
    ("C-c 1" . org-sidebar-tree-toggle)
    ("C-c 3" . org-sidebar-toggle)
    ("C-c 2" . me_make-frame)
    ("C-c =" . emms-volume-mode-plus)
    ("C-c -" . emms-volume-mode-minus)
    ("C-c <tab>" . mweb-set-extra-indentation)
    ("C-c w" . mweb-set-default-major-mode)
    ("C-c o" . olivetti-mode)
    ("C-c a" . org-agenda)
    ("C-c c" . org-capture)
    ("C-c d" . neotree-toggle)
    ("C-x C-g" . deft-find-file)
    ("C-c i" . bookmark-bmenu-list)
    ("C-c m" . org-md-export-to-markdown)
    ("C-c s" . me_org-tree-open-in-right-no-focus) 
    ("C-c t" . me_org-toggle-blocks)
    ("C-c v" . vmd-mode)
    ("C-c r" . view-mode)
    ("C-c '" . imenu)
    ("C-c b" . other-frame)
    ("C-c p" . me_toggle-default-face)
    ("C-c u" . me_toggle-letter-case)
    ("C-c z" . ztree-diff)
    ("M-x" . helm-M-x)
    ("C-x b" . helm-mini)
    ("C-x C-f" . helm-find-files)
    ("C-x C-r" . helm-recentf)
    ("<f5>" . me_reload-current-file)))

(defun me_apply-keyboard-bindings (pair)
  "Apply keyboard-bindings for supplied list of key-pair values"
  (interactive)
  (global-set-key (kbd (car pair))
                  (cdr pair)))

(mapc 'me_apply-keyboard-bindings
      me--my-keyboard-bindings)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Flyspell Correct Previous - Helm key binding:

(require 'flyspell-correct-helm)
(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous)


;;;
;; Map Linux Alt keys to Emacs META:
;; Don't enable this next form as it is not working yet...
;; On Linux, I swapped Caps-Lock and Home keys using xmodmap to accommodate using
;; Xah Fly Keys in an efficient manner...  However, I tried many ways, including
;; full blown custom xkb keymap to swap my control and alt keys, (so it would be
;; the same keystrokes for me on both Mac and Linux ;-).  All that sweat to no
;; avail so far! ...but I will get it.. I was stuck and needed a good break...
;; The Mac has its quirks as well but Karabiner Elements saved the day!
;;
;; You may not need any of this anyway... Unless you also want to enable and use
;; Xah Fly Keys... Then you are in the same Linux boat as I am... ARR!!! Mate! %^)
;; This form will be enabled, (with the right settings) once I get all this working!
;; Stay Tuned...

;(when ME--LINUX
  ;(define-key key-translation-map (kbd "Control_L") (kbd "Alt_L"))
  ;(define-key key-translation-map (kbd "Alt_L") (kbd "Ccontrol_L"))
  ;(define-key key-translation-map (kbd "Control_R") (kbd "Alt_R"))
  ;(define-key key-translation-map (kbd "Alt_R") (kbd "Ccontrol_R"))
;  (setq x-alt-keysym 'control)
;  (setq x-ctrl-keysym 'meta)
;  (setq x-super-keysym 'meta))

;;;
;; For Mac OS - Set Mac Command key (both sides of space bar) to work as Emacs
;; Control Key...  Set normal keyboard control key to work as Emacs META:
;; Note: Don't enable this unless you like to have the control key on both
;; sides right next to your space bar (for Emacs only)...
;;
;; In addition to above, you have to install and configure Karabiner-Elements on
;; your Mac to remap some of your keys. I did all the above and also enable
;; Xah-Fly-Keys to make Emacs behave as a modal editor similar to the way Vi
;; works. After using this setup over my hands and fingers are now much less
;; stressed, and I can edit code, compose documents or write fiction faster than
;; ever!

(when ME--DARWIN
  (setq mac-command-modifier 'control)
  (setq mac-control-modifier 'meta))

;;;
;; Make Escape Key Do C-g:
;; This is a goodie I learned while setting up Xah-Fly-Keys.  It is not for
;; Xah Fly Keys users specifically, but rather a general purpose aid...

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Set F1 key to load man page for keyword at current cursor position (woman):
;; This key is universal...  Nice to have it in Emacs for Man Pages! Cool! 

(global-set-key (kbd "<f1>")
                (lambda ()
                  (interactive)
                  (let ((woman-use-topic-at-point t))
                    (woman))))

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/13-key-bindings.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
