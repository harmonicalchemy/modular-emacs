;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [machine-name]:~/.emacs.d/lisp/modules/06-interface.el
;;
;; This file has been modified from my original Super-Emacs fork.
;; A different theme (Blackboard) & Features/Settings have been
;; changed/added...
;;
;; Update 2019-002-21:
;;    Updated to Release Candidate 3 for Modular Emacs Version: 1.0 (Q1-2019)
;;
;; Update 2019-001-21:
;;    This file marks the first Release Candidate for Version 1 of:
;;    Harmonic Alchemy Modular Emacs (displayed in the default Emacs frame title) 
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Change title-bar text
(setq frame-title-format
      "Modular Emacs v1.0 (RC3) [Q1 2019]")

;; Disable tool-bar - yes... I could care less about tool bars in emacs!  an oxymoron!
(tool-bar-mode -1)

;; Disable scroll-bar - yes... I use my fingers to swipe, or just use key commands to move around...
(scroll-bar-mode -1)

(cua-mode t)
(setq cua-auto-tabify-rectangles nil)    ;; Don't tabify after rectangle commands
(transient-mark-mode 1)                  ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t)      ;; Standard Windows behavior

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Disable this section if you like using the mouse wheel:
;;
;; On my laptop my palms get in the way and cause all kinds
;; of scrolling crazy stuff. I hate it!!! Seriously. Key commands
;; are best in Emacs.  Right? I Mean Right??? Yup. ;-)
;; 
;; Disable mouse wheel (and two finger swipe) scrolling because
;; it scrolls horribly and I would rather work without it. %~)
;;
(mouse-wheel-mode -1)
(global-set-key [wheel-up] 'ignore)
(global-set-key [wheel-down] 'ignore)
(global-set-key [wheel-right] 'ignore)
(global-set-key [wheel-left] 'ignore)
(global-set-key [double-wheel-up] 'ignore)
(global-set-key [double-wheel-down] 'ignore)
(global-set-key [double-wheel-right] 'ignore)
(global-set-key [double-wheel-left] 'ignore)
(global-set-key [triple-wheel-up] 'ignore)
(global-set-key [triple-wheel-down] 'ignore)
(global-set-key [triple-wheel-right] 'ignore)
(global-set-key [triple-wheel-left] 'ignore)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Set default Frame Dimensions:
;; You may have to play with this depending on your total screen size etc...
(setq default-frame-alist
      '((width . 101)
        (height . 38)
        (menu-bar-lines . 1)))

;; Set default font to Hermit Medium (my favorite mono font for everything)...
(set-face-attribute 'default nil
                    :family "Hermit"
                    :foundry "PfEd"
                    :slant 'normal
                    :height 113
                    :weight 'normal
                    :width 'normal)

;; Activate blackbord theme...
(load-theme 'blackboard t)
