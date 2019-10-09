;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/13-key-bindings.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;(defun me-insert-backtick-quote () (interactive) (xah-insert-bracket-pair "`" "`"))

;;;;
;; Map Modular Emacs General Keys:

(defvar modular-emacs--my-keyboard-bindings
  '(("C-}" . mc/mark-next-like-this)
    ("C-{" . mc/mark-previous-like-this)
    ("C-|" . mc/mark-all-like-this)
    ("C->" . ace-jump-mode)
    ("C-<" . ace-jump-mode-pop-mark)
    ("C-'" . imenu-list-smart-toggle)
    ("M-/" . undo-tree-visualize)
    ("C-\," . neotree-toggle)
    ("C-c d" . deft)
    ("C-x C-g" . deft-find-file)
    ("C-c m" . org-md-export-as-markdown)
    ("C-c v" . vmd-mode)
    ("C-c r" . view-mode)
    ("C-c M-x" . execute-extended-command)
    ("M-x" . helm-M-x)
    ("C-x b" . helm-mini)
    ("C-x C-b" . helm-buffers-list)
    ("C-x C-f" . helm-find-files)
    ("C-x C-r" . helm-recentf)
    ("M-y" . helm-show-kill-ring)
    ("C-;" . ace-window)
    ("C-S-<up>" . buf-move-up)
    ("C-S-<down>" . buf-move-down)
    ("C-S-<left>" . buf-move-left)
    ("C-S-<right>" . buf-move-right)
    ("<f5>" . modular-emacs-reload-current-file)))

(defun modular-emacs-apply-keyboard-bindings (pair)
  "Apply keyboard-bindings for supplied list of key-pair values"
  (global-set-key (kbd (car pair))
                  (cdr pair)))
 
(mapc 'modular-emacs-apply-keyboard-bindings
      modular-emacs--my-keyboard-bindings)

;;;;
;; Map Linux Alt keys to Emacs META:

;(when *is-linux*
  ;(define-key key-translation-map (kbd "Control_L") (kbd "Alt_L"))
  ;(define-key key-translation-map (kbd "Alt_L") (kbd "Ccontrol_L"))
  ;(define-key key-translation-map (kbd "Control_R") (kbd "Alt_R"))
  ;(define-key key-translation-map (kbd "Alt_R") (kbd "Ccontrol_R")) )
;  (setq x-alt-keysym 'control)
;  (setq x-ctrl-keysym 'meta)
;  (setq x-super-keysym 'meta))

;;;;
;; Map Mac OS command-key to Emacs META:

(when *is-darwin*
  (setq mac-command-modifier 'control)
  (setq mac-control-modifier 'meta))

;;;;
;; Make Escape Key Do C-g:

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; function slime-space() is called from SPC key!
;; Stop SLIME from grabbing "SPC", (Xah-fly-keys Global Leader Key)
;; when in slime-mode or visiting any .lisp file...
;; This really cripples my ability to use Emacs now!  Yikes!

(defun override-slime-space-key-binding ()
  (define-key slime-mode-indirect-map (read-kbd-macro slime-space) nil)
  (define-key slime-mode-indirect-map (kbd "SPC")
    'xah-fly-leader-key-map))

(add-hook 'slime-mode-hook 'override-slime-space-key-binding)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/13-key-bindings.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
