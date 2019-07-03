;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/10-key-bindings.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun me-insert-backtick-quote () (interactive) (xah-insert-bracket-pair "`" "`"))

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
    ("<f5>" . modular-emacs-reload-current-file)
    ;; Reassigned Xah Keys:
    ("C-1" . previous-error)            ; For Debugging - Was C-3
    ("C-2" . next-error)                ; For Debugging - Was C-4
    ("C-3" . xah-previous-emacs-buffer) ; For Debugging - Was C-5
    ("C-4" . xah-next-emacs-buffer)     ; For Debugging - Was C-6
    ;; Modular Emacs New Assigned Xah Keys:
    ("C-5" . xah-insert-square-bracket)       ; [] - This is new
    ("C-6" . xah-insert-paren)                ; () - This is new
    ("C-7" . xah-insert-brace)                ; {} - This is new
    ("C-8" . xah-insert-double-curly-quote“”) ; “” - This is new
    ("C-9" . me-insert-backtick-quote)        ; `` - This is new
    ("C-0" . xah-select-text-in-quote)))       ; This one is wicked HOT!

(defun modular-emacs-apply-keyboard-bindings (pair)
  "Apply keyboard-bindings for supplied list of key-pair values"
  (global-set-key (kbd (car pair))
                  (cdr pair)))
 
(mapc 'modular-emacs-apply-keyboard-bindings
      modular-emacs--my-keyboard-bindings)

;;;;
;; Map Linux Alt keys to Emacs META:

(when *is-linux*
  (setq x-alt-keysym 'meta))

;;;;
;; Map Mac OS command-key to Emacs META:

(when *is-darwin*
  (setq mac-command-modifier 'meta))

