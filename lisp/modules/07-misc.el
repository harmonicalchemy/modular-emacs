;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/07.misc.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Reload File Function:
(defun modular-emacs-reload-current-file ()
  "Reload the file loaded in current buffer from the disk"
  (interactive)
  (cond (buffer-file-name (progn (find-alternate-file buffer-file-name)
                                 (message "File reloaded")))
        (t (message "You're not editing a file!"))))


;;Disable splash message, start *scratch* buffer by default
(setq initial-buffer-choice 
      t)
(setq initial-scratch-message 
      "")

;; Enable show-paren-mode
(show-paren-mode)

;;Enable winner-mode
(winner-mode t)

;;Enable windmove
(windmove-default-keybindings)

;; Highlight current line
(global-hl-line-mode +1)

;; Turn off highlight long lines
(setq whitespace-line-column 10000)

;; Change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Turn on Visual Line Mode for text modes only
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Load Environment Vars from shell:
;; If we are using unix in a POSIX compliant shell...
;; (e.g., OS X, Linux, BSD, with POSIX: Bash, or Zsh etc.)
;; Reference: GitHub:Purcell/exec-path-from-shell
;; Install: from MELPA exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("NVM_BIN"))
  )
