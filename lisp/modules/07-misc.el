;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/07.misc.el
;;
;; This module is a catch-all for things that don't seem to fit in a specific
;; category.  More genearl nature...  I am using this module to troubleshoot
;; Mac OS problem of not getting the proper environment variables, (PATH etc.)
;; I still have not solved the Mac OS problem.  The "exec-path-from-shell"
;; related lines at the end of this file are my first attempt to fix this problem.
;; exec-path-from-shell did solve some environment problems on Linux however...
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
;; The default visible bell actually startles me when it occurs!
;; This is a much better way... Only the mode line flashes!  Great.
;; I found this clever snippit in a comment by: Phil@disqus_COwPSAc69c
;; on: Pragmatic Emacs.

(defun my-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(define-minor-mode my-visible-bell-mode
  "Use `my-visible-bell' as the `ring-bell-function'."
  :global t
  (let ((this 'my-visible-bell-mode))
    (if my-visible-bell-mode
        (progn
          (put this 'visible-bell-backup visible-bell)
          (put this 'ring-bell-function-backup ring-bell-function)
          (setq visible-bell nil
                ring-bell-function #'my-visible-bell))
      ;; Restore the original values when disabling.
      (setq visible-bell (get this 'visible-bell-backup)
            ring-bell-function (get this 'ring-bell-function-backup)))))

(setq visible-bell t)
(my-visible-bell-mode 1)

;; Do not invoke Debugger on Errors: key mistakes, etc...
;; (I prefer to do this manually only when there are real problems)
(setq debug-on-error nil)

