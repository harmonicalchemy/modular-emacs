;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/08.spelling.el
;;
;; This module encapsulates spelling features making it easier to turn
;; specific features on or off, choose to use Aspell or Hunspell or both,
;; switch dictionaries, languages. etc. all within this one module...
;;
;; I use aspell instead of Hunspell because it is considered by may to be better
;; for code...  I am also a writer as well so we will see if it is also good for
;; that...  (given some time using it)
;;
;; Currently I do not have Hunspell installed so functions Below will follow the
;; Aspell conditions only...
;;
;; Ref: http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;;
;; Function: flyspell-detect-ispell-args (&optional run-together)
;;
;; Pseudo Code Synopsis:
;;
;;   (flyspell-detect-ispell-args ()
;;     if (Aspell installed)
;;        use Aspell
;;     else if (Hunspell installed)
;;        use Hunspell
;;
;;     Always use Canadian English dictionary either case)

(defun flyspell-detect-ispell-args (&optional run-together)
  "If RUN-TOGETHER is true, spell check the CamelCase words.
Please note RUN-TOGETHER will make aspell less capable. So it should only be used in prog-mode-hook."
  (let* (args)
    (when ispell-program-name
      (cond
       ((string-match "aspell$" ispell-program-name)
        ;; force the English dictionary, support Camel Case spelling check
        ;;n(tested with aspell 0.6)
        (setq args (list "--sug-mode=ultra" "--lang=en_CA"))
        ;; "--run-together-min" could not be 3, see `check` in
        ;; "speller_impl.cpp".  The algorithm is not precise.
        ;; Run `echo tasteTableConfig | aspell --lang=en_CA -C
        ;;     --run-together-limit=16  --encoding=utf-8 -a`
        ;; in shell.
        (if run-together
            (setq args (append args '("--run-together" "--run-together-limit=16")))))
       ((string-match "hunspell$" ispell-program-name)
        (setq args nil))))
    args))

;; Don't bother testing for aspel or Hunspell, Just set the variable... Just make sure
;; to install aspell globally on the OS!
;; Important Note!: Realize this breaks if you don't have aspell installed in the environment!
;; Note to self: Add warning in the README.md file!  Don't forget. ;-)

(setq ispell-program-name "aspell")

;; Set dictionarys...

(setq ispell-local-dictionary "en_CA")
(setq ispell-local-dictionary-alist
      '(("en_CA" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_CA") nil utf-8)))

;; According to the above linked article: ispell-cmd-args are useless...
;; it's the list of *extra* arguments we will append to the ispell
;; process when "ispell-word" is called.
;;
;; On the other hand, ispell-extra-args are the command arguments which
;; will *always* be used when we start the ispell process...
;;
;; Please Note: When you use hunspell, ispell-extra-args will NOT be used.
;;
;; OK Got that?... Lets Hack ispell-local-dictionary-alist instead:

(setq-default ispell-extra-args (flyspell-detect-ispell-args t))

;; (setq ispell-cmd-args (flyspell-detect-ispell-args))

(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)))

(defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)))

(defun text-mode-hook-setup ()
  ;; Turn off RUN-TOGETHER option when spell check text-mode
  (setq-local ispell-extra-args (flyspell-detect-ispell-args)))

(add-hook 'text-mode-hook 'text-mode-hook-setup)

;; turn on flyspell in desired modes

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Set Keybindings:
;; You can also use "M-x ispell-word" or hotkey "M-$".
;; It will pop up a multiple choice box.
;; @see http://frequal.com/Perspectives/EmacsTip03-FlyspellAutoCorrectWord.html

(global-set-key (kbd "C-c s") 'flyspell-auto-correct-word)

;; Flyspell Correct Helm key binding:
(require 'flyspell-correct-helm)
(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; change case of letters:
;; http://ergoemacs.org/emacs/modernization_upcase-word.html
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word)))
        (setq p1 (car bds) p2 (cdr bds))))

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps"))
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps"))
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps"))
         (t (put this-command 'state "all lower")))))

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")))))

;; Set Global key for Letter Case Toggle to M-c:

(global-set-key "\M-c" 'toggle-letter-case)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/08.spelling.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
