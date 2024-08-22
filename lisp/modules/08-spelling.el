;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/08-spelling.el
;;
;; This module encapsulates spelling features making it easier to turn
;; specific features on or off, choose to use Aspell or Hunspell or both,
;; switch dictionaries, languages. etc. all within this one module...
;;
;; I use aspell AND Hunspell because each is better for different things...
;; I am a writer and a coder... I need EVERYTHING - LOL
;;
;; Reference:
;;   https://www.emacswiki.org/emacs/FlySpell
;;   https://github.com/d12frosted/flyspell-correct
;;
;; CHANGE LOG: (descending chronological order)
;;

;; 2024-005-06 - Alisha Awen, HarmonicAlchemy@protonmail.com
;;    This update represents a COMPLETE rewrite of my spell checking
;;    config for Modular Emacs... It is Much simpler now and I only
;;    use Hunspell... I made this choice because using aspell is a PITA!
;;    to configure and then it still generates unwanted noise for suggestions!
;;    for strange words, camelCase, etc. Easier to simply add them to my personal
;;    dictionary as they come up...
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;; Create Repositories Cache, If Required:

(when (not package-archive-contents)
  (package-refresh-contents))

;;;
;; Declare Default Modular Emacs List of Required Packages:

(defvar me--required-packages
  '(flyspell-correct
    flyspell-correct-helm))

;;;
;; Install Required Packages:

(mapc (lambda (p) (package-install p))
      me--required-packages)

(require 'flyspell-correct-helm)


;; Flyspell Mode Keys:

(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-word-before-point)
                                                           ; default: flyspell-correct-wrapper)
                                                           ; alt1: flyspell-correct-word-before-point)
                                                           ; alt2: flyspell-auto-correct-previous-word)
;; Initialize Flyspell:

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;;
;; Order Spelling Corrections by Likeliness (not alphabetical - i.e. the default)

(setq flyspell-sort-corrections nil)

;;;
;; Do not print messages for every word:
;;    When checking the entire buffer, don’t print messages for every word.
;;    This is a major performance gain.

(setq flyspell-issue-message-flag nil)


;;;
;; UPDATE 2024-005-06 - NEW BARE BONES HUNSPELL SETUP for EMACS
;;   Use this simpler config below... It does not use aspell, camelCase,
;;   or run-together. I experienced to much noise with that setup...
;;
;; This configuration uses hunspell exclusively with multiple dictionaries
;;
;; Requirements: Make sure to install hunspell globally on the OS and its
;; associated dictionaries are also installed! Do All this via "macports" (yay),
;; "Homebrew" (yuck) or your "other" (Linux,BSD) OS pkg manager...
;;
;; Dictionaries installed by Macports:
;;
;;   /opt/local/share/hunspell/en_US-large
;;   /opt/local/share/hunspell/en_GB-large
;;   /opt/local/share/hunspell/en_CA-large

(with-eval-after-load "ispell"
  
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  
  (setenv "LANG" "en_CA.UTF-8")
  (setq ispell-program-name "hunspell")
  
  ;; Configure Two variants of English...
  
  (setq ispell-dictionary "en_CA-large,en_GB-large,en_US-large")
  
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_CA-large,en_GB-large,en_US-large")
  
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  
  (setq ispell-personal-dictionary "~/.hunspell_personal"))

;; The personal dictionary file has to exist, otherwise hunspell will
;; silently not use it.

(unless (file-exists-p ispell-personal-dictionary)
  (write-region "" nil ispell-personal-dictionary nil 0))

;; ALSO! Set Shell ENV VAR as well (this may be a caveat to overcome)

(setenv "DICTIONARY" "en_CA-large")

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Flyspell Mode Key Bindings: 
;;  Global flyspell-mode toggle & Correct-at-point
;;  NOTE: These Keybindings were MOVED to 13-key-bindings.el...

;(global-set-key (kbd "C-c f") 'flyspell-mode)
;(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)
;(global-set-key (kbd "C-;") 'flyspell-correct-word-before-point)
;(global-set-key (kbd "C-;") 'flyspell-auto-correct-previous-word)
;(global-set-key (kbd "C-;") 'flyspell-correct-wrapper)


;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Function:  me_toggle-letter-case ()
;;   Change case of letters, toggleing through
;;   three different options:
;;     (First Letter Only - all lower - ALL CAPS)
;; http://ergoemacs.org/emacs/modernization_upcase-word.html
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun me_toggle-letter-case ()
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

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/08-spelling.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
