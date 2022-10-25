;; -*- coding: utf-8; lexical-binding: t; -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; File: ~/.emacs.d/lisp/modules/me-abbrev-defs.el
;; Ref: http://xahlee.info/emacs/emacs/emacs_abbrev_mode.html
;;
;; This File Contains Modular Emacs DEFAULT GP Emacs Abbrev Definitions...
;; Override this default by copying this file to:  ~/.emacs.d/lisp/my-modules/.
;; and then change anything below to suit your particular abbreviation needs...

;; NOTE1: Many of these below match Xah Lee's abbrevs making it easy for you to
;;        follow along with him during tutorials, and or Live Stream sessions...
;;        Consider that when modifying this file... (For the above pedagogical
;;        reason, it may be smarter to add your own new abbrevs to the bottom
;;        of the GLOBAL List and keep the existing definitions as they are)...

;; NOTE2: ME = Modular Emacs (used in comments below to distinguish between
;;        Modular Emacs Default Abbrevs and Xah Lee's Definitions from his
;;        Emacs Abbrevs Help Page)... ME used in file names, functions, and
;;        symbol names, means "Modular Emacs" or could also mean the word "me"
;;        (if you like to think of it that way)... i.e. me=you in your point
;;        of view ;-) 
;;
;; Once this file is loaded typing any of the abbreviations below and then
;; typing a space will automatically expand them.  If you DON'T WANT an abbrev
;; to expand for any reason you can stop expansion by pressing: C-q AFTER 
;; typing the Abbrev AND BEFORE typing a space or punctuation. Therefore,
;; You are NOT TRAPPED or Forced into using this speedy tool...
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(
    ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ;; NOTE: These are GLOBAL Abbrev Definitions taken directly from this page:
    ;;          http://xahlee.info/emacs/emacs/emacs_abbrev_mode.html
    ;;       IT may be wise NOT to change many of these (except personal ones)
    ;;       and instead add Your own section at the BOTTOM of this list
    ;;       where noted... Closing Parenthesis for this list are on a blank line
    ;;       to make it easy to add more forms with out breaking this list. ;-)

    ;; English Word Abbrevs:

    ("arg" "argument")
    ("bc" "because")
    ("bg" "background")
    ("bt" "between")
    ("paren" "parenthesis") ;; ME added this one... (not xah-lee)

    ;; English Abridged Words:

    ("cnt" "can't")
    ("ddnt" "didn't")
    ("dnt" "don't")

    ;; Phrase Abbrev:
    
    ("afaik" "as far as i know")
    ("atm" "at the moment")
    ("ty" "thank you")
    ("btw" "by the way")

    ;; Computing:
    
    ("cfg" "context-free grammar")
    ("cs" "computer science")

    ;; Tech Company:
    
    ("gc" "Google Chrome")
    ("macos" "macOS")
    ("msw" "Microsoft Windows")
    ("glx" "GNU Linux")            ;; ME added this one... (not xah-lee)
    ("bsx" "BSD Unix")             ;; ME added this one... (not xah-lee)
    ("onx" "Other Unix")           ;; ME added this one... (not xah-lee)

    ;; Programing:
    
    ("ipa" "IP address")
    ("jvm" "Java Virtual Machine")
    ("rsi" "Repetitive Strain Injury")
    ("subdir" "subdirectory")
    ("db" "database")

    ("evp" "environment variable")
    ("guip" "graphical user interface")
    ("oopp" "object oriented programing")
    ("osp" "operating system")

    ("eq" "==")
    ("r" "return")
    ("utf8" "-*- coding: utf-8 -*-")

    ;; Regex:
    
    ("azt" "\\([A-Za-z0-9]+\\)")
    ("brackett" "\\[\\([^]]+?\\)\\]")
    ("curlyt" "“\\([^”]+?\\)”")
    ("digitst" "\\([0-9]+\\)")
    ("datet" "\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)")
    ("strt" "\\([^\"]+?\\)")

    ;; Unicode:
    
    ("hr" "--------------------------------------------------")
    ("bu" "•")
    ("catface" "😸")
    ("octo" "🐙")    ;; ME added this one... (not xah-lee)
    ("aa" "👩‍💻 Alisha Awen @harmonicalchemy")  ;; ME added this one... (not xah-lee)
                                              ;; (Change this to your name and emoji)
    
    ("hearts" "♥💕💓💔💖💗💘💝💞💟💙💚💛💜")
    ("ra" "→")

    ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ;; ADD YOUR OWN ABBREV DEFINITIONS HERE:

    )) ;; END: global-abbrev-table

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Turn Abbrev Mode ON Globally...

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)
