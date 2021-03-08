;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; blackboard-theme.el - A Greatly Mangled Version of
;;                       The TextMate Blackboard Theme
;;                       (with Org Mode tweaks %^)
;;
;; Original MIT License Copyright (c) 2008 JD Huntington
;;    <jdhuntington at gmail dot com>
;;
;; Credits also due to the excellent TextMate Blackboard theme itself!!!
;;
;; Original Author: Dong Zheng
;; URL: https://github.com/don9z/blackboard-theme
;; All patches welcome
;;
;; The current fork of this theme makes it kind of "completely different"
;; Version: 3.1 - 2021-003-07
;; Current Author: Alisha Awen - siren1@disroot.org
;; This Package-Requires: Emacs "25" or later
;; --------------

;; This version attempts to make attractive Org-Mode headers, blocks,
;; keywords, etc for a nice "Scrivener Like" writing/publishing IDE
;;
;; The inspiration for this rather complicated eLisp code comes from:
;;   "Beautifying Org Mode in Emacs"
;;      https://zzamboni.org/post/beautifying-org-mode-in-emacs/
;;
;; Some of the code examples from the above article were implemented
;; within 09-1-org-beautify-conf.el, but to keep face customization
;; all in once place much of the styling for faces etc., have been
;; placed here within my customized Blackboard Colour Theme...
;; I had this code sprinkled around before and was experiencing
;; naughty side effects!

;; NOTE: To achieve best rendering of faces, you need to install the UTF 8
;;       fonts...
;;
;;          Mono:
;;             "Hermit" (for codeing)
;;             "Courier Prime" (default: for writing, org-mode, Screenplays, etc.) 
;;          Proportional:
;;             "Averia" (all styles, faces) 

;; How to use:
;;
;; First, add a local directory to custome-theme-load-path,
;; (add-to-list 'custom-theme-load-path "~/home/$USER/drop/the/theme/to")
;;
;; Then drop this theme into the above directory you created and defined...

;; Manually Load:
;;   Use "M-x load-theme"          (and choose blackboard)
;;
;; Load from init.el:
;;   (load-theme 'blackboard t)    ;; to enable the theme from Emacs startup...
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(deftheme blackboard
  "Based on Color theme by JD Huntington, which based off the
   TextMate Blackboard theme, created 2008-11-27. This new 2021
   version changes the background blackboard colour to a deep
   dark indigo rather than all black, and customizes Org Mode
   faces to complement Harmonic Alchemy Modular Emacs Writing/Publishing")

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Set Default Emacs Faces:
;;
;; NOTE1: If you don't have the named fonts below installed on your
;;        system, you need to do so now... Modular Emacs may not look
;;        very good using fallback fonts...
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Set Default Font Base Attributes:

(set-face-attribute
 'default nil
 :family "Hermit"
 :width 'normal
 :height 120
 :weight 'normal
 :slant 'normal
 :foreground "#F8F4FB"
  ;    :distant-foreground "3800A8"
 :background "#180028"
 :underline nil
 :overline nil
 :strike-through nil
 :box nil
 :inverse-video nil)

;; Fixed Pitch for Coding:

(set-face-attribute
 'fixed-pitch nil
 :inherit 'default)

;; Fixed Pitch for Writing:

(set-face-attribute
    'fixed-pitch-serif nil
    :inherit 'fixed-pitch
    :family "Courier Prime"
    :height 130)

;; Variable Pitch Face:

(set-face-attribute
    'variable-pitch nil
    :family "Averia Libre"
    :height 130)

;;; ~~~~~~~~~~~~~~~~~~~~~~
;; Set Blackboard Theme Faces:

(require 'org-faces)

(custom-theme-set-faces
 'blackboard

 ;; General Emacs Faces:

 `(bold ((t (:bold t))))

 `(bold-italic ((t (:bold t))))

 `(border-glyph ((t (nil))))

 `(buffers-tab ((t (:background "#180028" :foreground "#F8F8F8"))))

 `(font-lock-builtin-face ((t (:foreground "#94bff3"))))

 `(font-lock-comment-face ((t (:italic t :foreground "#AEAEAE"))))

 `(font-lock-constant-face ((t (:foreground "#D8FA3C"))))

 `(font-lock-doc-string-face ((t (:foreground "DarkOrange"))))

 `(font-lock-function-name-face ((t (:foreground "#FF6400"))))

 `(font-lock-keyword-face ((t (:foreground "#FBDE2D"))))

 `(font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))

 `(font-lock-reference-face ((t (:foreground "SlateBlue"))))

 `(font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))

 `(font-lock-regexp-grouping-construct ((t (:foreground "red"))))

 ;; Org Mode Faces:

 `(org-default ((t (:inherit fixed-pitch-serif))))

 `(org-block ((t (:inherit fixed-pitch))))

 `(org-code ((t (:inherit fixed-pitch))))

 `(org-document-info ((t (:foreground "dark orange"))))

 `(org-document-info-keyword ((t (:inherit (shadow fixed-pitch-serif)))))

 `(org-indent ((t (:inherit (org-hide fixed-pitch-serif)))))

;; org-link was previously royal blue no background...
 `(org-link ((t (
                 :foreground "skyblue2"
                 :background "#2e3436"
                 :underline t))))

 `(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch-serif)))))

 `(org-property-value ((t (:inherit fixed-pitch-serif))) t)

 `(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch-serif)))))

 `(org-table ((t (
                  :inherit fixed-pitch
                  :foreground "#98a583"))))

 `(org-tag ((t (
                :inherit (shadow fixed-pitch)
                :weight bold
                :height 0.8))))

 `(org-verbatim ((t (
                     :inherit (shadow fixed-pitch)
                     :foreground "#eeeeec"
                     :underline t
                     :slant italic
                     ))))

 `(org-hide ((t (:foreground "#2e3436"))))

 `(org-level-1 ((t (:bold nil :foreground "dodger blue" :height 1.3))))

 `(org-level-2 ((t (:bold nil :foreground "#edd400" :height 1.2))))

 `(org-level-3 ((t (:bold nil :foreground "#6ac214" :height 1.1))))

 `(org-level-4 ((t (:bold nil :foreground "tomato" :height 1.0))))

 `(org-date ((t (:underline t :foreground "magenta3"))))

 `(org-footnote  ((t (:underline t :foreground "magenta3"))))

 `(org-special-keyword ((t (:foreground "brown"))))

 `(org-block ((t (:foreground "#bbbbbc"))))

 `(org-quote ((t (:inherit org-block :slant italic))))

 `(org-verse ((t (:inherit org-block :slant italic))))

 `(org-todo ((t (:family "Hermit" :height 100 :foreground "red" :weight bold))))

 `(org-done ((t (:family "Hermit" :height 100 :foreground "green2" :weight bold))))

 `(org-agenda-structure ((t (:weight bold :foreground "tomato"))))

 `(org-agenda-date ((t (:foreground "#6ac214"))))

 `(org-agenda-date-weekend ((t (:weight normal :foreground "dodger blue"))))

 `(org-agenda-date-today ((t (:weight bold :foreground "#edd400"))))

 ;; Misc Faces:

 `(font-lock-string-face ((t (:foreground "#61CE3C"))))
 `(font-lock-type-face ((t (:foreground "#8DA6CE"))))
 `(font-lock-variable-name-face ((t (:foreground "#FF6400"))))
 `(font-lock-warning-face ((t (:bold t :foreground "pink"))))
 `(gui-element ((t (:background "#D4D0C8" :foreground "black"))))
 `(region ((t (:background "#253B76"))))
 `(mode-line ((t (:background "grey75" :foreground "black"))))
 `(highlight ((t (:background "#222222"))))
 `(highline-face ((t (:background "SeaGreen"))))
 `(italic ((t (nil))))
 `(left-margin ((t (nil))))
  ; `(text-cursor ((t (:background "white" :foreground "red"))))
 `(text-cursor ((t (:background "yellow" :foreground "black"))))
 `(toolbar ((t (nil))))
 `(underline ((nil (:underline nil))))
 `(zmacs-region ((t (:background "snow" :foreground "blue")))))

;;; ~~~~~~~~~~~~~~~~~~~~~~
;; Autoload This Theme:

(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'blackboard)

;; END blackboard-theme.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
