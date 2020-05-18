;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; blackboard-theme.el - A Slightly Mangled Version of
;;                       The TextMate Blackboard Theme ;-)
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
;; Current modifications of this file were made by:
;; New Author: Alisha Awen - siren1@disroot.org
;; --------------
;; Version: 3.0 - 2020-May-15
;; Package-Requires: Emacs "25" or later
;;
;; Commentary:
;;
;; This blackboard-theme does not rely on the old Emacs color-theme package...
;;
;; NOTE: To achieve best rendering of faces, you need to install the UTF 8
;;       mono fonts: "Hermit" (for code), and "Go Mono for Powerline"
;;       (for Org-Mode). 
;;
;; How to use:
;;
;; First, add a local directory to custome-theme-load-path,
;; (add-to-list 'custom-theme-load-path "~/home/$USER/drop/the/theme/to")
;;
;; Then drop this theme into the above directory you created and defined...
;;
;; Manually Load:
;;   Use "M-x load-theme"          (and choose blackboard)
;;
;; Load from init.el:
;;   (load-theme 'blackboard t)    ;; to enable the theme from Emacs startup...
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; Code:

(deftheme blackboard
  "Based on Color theme by JD Huntington, which based off the TextMate Blackboard theme, created 2008-11-27
   This new 2020 version changes the background blackboard colour to a deep dark indigo rather than all black")

(custom-theme-set-faces
 `blackboard
 `(default ((t (:background "#180028" :foreground "#F8F4FB" ))))
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

 ;; org-mode

 '(org-default ((t (:family "Go Mono for Powerline" :height 125 :weight light))))
 `(org-hide ((t (:foreground "#2e3436"))))
 `(org-level-1 ((t (:bold nil :foreground "dodger blue" :height 1.3))))
 `(org-level-2 ((t (:bold nil :foreground "#edd400" :height 1.2))))
 `(org-level-3 ((t (:bold nil :foreground "#6ac214" :height 1.1))))
 `(org-level-4 ((t (:bold nil :foreground "tomato" :height 1.0))))
 `(org-date ((t (:underline t :foreground "magenta3"))))
 `(org-footnote  ((t (:underline t :foreground "magenta3"))))
 `(org-link ((t (:foreground "skyblue2" :background "#2e3436"))))
 `(org-special-keyword ((t (:foreground "brown"))))
 `(org-verbatim ((t (:foreground "#eeeeec" :underline t :slant italic))))
 `(org-block ((t (:foreground "#bbbbbc"))))
 `(org-quote ((t (:inherit org-block :slant italic))))
 `(org-verse ((t (:inherit org-block :slant italic))))
 `(org-todo ((t (:family "Hermit" :height 100 :foreground "red" :weight bold))))
 `(org-done ((t (:family "Hermit" :height 100 :foreground "green2" :weight bold))))
 `(org-agenda-structure ((t (:weight bold :foreground "tomato"))))
 `(org-agenda-date ((t (:foreground "#6ac214"))))
 `(org-agenda-date-weekend ((t (:weight normal :foreground "dodger blue"))))
 `(org-agenda-date-today ((t (:weight bold :foreground "#edd400"))))

 ;; Misc:
 
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
 `(text-cursor ((t (:background "yellow" :foreground "black"))))
 `(toolbar ((t (nil))))
 `(underline ((nil (:underline nil))))
 `(zmacs-region ((t (:background "snow" :foreground "ble")))))

;;;###autoload

(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'blackboard)

;;; blackboard-theme.el ends here.
