;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   [modular-emacs]:~/.emacs.d/lisp/modules/09-1-org-beautify-conf.el
;;
;;   This is a sub module of 09-org-mode-pkg-conf.el which configures Org Mode
;;   Beautify Settings:          (constantly under revision %^)
;;
;;   Override this file by placing a copy of it into "my-modules" then change
;;   it to suit your personal org-mode look and feel...
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(require 'org-faces)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   Org-Bullets:
;;
;;  NOTE:
;;    After setting up my Book Publishing Project
;;    Templates, I realized that showing any bullets
;;    at all, (even the last one) clutters up my nice
;;    Headings Display (which use variable scale fonts
;;    etc.) So now I am not actually displaying any of
;;    the fancy bullets below... I left this code in
;;    however to allow them to show should you choose
;;    to make the last bullet visible again...
;;    (see function:  me_hide-org-bullets () below)

;; Use org-bullets-mode for utf8 symbols as org bullets

(require 'org-bullets)

;; make available "org-bullet-face" allowing control of the font
;; sizes individually:

(setq org-bullets-face-name (quote org-bullet-face))

;;;
;; Bullet options to try out: (commented out)
;; Enable the one you like... Add more choices below if you find them...

;(setq org-bullets-bullet-list '("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥"))

;; Hexagrams:
;(setq org-bullets-bullet-list '("✡" "⎈" "✽" "✲" "✱" "✻" "✼" "✽" "✾" "✿" "❀" "❁" "❂" "❃" "❄" "❅" "❆" "❇"))

;; Special Symbols:
(setq org-bullets-bullet-list '("☀" "♼" "☼" "☾" "☽" "☣" "§" "¶" "‡" "※" "✕" "△" "◇" "▶" "◀" "◈"))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Org ellipsis options, other than the default Go to Node...
;; not supported in common font, but supported in Symbola font. ⬎, ⤷, ⤵

(setq org-ellipsis "⤵")

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Hide all bullets/asterisks etc. in Org mode:
;;
;;  For writing books, docs, etc. I decided showing
;;  the bullets, (even the last one) clutters up my
;;  nice variable scale headings outline display...
;;  This function was written to perform that service
;;  globally in org mode... It is added to the my
;;  org-mode hook function below...
;;
;;  NOTE: if you want to see the fancy bullets in your
;;  outline headings, than don't call this function
;;  in any org-mode hook functions...
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun me_hide-org-bullets ()
  "Hide the Org Bullets..."
  (interactive)
  (font-lock-add-keywords
   'org-mode `(("\\(?:^\\(?1:\\*+\\)[[:blank:]]\\)"
              (0 (progn (compose-region
                         (match-beginning 1) (match-end 1)
                         (pcase (length (match-string 1))
                           (1 ?\u2219)
                           (2 ?\u2022)
                           (3 ?\u25c9)
                           (_ ?\u25CB)))
                        nil))))))

;; Open Org Files initially folded in Overview Mode:

(setq org-startup-folded 'overview)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Force org-mode to auto insert single blank lines
;;  around headings...

(setq org-ascii-headline-spacing (quote (1 . 1)))

(setq org-startup-indented t)

(setq org-cycle-separator-lines 0)

(setq org-blank-before-new-entry (quote ((heading)
                                         (plain-list-item . auto))))

(setq org-insert-heading-respect-content nil)

(setq org-reverse-note-order nil)

(setq org-show-following-heading t)

(setq org-show-hierarchy-above t)

(setq org-show-siblings (quote ((default))))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Show nice chosen symbols instead of a dash in bulleted lists.
;; If you don't like these, either disable the next two forms or
;; change the characters at the end of each expression to something
;; that fits your style...

(font-lock-add-keywords
 'org-mode
 '(("^ *\\([-]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "🍥"))))))

(font-lock-add-keywords
 'org-mode
 '(("^ *\\([+]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "🞜"))))))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Customize look of emphasized text in org-mode 
;; This is for how markup will look in an org-mode
;; buffer.  Not what it may or may not look like
;; after exported to HTML or LaTeX etc...
;; Experiment with this until you have something
;; you like...

;(add-to-list 'org-emphasis-alist
;             '("*" (:foreground "DarkOrange"))
;             '("/" (:foreground "LimeGreen")))

(setq org-emphasis-alist
  '(("*" (bold :foreground "chocolate" ))                          ;; *BOLD*
    ("/" (italic :foreground "LimeGreen" :family "Courier Prime")) ;; /Italic/
    ("_" underline)                                                ;; _underline_
    ("=" (:background "maroon" :foreground "white"))               ;; =org-verbatim=
    ("~" (:background "LemonChiffon" :foreground "IndianRed"))     ;; ~org-code~
    ("+" (:strike-through t))))                                    ;; +strike-through+



   

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Make Attractive Org-Mode Headers, blocks, keywords etc:
;;
;; The inspiration for this rather complicated eLisp code comes from:
;;   "Beautifying Org Mode in Emacs"
;;      https://zzamboni.org/post/beautifying-org-mode-in-emacs/

;;;
;;  Set default faces:

(set-face-attribute
 'fixed-pitch t
 :family "Hermit"
 :height 120
 :width 'normal)

(when ME--DARWIN
  (set-face-attribute
   'fixed-pitch-serif t
   :family "Courier Prime"
   :height 130
   :width 'normal))

(when ME--LINUX
  (set-face-attribute
   'fixed-pitch-serif t
   :family "Courier Prime"
   :height 130
   :width 'normal))

;; Variable Pitch Face:

(set-face-attribute
 'variable-pitch t
 :family "Averia Serif Libre"
 :height 130
 :width 'normal)

;; Set Org Mode Faces:

(custom-theme-set-faces
 'user
 `(default ((t (:inherit fixed-pitch-serif :height 120))))
 `(org-default ((t (:inherit fixed-pitch-serif :height 120))))
 `(org-block ((t (:inherit fixed-pitch-serif))))
 `(org-code ((t (:inherit fixed-pitch))))
 `(org-document-info ((t (:foreground "dark orange"))))
 `(org-document-info-keyword ((t (:inherit (shadow fixed-pitch-serif)))))
 `(org-indent ((t (:inherit (org-hide fixed-pitch-serif)))))
 `(org-link ((t (:foreground "royal blue" :underline t))))
 `(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch-serif)))))
 `(org-property-value ((t (:inherit fixed-pitch-serif))) t)
 `(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch-serif)))))
 `(org-table ((t (:inherit fixed-pitch :foreground "#98a583"))))
 `(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 `(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;; Customize Org headings:

(let* ((variable-tuple
        (cond
         ((x-list-fonts "Averia Serif Libre")
          '(:font "Averia Serif Libre"))
         ((x-list-fonts "Averia Libre Light")
          '(:font "Averia Libre Light"))
         (nil (warn "Averia Fonts Not Found! Did you install them?"))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight normal )))

  (custom-theme-set-faces
   'user
   `(org-level-10
     ((t (,@headline
          ,@variable-tuple
          :height 1.11
          :foreground "AntiqueWhite" ))))

   `(org-level-9
     ((t (,@headline
          ,@variable-tuple
          :height 1.12
          :foreground "AntiqueWhite" ))))

   `(org-level-8
     ((t (,@headline
          ,@variable-tuple
          :height 1.13
          :foreground "AntiqueWhite" ))))

   `(org-level-7
     ((t (,@headline
          ,@variable-tuple
          :height 1.17
          :foreground "AntiqueWhite" ))))

   `(org-level-6
     ((t (,@headline
          ,@variable-tuple
          :height 1.20
          :foreground "AntiqueWhite" ))))

   `(org-level-5
     ((t (,@headline
          ,@variable-tuple
          :height 1.23
          :foreground "AntiqueWhite" ))))

   `(org-level-4
     ((t (,@headline
          ,@variable-tuple
          :height 1.28
          :foreground "AntiqueWhite" ))))

   `(org-level-3
     ((t (,@headline
          ,@variable-tuple
          :height 1.38
          :foreground "AntiqueWhite" ))))

   `(org-level-2
     ((t (,@headline
          ,@variable-tuple
          :height 1.62
          :foreground "AntiqueWhite" ))))

   `(org-level-1
     ((t (,@headline
          ,@variable-tuple
          :height 1.88
          :foreground "AntiqueWhite" ))))

   `(org-document-title
     ((t (,@headline
          ,@variable-tuple
          :height 1.5
          :foreground "AntiqueWhite" :underline nil))))))

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   END: [modular-emacs]:~/.emacs.d/lisp/modules/09-1-org-beautify-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
