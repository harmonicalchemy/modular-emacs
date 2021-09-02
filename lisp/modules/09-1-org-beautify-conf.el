;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   [modular-emacs]:~/.emacs.d/lisp/modules/09-1-org-beautify-conf.el
;;
;;   This is a sub module of 09-org-mode-pkg-conf.el which configures Org Mode
;;   Beautify Settings:          (constantly under evolving revision %^)
;;
;;   Override this file by placing a copy of it into "my-modules" then change
;;   it to suit your personal org-mode look and feel...
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(require 'org-faces)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Set Fallback Font:
;;  This is critical to ensure unicode symbols always
;;  display correctly...
;;  Note: Setting 'nil' below makes it the fallback
;;        font.  Other values allow you to set a range
;;        of gliphs...
;;  Reference:
;;  http://endlessparentheses.com/manually-choose-a-fallback-font-for-unicode.html

(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "Symbola"))


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

;; Make "org-bullet-face" available allowing control of the font
;; sizes individually:  (NOTE: I don't really use this but it's here)

(setq org-bullets-face-name (quote org-bullet-face))

;;;
;; Bullet options to try out: (commented out)
;; Enable the one you like... Add more choices below if you find them...

;(setq org-bullets-bullet-list '("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥"))

;; Hexagrams:
;(setq org-bullets-bullet-list '("✡" "⎈" "✽" "✲" "✱" "✻" "✼" "✽" "✾" "✿" "❀" "❁" "❂" "❃" "❄" "❅" "❆" "❇"))

;; Special Symbols:
(setq org-bullets-bullet-list '("☀" "♼" "☼" "☾" "☽" "☣" "§" "¶" "‡" "※" "✕" "△" "◇" "▶" "◀" "◈"))

;; NOTE: I find Org Outline Bullets in general to be cluttering and non-essential. ;;       I make my outline bullets in Modular Emacs default to "invisible" and
;;       instead focus on heading indentation, size, and font style.
;;       (like a real doc should look)
;;
;;       Hiding Outline bullets also aids visibility of plain list bullets and
;;       enumerations, (which traditionally are expected to have bullets in
;;       front of them...

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Org ellipsis options, other than the default Go to Node...
;; not supported in common font, but supported in Symbola font. ⬎, ⤷, ⤵
;;
;; I LOVE This One:

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
;;
;;  NOTE: I am now trying this in my org-mode hook:
;
;   (after! org
;     (setq org-hide-leading-stars nil
;           org-indent-mode-turns-on-hiding-stars nil))
;
;;  As an alternative way no longer needing the
;;  explicit function call below...
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

(setq org-show-siblings (quote (default)))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Show nice chosen symbols instead of a dash in bulleted lists.
;; If you don't like these, either disable the next two forms or
;; change the characters at the end of each expression to something
;; that fits your style...

(font-lock-add-keywords
 'org-mode
 '(("^ *\\([-]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "🍥" ))))))

(font-lock-add-keywords
 'org-mode
 '(("^ *\\([+]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "🞜"))))))

;; Automatically demote / promote list items with different characters...

(setq org-list-demote-modify-bullet
      (quote (("+"  . "-")
              ("-"  . "+")
              ("*"  . "-")
              ("1." . "-")
              ("1)" . "-")
              ("A)" . "-")
              ("B)" . "-")
              ("a)" . "-")
              ("b)" . "-")
              ("A." . "-")
              ("B." . "-")
              ("a." . "-")
              ("b." . "-"))))

;; Increase offset a wee bit more between plain list levels... 

(setq-default org-list-indent-offset 1)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Customize look of emphasized text in org-mode 
;; This is for how markup will look in an org-mode
;; buffer.  Not what it may or may not look like
;; after exported to HTML or LaTeX etc...
;;
;; Experiment with this until you have something
;; you like... so far it's angry fruit salad! ;-)
;; It took me so long to get around to fixing
;; this... So now I will live with these settings
;; until they drive me crazy!  LOL

(setq org-emphasis-alist
      '(("*" (bold
              :foreground "chocolate" ))            ;; *BOLD*

        ("/" (italic
              :family "Courier Prime"
              :slant italic
              :foreground "LimeGreen"))             ;; /Italic/

        ("_" (
              :underline (
                          :color foreground-color
                          :style line)))            ;; _underline_

        ("=" (
              :family "Hermit"
              :background "maroon"
              :foreground "white"))                 ;; =org-verbatim=

        ("~" (
              :family "Hermit"
              :background "Ivory"
              :foreground "tomato"))             ;; ~org-code~

        ("+" (:strike-through t))))                 ;; +strike-through+


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Customize Org headings:
;; The fonts listed below will be tried in sequence,
;; and the first one found will be used...
;;
;; The inspiration for this rather complicated eLisp code comes from:
;;   "Beautifying Org Mode in Emacs"
;;      https://zzamboni.org/post/beautifying-org-mode-in-emacs/

(let* ((variable-tuple
        (cond
         ((x-list-fonts "Averia Libre Regular")
          '(:font "Averia Libre Regular"))
         ((x-list-fonts "Averia Libre Light")
          '(:font "Averia Libre Light"))
         ((x-list-fonts "Averia Sans Libre Regular")
          '(:font "Averia Libre Regular"))
         ((x-list-fonts "Averia Sans Libre Light")
          '(:font "Averia Libre Regular"))
         ((x-list-fonts "Averia Serif Libre Regular")
          '(:font "Averia Libre Regular"))
         ((x-list-fonts "Averia Serif Libre Light")
          '(:font "Averia Libre Regular"))
         (nil (warn "Averia Fonts Not Found! Did you install them?"))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit 'default )))

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
