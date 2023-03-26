;;
;; Change Log: (descending chronological order)
;;

;; 2022-009-28 - Alisha Awen, siren1@disroot.org
;;   Removed Org-Bullets... This has been commented out for a while...
;;   I am NOT using org-bullets and only used them for a short while in
;;   the beginning BEFORE I set up my Fancy Org Mode which HIDES all bullets
;;   anyway... (except for the top most level)... Therefore org-bullets has
;;   always been a mute thing here...
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
;;  Hide all bullets/asterisks etc. in Org mode:
;;
;;  For writing books, docs, etc. I decided showing
;;  the bullets, clutters up my nice variable scale
;;  headings outline display...
;;
;;  This function was written to perform that service
;;  globally in org mode... It is added to the my
;;  org-mode hook function below...
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

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Org ellipsis options, other than the default Go to Node...
;; not supported in common font, but supported in Symbola font. ⬎, ⤷, ⤵
;;
;; I LOVE This One:

(setq org-ellipsis "⤵")

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
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "►" ))))))

(font-lock-add-keywords
 'org-mode
 '(("^ *\\([+]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "🞜"))))))

;; Automatically demote / promote list items with different characters...
;; As above, You can change (replace) bullet glifs (left column) to others
;; that you may prefer instead...

(setq org-list-demote-modify-bullet
      (quote (("+"  . "-")
              ("-"  . "+")
              ("🞜"  . "-")
              ("1." . "-")
              ("1)" . "-"))))

;; Increase offset a wee bit more between plain list levels... 

(setq-default org-list-indent-offset 1)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Use Unicode Symbols To Display Org-Mode Checkboxes:

(defun me_set-unicode-checkboxes ()
  "Use Unicode Symbols To Display Org-Mode Checkboxes"
  (interactive)
  (progn
  (push '("[ ]" .  "☐") prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "☢︎" ) prettify-symbols-alist)
  (prettify-symbols-mode)))

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
;;
;; For a nice visual reference of ALL available Emacs Colors
;; See: http://www.raebear.net/computers/emacs-colors/
;; (Jessica's Corner of Cyberspace) Thanks! Jessica

(setq org-emphasis-alist
      '(
	;; *ORG-BOLD*
	("*" (bold
              :foreground "DeepSkyBlue" ))          

	;; /ORG-ITALIC/
        ("/" (italic
              :family "Courier Prime"
              :slant italic
              :foreground "PaleGreen")) 

	;; _ORG-UNDERLINE_
        ("_" (
              :underline (
                          :color foreground-color
                          :style line)))            

	;; =ORG-VERBATIM=
        ("=" (
              :family "Hermit"
              :background "aquamarine4"
              :foreground "GhostWhite"))

        ;; ~ORG-CODE~
        ;;
        ;; Backgrounds: GhostWhite honeydew Ivory
        ;;
        ;; Foregrounds: red red4 DarkRed firebrick DarkGreen chocolate sienna
        ;;              SaddleBrown blue MidnightBlue aquamarine4 OrangeRed DarkOrange
        ("~" (
              :family "Hermit"
              :background "honeydew"
              :foreground "firebrick"))             ;; ~org-code~

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
         ((x-list-fonts "Averia Libre")
          '(:font "Averia Libre"))
         ((x-list-fonts "Averia Libre Light")
          '(:font "Averia Libre Light"))
         ((x-list-fonts "Averia Sans Libre Regular")
          '(:font "Averia Sans Libre Regular"))
         ((x-list-fonts "Averia Sans Libre Light")
          '(:font "Averia Sans Libre Light"))
         ((x-list-fonts "Averia Serif Libre Regular")
          '(:font "Averia Serif Libre Regular"))
         ((x-list-fonts "Averia Serif Libre Light")
          '(:font "Averia Serif Libre Light"))
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
