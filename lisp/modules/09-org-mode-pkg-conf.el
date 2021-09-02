;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/09-org-mode-pkg-conf.el
;;
;; This module adds extra tools for customizing org-mode for specific purposes
;; which can be vast!!! For starters, my current logging (which I currently do
;; in markdown mode, is very limited.  org-mode offers a total improvement that
;; makes using traditional PIM Apps look like toys! Org-Mode is the true answer
;; to Knowledge Management (KM) as we discussed and defined the term back at
;; Lotus Research in the 90s...  The solution was right under our noses in Emacs!
;; Secondly, I will be using org-mode for a total Business Accounting System...
;; Thirdly, I will be using org-mode for Writing, Publishing, Scrivener Style...
;; Also, I may end up using org-mode to organize all my System Admin stuff,
;; including the code and scripts...  More about that later....
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;; Create repositories cache for Org Mode extras, if required:

(when (not package-archive-contents)
  (package-refresh-contents))

;;;
;; Declare a list of required packages for my fancy org-mode setup:
;;

(defvar me--req-org-packages
  '(org-bullets
    gnuplot-mode
    sass-mode
    abc-mode
    org-sidebar
    org-mind-map))

;;;
;; Install required packages:

(mapc (lambda (p) (package-install p))
      me--req-org-packages)

;; Summon the Org-mode Gods to be ready to grant your Mod requests...

(require 'org)
(require 'org-tempo)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Define LOCAL USER MASTER ORG FILES DIRECTORY:
;; The place where you will be keeping all your private org files and
;; sub-directories... You may already have a special directory for your
;; org files.  In that case be sure to read all the notes below!
;;
;; IMPORTANT NOTE: Change the path in the first two forms below to match your
;; Org-Files Home Directory. All path definitions below this form will be
;; relative to this defined MY-ORG-DIR... (if you don't have a special
;; org-notes directory on your file system yet, create that directory now and
;; change this next form below to reflect your new directory's path)

;;;
;; Path to Your ORG Docs on Mac OS:

(when ME--DARWIN
  (defconst me--org-dir
    (file-name-as-directory
     (expand-file-name "~/Path/To/Your/MacOS/Org-Docs"))))

;;;
;; Path to Your ORG Docs on Linux:

(when ME--LINUX
  (defconst me--org-dir
    (file-name-as-directory
     (expand-file-name "~/Path/To/Your/Linux/Org-Docs"))))

;;;
;; Use above local Constants to Define Org Mode Default "Org Directory":

(setq org-directory me--org-dir)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Configure Org Mode Beautify Settings: (constantly under revision %^)
;;  Override the file below by putting it into "my-modules" then change it to
;;  suit your needs and planning style...

(if (file-exists-p "~/.emacs.d/lisp/my-modules/09-1-org-beautify-conf.el")
    (load-file "~/.emacs.d/lisp/my-modules/09-1-org-beautify-conf.el")
  (load-file "~/.emacs.d/lisp/modules/09-1-org-beautify-conf.el"))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Setup my Default Org-Mode Keywords and Tags:
;;  Schemes are based on the general GTD process I am using...
;;  Override the file below by putting it into my-modules then change it to
;;  suit your needs and planning style...

(if (file-exists-p "~/.emacs.d/lisp/my-modules/09-2-org-keywords-tags-conf.el")
    (load-file "~/.emacs.d/lisp/my-modules/09-2-org-keywords-tags-conf.el")
  (load-file "~/.emacs.d/lisp/modules/09-2-org-keywords-tags-conf.el"))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Setup my Org-Mode Project Management (Capture, GTD, TODO, Tasks, Agenda, etc.
;;  Schemes are based on the "Autofocus GTD Process" I use along with the
;;  Timesheets emacs package and a few other things like mind maps etc...
;;  Override the file below by putting it into my-modules then change it to
;;  suit your needs and planning style...

(if (file-exists-p "~/.emacs.d/lisp/my-modules/09-3-org-PM-conf.el")
    (load-file "~/.emacs.d/lisp/my-modules/09-3-org-PM-conf.el")
  (load-file "~/.emacs.d/lisp/modules/09-3-org-PM-conf.el"))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Configure Org-Mode Export Settings:
;;  Override the file below by putting it into my-modules then change it to
;;  fit your desired Org Mode Export features...

(if (file-exists-p "~/.emacs.d/lisp/my-modules/09-4-org-export-conf.el")
    (load-file "~/.emacs.d/lisp/my-modules/09-4-org-export-conf.el")
  (load-file "~/.emacs.d/lisp/modules/09-4-org-export-conf.el"))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   Alisha's Advanced Org-Mode Configurations:
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;             Org Mind Map Configuration

(require 'ox-org)

(setq org-mind-map-engine "dot")       ; Default. Directed Graph
;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
;; (setq org-mind-map-engine "twopi")  ; Radial Layout
;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
;; (setq org-mind-map-engine "twopi")  ; Radial layouts
;; (setq org-mind-map-engine "circo")  ; Circular Layout


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;            Mobile Org Configuration

;; Set up MobileOrg Staging Area:

(when ME--DARWIN
  (defconst me--mobile-org-dir
    (file-name-as-directory
     (expand-file-name "~/Path/To/Your-MacOS/Dropbox/Apps/MobileOrg"))))

;;;
;; Path to Your ORG Docs on Linux:

(when ME--LINUX
  (defconst me--mobile-org-dir
    (file-name-as-directory
     (expand-file-name "~/Path/To/Your-Linux/Dropbox/Apps/MobileOrg"))))

(setq org-mobile-directory me--mobile-org-dir)

;; Set Name of file where new notes will be stored:

(setq org-mobile-inbox-for-pull (expand-file-name  "flagged.org" me--org-dir))

;; Enable MobileOrg Encryption:

(setq org-mobile-use-encryption t)

;; Set MobileOrg Password:

(setq org-mobile-encryption-password "<enter-password-here>")


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Defined Variable:  ME--YT-iFrame-format
;;  For embedding YouTube Links in Org Files:
;;
;;  Ref:
;;     https://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html
;;
;;  Usage:
;;     To use this, just write your org links in the following way
;;     (optionally adding a description).
;;
;;                     [[yt:A3JAlWM8qRM]]
;;
;; When you export to HTML, the above will produce an inline snippet that Youtube
;; specifies. The advantage (over simply writing out the iframe) is that this link
;; can also be clicked in org-mode, and can be exported to other formats as well.

(defvar me--yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(org-add-link-type
 "yt"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format me--yt-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Modular Emacs Quick Insert Org :README: Drawer  
;;  Function...

(defun me_org-insert-readme-drawer ()
  (interactive)
  (org-insert-drawer nil "README"))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Modular Emacs Quick Insert CUSTOM_ID for links
;;  with Helper Functions...
;;  Ref: https://writequit.org/articles/emacs-org-mode-generate-ids.html
;;       This implements the initial set of functions etc. from Lee's
;;       article.  He has revised his original due to some user input
;;       etc...  I have not implemented that but if problems arise
;;       as mentioned in the above article, go back and implement
;;       those updates at the bottom of his article...

;; Require the org-id library, which contains
;; helpers. Use a CUSTOM_ID for links:

(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;; Define custom Modular Emacs version of org-custom-id-get
;; that calls org-id-new and creates a new property if one
;; doesn't already exist...

(defun me_org-custom-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an CUSTOM_ID, the function returns nil. However, when
   CREATE is non nil, create a CUSTOM_ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the CUSTOM_ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let ((id (org-entry-get nil "CUSTOM_ID")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new (concat prefix "h")))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))

;; Add a helper function that's interactive to add custom ids to all
;; headlines in the buffer if they don't already have one. 

(defun me_org-add-ids-to-headlines-in-file ()
  "Add CUSTOM_ID properties to all headlines in the
   current file which do not already have one."
  (interactive)
  (org-map-entries (lambda () (me_org-custom-id-get (point) 'create))))

;; Add a helper function that's interactive to add a custom id to 
;; the headline at the current cursor location in buffer
;; (if there isn't a custom ID set allready)...

(defun me_org-add-id-to-headline ()
  "Add CUSTOM_ID property to headline nearest to current cursor
   location if the heading does not already have one."
  (interactive)
  (me_org-custom-id-get (point) 'create))

;; Optional helper to add a CUSTOM_ID to headlines
;; created when using an org-capture template: 

(add-hook 'org-capture-prepare-finalize-hook
          (lambda () (me_org-custom-id-get (point) 'create)))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Org Outline Tree on left | content on 
;;  right Functions...
;;
;; Open sub elements in right window pane,
;; move cursor focus to right window pane...
;; NOTE: I am not using this anymore because
;;       I changed to using org-sidebar which
;;       does this by defult when you hit RET
;;       while your cursor is on an outline
;;       heading within the leftmost
;;       org-sidebar-tree window pane.

(defun me_org-tree-open-in-right-win ()
  (interactive)
  (org-tree-to-indirect-buffer)
  (windmove-right))

;; Open sub elements in right window pane,
;; Leave cursor in left outline window pane...

(defun me_org-tree-open-in-right-no-focus ()
  (interactive)
  (org-sidebar-tree-jump-indirect)
  (windmove-left))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Set up Normal Org View in Single Window Frame
;;  NO Org Sidebar Tree Left navigation pane...

(defun me_org-narrow-screen ()
  "Set up Org View in Single Window Frame with
   dimensions set to work fine as a single window only
   session"
  (interactive)
  ;; Test to make sure we are in Org Mode, if not print a warning...
  (if (equal 'org-mode major-mode)
      (progn
        ;; Make sure we are in Split Window Fancy Org View First!
        (when (= (length (window-list)) 2)
          (if (eq 'selected-window 'window-left-child)
              ;; Remove Split Sidebar Tree Window:
              (progn
                (org-sidebar-tree-toggle))
            (progn
              ;; Else Move to left most window:
              (windmove-left)
              ;; And now Remove Split Sidebar Tree Window...
              (org-sidebar-tree-toggle))))

        ;; Make NORMAL ORG MODE Frame size for writing/composition:
        (modify-frame-parameters nil
                                 (quote
                                  ((name   . "HA Mod Emacs v3.4 - Normal Org Mode")
                                   (height . 38)
                                   (width  . 88))))

        ;; Set Olivetti Width (88 column wide)
        (olivetti-set-width 82))

    ;; ELSE print warning about buffer not being an Org File...
    (message "Warning: You are NOT visiting a .ORG file!")))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Set up Fancy Org View in W I D E  S C R E E N
;;  mode with Org Sidebar Tree in Left pane and
;;  Content window on right... (i.e., Like Scrivener)

(defun me_org-wide-screen ()
  "Set up Fancy Org View in W I D E  S C R E E N view with frame
   dimensions set to work nicely with split windows:  A navigation
   window pane on left side showing the Org Tree headings only, and a content
   window on the right side showing everything (for editing)"
  (interactive)
  ;; Test to make sure we are in Org Mode, if not print a warning...
  (if (equal 'org-mode major-mode)
      (progn
          ;; Make Wide Screen Fancy Org View View:
            (modify-frame-parameters nil
                                     (quote
                                      ((name   . "HA Mod Emacs v3.4 - Fancy Org View")
                                       (height . 38)
                                       (width  . 138))))
            ;; Split Windows with org outline tree in narrow left window...
            (org-sidebar-tree)
            ;; Set Olivetti Width (100 column wide)
            (olivetti-set-width 82))

    ;; ELSE print warning about buffer not being an Org File...
    (progn
      (message "Warning: You are NOT visiting a .ORG file!"))))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Modular Emacs - me_org-toggle-blocks Function:
;;
;;  With this in place, org-mode collapses SRC blocks by default and lets you
;;  toggle visibility of all SRC blocks by hitting C-c t.

(defvar me--org-blocks-hidden nil)

(defun me_org-toggle-blocks ()
  (interactive)
  (if me--org-blocks-hidden
      (org-show-block-all)
    (org-hide-block-all))
  (setq-local me--org-blocks-hidden (not me--org-blocks-hidden)))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Modular Emacs - Fancy Org View - org-mode Hook Function:
;;  This Fancy Org View hook takes care of setting your writing/publishing
;;  environment nicely...
;;
;;  Using this mode you can have two different Screen Layouts via a Toggle key..
;;  Normal Layout for general purpose Org mode work in a single window...  and
;;  Fancy Org View W I D E  S C R E E N layout with navigation tree window on
;;  left side and content window on right side for adding content.  The wide screen
;;  based publishing environment is working just as well as my Scrivener Projects
;;  were in the past!  It will soon be exceeding anything that I could do with
;;  Scrivener and be future proof to boot!  Combined with Fountain mode for
;;  screenwriting, audio and VR scripting etc... When this project is done
;;  you will be glad to know "all your base are belong to" Emacs Org Mode! %^)

(defun me_fancy-org-mode-hook ()
  "Harmonic Alchemy Modular Emacs Fancy Org Mode Hook Function.
   This function takes care of setting up a nice org-mode writing
   environment for both planning and / or writing-publishing"

  ;; Set default face to Courier Prime (A nice mono serif for writing)
  ;; NOTE: All settings below change the currently selected frame only...
  ;;       (other existing and future frames are not affected)

  (set-face-attribute 'default
                      (selected-frame)
                      :family "Courier Prime"
                      :height 130)

  ;; Set SCREEN Frame size for org-mode planning,
  ;; finances, TODOs, etc. (no split windows)

  (modify-frame-parameters nil
                           (quote
                            ((name   . "HA Mod Emacs v3.4 - Normal Org Mode")
                             (height . 38)
                             (width  . 88))))

  ;; Collapse all source blocks;
  (org-hide-block-all)

  ;; Enable Olivetti Mode (100 column wide)
  (olivetti-set-width 82)

  ;; Hide bullets...
  (me_hide-org-bullets))

  ;; The below alternate method to above "me_hide-org-bullets"
  ;; does not seem to work for me (yet): FIXME
;  (after! org
;          (setq org-hide-leading-stars nil
;                org-indent-mode-turns-on-hiding-stars nil)))

;; END Fancy Org View - org-mode Hook function...


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Modular Emacs - Org Mode Hook function For EDIFF:
;; (EXPIRIMENTAL for Debugging only - Don't Use this)
;;
;;  This EDIFF Org Mode hook puts Org Mode in bare
;;  bones, single pane mode, which works fine for
;;  using tools that operate on .org files and also
;;  need to controll splitting of windows etc...
;;
;;  USEAGE Notes:
;;
;;    1. No Notes... Just use as is...  It just works...
;;       What it says on the tin...  %^)
;;
;;       Oh... Right....  Don't forget to switch forms
;;       below to register THIS hook instead of the Fancy
;;       Org Mode Hook (above) Right? "That's how you turn
;;       it on honey...  And make sure it's plugged in!"
;;       LOL - OK... ready eddy?  let's dance...

(defun me_normal-org-mode-hook ()
  "Harmonic Alchemy Modular Emacs Simple Org Mode Hook Function.
   This function configures org-mode to work better while using
   diff tools like eDiff to compare .org files.  Fancy Org Mode
   breaks eDiff and other tools that split windows..."

  ;; All this Function does is Set default face to Courier Prime

  (set-face-attribute 'default (selected-frame)
                      :family "Courier Prime"
                      :height 130))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;              FANCY ORG MODE INIT
;;  Add Fancy Org Mode Hook Function to Org Mode startup list:

(add-hook 'org-mode-hook 'me_fancy-org-mode-hook)

;;;              NORMAL ORG MODE INIT
;;  Add Normal Org Mode Hook Function to Org Mode startup list:
;;  NOTE:  Don't enable this unless you are troubleshooting Org Mode or something...
;;         And if you do enable this form, you need to disable the form above it...
;(add-hook 'org-mode-hook 'me_normal-org-mode-hook)

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   END: [modular-emacs]:~/.emacs.d/lisp/modules/09-org-mode-pkg-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
