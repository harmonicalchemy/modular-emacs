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
    emojify
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

(when *is-darwin*
  (defconst my-org-dir
    (file-name-as-directory
     (expand-file-name "~/Documents/000-Alisha/000-GIT/My-Docs/Org-Docs"))))
     (expand-file-name "~/Path/To/Your-Linux-Org-Docs"))))

;;;
;; Path to Your ORG Docs on Linux:

(when *is-linux*
  (defconst my-org-dir
    (file-name-as-directory
     (expand-file-name "~/Path/To/Your-Linux-Org-Docs"))))

;;;
;; The rest of the definitions below depend on above "my-org-dir" being set
;; correctly to an existing directory on your file system where you will be
;; storing all your important org related files...
;;
;; NEW ORG USERS NOTE: If you don't allready have an org-files system structure
;; set up, it is highly recommended to create your directory structure exactly as
;; below, (replacing "Your-Org-Docs-dir" with the directory path where you will
;; be setting up the structure below it... The symbol defined above:
;; "my-org-dir" must match the path to Your Org Docs Master directory.
;;
;;   Your-Org-Docs-dir/      # This must match (my-org-dir) set above!
;;     |__00-Agenda-files/   # All others must match exactly as shown!
;;     |__03-Private/        # Create your files folders following this diagram. 
;;     |   |____diary.org
;;     |   |____refile.org
;;     |   |____Timesheet/
;;     |__refile.org
;;
;; EXISTING ORG USERS NOTE! For existing org-files directory structures,
;; unfortunately you may need to adjust the file/folder names below within 
;; all related setq forms before this will work for your already existing setup...
;; It's probably best to try doing all this (as is) outside of your normal Org Files
;; directory first...  Once you have a good handle on this it will be easier to
;; adjust the code to fit into your existing org-mode configuration...
;; I may change how this works later to make it much easier to integrate...
;; For now, it's all kind of hard-wired...  Until I myself get a handle on it... %^)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Define LOCAL USER ORG FILES SUB Directories:
;;
;; Note: These constants are also used by 15-Accounting-pkg-conf.el
;;       which gets loaded later in the module chain...

(defconst my-org-agenda-files
  (file-name-as-directory
   (expand-file-name "00-Agenda-files" my-org-dir)))

(defconst my-org-templates
  (file-name-as-directory
   (expand-file-name "02-Templates" my-org-dir)))

(defconst my-org-files
  (file-name-as-directory
   (expand-file-name "03-Private" my-org-dir)))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Set default Org Mode directories:

(setq org-directory my-org-dir)
(setq org-default-notes-file (concat org-directory "/autofocus.org"))

;; Load Default Org Agenda Files (directories) that are permanent...
;; NOTE:  This list is mostly updated from within org files
;;        or by manually inserting them while you use org-mode...

(setq org-agenda-files '(my-org-dir
                         my-org-agenda-files
                         my-org-templates
                         my-org-files))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Configure Org Mode Beautify Settings: (constantly under revision %^)
;;  Override the file below by putting it into "my-modules" then change it to
;;  suit your needs and planning style...

(if (file-exists-p "~/.emacs.d/lisp/my-modules/09-1-org-beautify-conf.el")
    (load-file "~/.emacs.d/lisp/my-modules/09-1-org-beautify-conf.el")
  (load-file "~/.emacs.d/lisp/modules/09-1-org-beautify-conf.el"))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   Alisha's Advanced Org-Mode Configurations:
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
;;  Open Org File in new frame from Dired and set its width to 180:
;;  (no checks are made to be sure this is a .org file but no matter...
;;  NOTE:  Coding / debugging of this function is not complete yet
;;         Don't use it unless you modify it to work first...
;;         To be continued...
(defun me_dired-find-org-file-other-frame ()
  "In Dired, visit this file or directory in a new frame that has its
   dimensions set to fit an org file in Fancy Org Mode with split
   narrow left outline pane, and wider right pane for content editing"
  (interactive)
  (find-file-other-frame (dired-get-file-for-visit))
  
  (modify-frame-parameters nil
                           (quote
                            ((name . "Fancy Org Mode")
                             (height . 45)
                             (width . 175))))
  ;; NOTE: Still coding this one up... Don't use it yet.. It is unfinished...
  ;;       I don't need it yet... Maybe i will later and then I will finish it %^)
  )


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Modular Emacs - Fancy Org Mode Hook function:
;;  This Fancy Org Mode hook takes care of setting your writing/publishing
;;  environment nicely...
;;
;;  USEAGE Notes:
;;
;;    1. Do you want a nice environment for writing / publishing in
;;       "Wide Screen" mode right and left panes? i.e.:
;;         * Narrow Outline Headings (collapsed) on Left side...
;;         * Wide distraction free Olivetti Content Editor on Right side...
;;       Yes? - Do the following Before opening a .org file for editing/viewing:
;;         * Stretch your window frame out beyond 150 columns...
;;       The frame width will be automagically detected by this function...
;;       You will get a "double-wide" window frame with auto
;;       adjusted right / left panes with title bar: "Fancy Org Mode"
;;
;;    2. If you don't want Fancy Org Mode (double wide split panes) than keep
;;       your window narrow (i.e., the default 100 columns or less than 130)...
;;       In this mode, you still get the nicer mono font for writing, etc...
;;       but you don't get split panes or olivetti mode... This mode is better
;;       for things like finances, general org-mode planning, etc...
;;       This mode will automagically be set if you start with an initial frame
;;       width of less than 130 columns...  That's it...
;;
;;  With the two options above, I believe at this point this Emacs Org Mode
;;  based publishing environment is working just as well as my Scrivener Projects
;;  were in the past!  It will soon be exceeding anything that I could do with
;;  Scrivener and be future proof to boot!  Combined with Fountain mode for
;;  screenwriting, audio and VR scripting etc... When this project is done
;;  you will be glad to know "all your base are belong to" Emacs Org Mode! %^)

(defun me_org-mode-hook-func ()
  "Harmonic Alchemy Modular Emacs Fancy Org Mode Hook Function.  This function
takes care of setting up a nice org-mode writing environment for both planning
and / or writing-publishing"

  ;; Set default face to Courier Prime Emacs (A nice mono serif for writing)
  ;; NOTE: All settings below change the currently selected frame only...
  ;;       (other existing and future frames are not affected)
  (set-face-attribute 'default (selected-frame)
                      :family "Courier Prime Emacs"
                      :slant 'normal
                      :height 138
                      :weight 'normal
                      :width 'normal)

  ;; Set Default Face Flag (2 = org-mode face):
  (setq me--def-face 2)

  ;;  Test for "Wide Screen" (i.e., The user has set the frame
  ;;  wider than 130 columns). If true, Set up a split screen
  ;;  session with outline in left pane and content in right pane
  ;;  Resize frame & window dimensions for best fit..
  ;;  Set Frame Title to "HA Mod Emacs Fancy Org Mode"
  ;;  Enable olivetti-mode for distraction free writing...
  (if (> (frame-parameter nil 'width) 130)
      (progn

        ;; Set up WIDE SCREEN Frame size for writing/composition:
        (modify-frame-parameters nil
                                 (quote
                                  ((name   . "HA Mod Emacs - Fancy Org Mode")
                                   (height . 50)
                                   (width  . 155))))

        ;; Split Windows with org outline tree in narrow left window...
        (org-sidebar-tree)
        ;; Enable Olivetti Mode (100 column wide)
        (olivetti-mode)
        (olivetti-set-width 100))

    ;; OTHERWISE - Set NORMAL SCREEN Org Mode Frame size for org-mode
    ;; planning, finances, TODOs, etc. (no split windows)
    (progn
      (modify-frame-parameters nil
                               (quote
                                ((name   . "HA Mod Emacs - Normal Org Mode")
                                 (height . 42)
                                 (width  . 100))))))

  ;; Set Default Face Flag (to org-face) and Allways hide bullets no matter what...
  (setq me--def-face 2)
  (me_hide-org-bullets))

;;;
;; More Usage Notes for above hook function:
;;
;; NOTE1: Remove call to hide-org-bullets in function list above if you
;;        would like to see fancy org bullet headings instead...
;; NOTE2: This looks ugly if you are still using fancy scaleable
;;        fonts for headings as well, (IMHO)...  IF you want to try using
;;        Fancy Coloured Bullets etc., you most likely will want to disable
;;        Fancy scaleable fonts as well... To do that you will have to
;;        play around with org mode font styles in:
;;                      09-1-org-beautify-conf.el
;;        You will also find different org bullet styles within the above
;;        file as well.... (go ahead... hack away!)

;;;
;;  Add Above Hook Function to Org Mode startup list:

(add-hook 'org-mode-hook 'me_org-mode-hook-func)


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

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   END: [modular-emacs]:~/.emacs.d/lisp/modules/09-org-mode-pkg-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
