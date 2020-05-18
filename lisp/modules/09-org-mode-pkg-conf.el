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
     (expand-file-name "~/Path/To/Your-MacOS-Org-Docs"))))

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


;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   Beautify Settings for Org-Mode: (constantly under revision %^)
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
;; Emojify for Org-Mode - Information Only:
;;
;; Searching emojis:
;;
;;   The command:
;;
;;       emojify-apropos-emoji
;;
;;   can be used to display emojis that match given regexp/apropos pattern.
;;   The results are displayed in a specialized buffer, where 'w' or 'c' can be
;;   used to copy emojis to the kill ring.
;;
;; Inserting emojis:
;;
;;   The command:
;;
;;       emojify-insert-emoji
;;
;;   can be used to insert emojis interactively. While the command works with
;;   vanilla Emacs completion system, the experience would be better with
;;   something like Helm, Ivy, Icicles or Ido depending on you preference.
;;
;; Describing emojis:
;;
;;   The command:
;;
;;       emojify-describe-emoji-at-point
;;
;;   can be used to view explanation about the command displayed at point.
;;   Additionally the command emojify-describe-emoji can be used to display
;;   description for an arbitrary emoji.
;;
;; Listing all emojis:
;;
;;   The command:
;;
;;       emojify-list-emojis
;;
;;   can be used to view all the available emojis in a list form.
;;
;; Configuring how emojis are displayed:
;;
;;   By default emojis are displayed using images. However you can instruct
;;   emojify to display it using unicode characters or ascii characters.
;;   To do so, customize the variable:
;;
;;       emojify-display-style
;;
;;   You can set it one of the following values:
;;
;;      image - Display emojis using images, obviously this requires the Emacs
;;              instance to support image
;;
;;     unicode - Display emojis using unicode characters, this might be a good
;;               option on platforms with good emoji fonts
;;
;;     ascii - This is simplest and does not require any external dependencies
;;             In this case emojify will display ascii equivalents of github
;;             style emojis.
;;
;;  More Info At:
;;
;;      https://github.com/iqbalansari/emacs-emojify
;;
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; I have chosen to leave Emojify Mode disabled and 
;; instead only use the option to enable it manually
;; if needed.   I find using the Symbola unicode font
;; by itself, without using emojify mode, works fine...
;; So far, I am sourcing all my symbols and icons from
;; the Symbola font... Symbola contains simple clear
;; line drawing representations of all the standard
;; emojis... and Symbola is available on all platforms!
;;
;; If you would rather, or also wish to use Emojify mode,
;; Un-comment the single line form below...  This will 
;; enable Emojify Mode Globally at Emacs Startup...

;(add-hook 'after-init-hook #'global-emojify-mode)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Make better Org-Mode Headers:
;;
;; This rather complicated eLisp code came from:
;; http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html
;; It may need some improvements (simplifications) as Howard said in his post...

;; Set default faces: (disabled... I don't need this.  It's for experiments)
;; Instead set these in your custom Blackboard Theme... I was testing them here...
;(custom-theme-set-faces
; 'user
; '(org-default ((t (:family "Advent Pro Light" :height 125 :weight light))))
; '(variable-pitch ((t (:family "Averia Serif Libre" :height 120 :weight light))))
; '(fixed-pitch ((t ( :family "Go Mono for Powerline" :slant normal :weight normal :height 1.0 :width normal)))))

;; Customize Org headings:

(let* ((variable-tuple (cond ((x-list-fonts "Averia Serif Libre") '(:font "Averia Serif Libre"))
                             ((x-list-fonts "Averia Libre Light") '(:font "Averia Libre Light"))
                             (nil (warn "Cannot find a Sans Serif Font.  Trying to Install Averia Serif Libre."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight normal )))
  
  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple :height 1.13 :foreground "AntiqueWhite" ))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple :height 1.17 :foreground "AntiqueWhite" ))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple :height 1.20 :foreground "AntiqueWhite" ))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple :height 1.23 :foreground "AntiqueWhite" ))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.28 :foreground "AntiqueWhite" ))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.38 :foreground "AntiqueWhite" ))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.62 :foreground "AntiqueWhite" ))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.88 :foreground "AntiqueWhite" ))))
                          `(org-document-title
                            ((t (,@headline ,@variable-tuple :height 1.5 :foreground "AntiqueWhite" :underline nil))))))


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
;;  from the org-mode hook below...
;;
;;  I also have a Xfk Cmd-Mode key "p" defined
;;  to invoke this function... (turning off bullets)
;;  But no way to turn them back on...
;;  TODO: Make this a toggle so you can turn them
;;        back on for other non writing related work.
;;        Then you don't have to mess with it here...
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun me_hide-org-bullets ()
  "Hide the Org Bullets... TODO: make this into a toggle"
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
;;  Modular Emacs - Org Mode Hook:

(defun me_org-mode-hook ()
  ;; Set default face to Go Mono for Powerline (A nice mono serif for writing)...
  (set-face-attribute 'default nil
                      :family "Go Mono for Powerline"
                      :slant 'normal
                      :height 120
                      :weight 'normal
                      :width 'normal)
  (me_hide-org-bullets))   ;; Remove this last element if you want to see fancy bullets...

(add-hook 'org-mode-hook 'me_org-mode-hook)


;;;
;; Setup my Default Org-Mode Keywords:
;; The above are based on general GTD schemes I am using...
;; (adjust this list to fit your own planning style)

(setq org-todo-keywords
      (quote ( (sequence "TODO(t)" "NEXT(x)" "|" "DONE(d!/!)")
               (sequence "ACTIVE(a)" "REPEATING(r)" "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE")
	       (sequence  "NEW(n)" "NOW(o)" "SOMEDAY(s)"  "|" "ARCHIVED(a)"))))

;;;
;; Setup my Default Org-Mode Keywords: (with fancy UTF 8 symbols)
;; NOTE: I decided this was cluttering things up to much so I disabled it...
;(setq org-todo-keywords
;      (quote ( (sequence "☞ TODO(t)" "⚡ NEXT(x)" "|" "✔ DONE(d!/!)")
;               (sequence "ACTIVE(a)" "↺ REPEATING(r)" "⚑ WAITING(w@/!)" "⨂ HOLD(h@/!)" "|" "✘ CANCELLED(c@/!)" "PHONE(p)")
;	       (sequence  "NEW(n)" "⦾ NOW(o)" "SOMEDAY(s)"  "|" "ARCHIVED(a)"))))

;;;
;; Set Org TODO keyword faces:
;; (don't settle for boring red, green, blue... Be creative!)
;; INFO:  To lookup the list of Emacs colours that can be used by name use:
;;
;;                         M-x list-colors-display
;;
;; NOTE:  You can also do this for tag faces:
;;        Copy this form when you are ready to do that and add in your tags as a new element in the list...
;;
;(setq org-tag-faces
;      (quote (
;              ( "TAG-NAME" . (:family "Hermit" :height 100 :foreground "red" :weight bold))
;              ;; Add More Tag Elements to the list...
;              ;;
;              )))

(setq org-todo-keyword-faces
      (quote (
              ("TODO" . (:family "Hermit" :height 100 :foreground "red" :weight bold))
              ("NEXT" . (:family "Hermit" :height 100 :foreground "BlueViolet" :weight bold))
              ("DONE" . (:family "Hermit" :height 100 :foreground "green2" :weight bold))
              ("ACTIVE" . (:family "Hermit" :height 100 :foreground "chocolate1" :weight bold))
              ("REPEATING" . (:family "Hermit" :height 100 :foreground "DeepSkyBlue" :weight bold))
              ("WAITING" . (:family "Hermit" :height 100 :foreground "lavender" :weight bold))
              ("HOLD" . (:family "Hermit" :height 100 :foreground "gray62" :weight bold))
              ("CANCELLED" . (:family "Hermit" :height 100 :foreground "SlateGray" :weight bold))
              ("PHONE" . (:family "Hermit" :height 100 :foreground "DarkOrange" :weight bold))
              ("NEW" . (:family "Hermit" :height 100 :foreground "DodgerBlue" :weight bold))
              ("NOW" . (:family "Hermit" :height 100 :foreground "HotPink" :weight bold))
              ("SOMEDAY" . (:family "Hermit" :height 100 :foreground "gold" :weight bold))
              ("ARCHIVED" . (:family "Hermit" :height 100 :foreground "AntiqueWhite" :weight bold)))))

;; For some reason trying to do this with custom-theme-set-faces did not work... But the above does...
;; I need to come back and re-examine this code later...
;;
;(let* ((fixed-tuple (cond ((x-list-fonts "Hermit") '(:face "Hermit"))
;                          (nil (warn "Cannot find Hermit Font. Is it installed on this system?")))))
;  (custom-theme-set-faces 'user
;                          '(org-todo-keyword-faces (("TODO" :face "Hermit" :height 100 :foreground "red" :weight bold)))
;                          '(org-todo-keyword-faces (("NEXT" :face "Hermit" :height 100 :foreground "BlueViolet" :weight bold)))
;                          '(org-todo-keyword-faces (("DONE" :face "Hermit" :height 100 :foreground "green2" :weight bold)))
;                          '(org-todo-keyword-faces (("ACTIVE" :face "Hermit" :height 100 :foreground "chocolate1" :weight bold)))
;                          '(org-todo-keyword-faces (("REPEATING" :face "Hermit" :height 100 :foreground "DeepSkyBlue" :weight bold)))
;                          '(org-todo-keyword-faces (("WAITING" :face "Hermit" :height 100 :foreground "lavender" :weight bold)))
;                          '(org-todo-keyword-faces (("HOLD" :face "Hermit" :height 100 :foreground "gray62" :weight bold)))
;                          '(org-todo-keyword-faces (("CANCELLED" :face "Hermit" :height 100 :foreground "SlateGray" :weight bold)))
;                          '(org-todo-keyword-faces (("PHONE" :face "Hermit" :height 100 :foreground "DarkOrange" :weight bold)))
;                          '(org-todo-keyword-faces (("NEW" :face "Hermit" :height 100 :foreground "DodgerBlue" :weight bold)))
;                          '(org-todo-keyword-faces (("NOW" :face "Hermit" :height 100 :foreground "HotPink" :weight bold)))
;                          '(org-todo-keyword-faces (("SOMEDAY" :face "Hermit" :height 100 :foreground "gold" :weight bold)))
;                          '(org-todo-keyword-faces (("ARCHIVED" :family "Hermit" :height 100 :foreground "AntiqueWhite" :weight bold)))))


;;;
;; Define Custom Org-files:

(setq default-org-note "refile.org")
(setq default-org-diary "diary.org")

(setq org-default-notes-file (expand-file-name default-org-note my-org-dir))

;;;
;; Set Up Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol

(setq org-capture-templates
      (quote (("t" "todo" entry (file (expand-file-name default-org-note my-org-files)
	       "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t))
	       ("r" "respond" entry (file (expand-file-name default-org-note my-org-files) "* TODO Respond to %:from on %:subject\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t))
	        ("n" "note" entry (file (expand-file-name default-org-note my-org-files) "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t))
	      ("j" "Journal" entry (file+datetree (expand-file-name default-org-diary my-org-files)
	       "* %?\n%U\n" :clock-in t :clock-resume t))
	      ("w" "org-protocol" entry (file (expand-file-name default-org-note my-org-files))
	       "* TODO Review %c\n%U\n" :immediate-finish t)
	      ("p" "Phone call" entry (file (expand-file-name default-org-note my-org-files))
	       "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
	      ("h" "Habit" entry (file (expand-file-name default-org-note my-org-files))
	       "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

;;;
;; Dim blocked tasks
(setq org-agenda-dim-blocked-tasks t)

;;; 
;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;;; 
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)

;;; 
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;;; 
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)

;;; 
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)

;;; 
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)

;;; 
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)

;;; 
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))

;;; 
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

;;; 
;; round time
(setq org-time-stamp-rounding-minutes (quote (1 1)))

;;; 
;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;;; 
;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;;; 
;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;;; 
;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

;;; 
;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;;; 
;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;;; 
;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

;;; 
;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;;; 
;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;;; 
;; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      (quote ((agenda habit-down time-up user-defined-up priority-down effort-up category-keep)
	      (todo category-up priority-down effort-up)
	      (tags category-up priority-down effort-up)
	      (search category-up))))

;;; 
;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)

;;; 
;; Enable display of the time grid so we can see the marker for the current time
(setq org-agenda-time-grid (quote ((daily today remove-match)
				   #("----------------" 0 16 (org-heading t))
				   (830 1000 1200 1300 1500 1700))))

;;; 
;; Display tags farther right
(setq org-agenda-tags-column -102)

(setq org-enforce-todo-dependencies t)

(setq org-hide-leading-stars t)

(setq org-startup-indented t)

(setq org-cycle-separator-lines 0)

(setq org-blank-before-new-entry (quote ((heading)
                                         (plain-list-item . auto))))

(setq org-insert-heading-respect-content nil)

(setq org-reverse-note-order nil)

(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings (quote ((default))))

(setq org-id-method (quote uuidgen))

(setq org-deadline-warning-days 30)

(setq org-table-export-default-format "orgtbl-to-tsv")

(setq org-link-frame-setup (quote ((vm . vm-visit-folder)
                                   (gnus . org-gnus-no-new-news)
                                   (file . find-file))))
 

;; Use the current window for C-c ' source editing

(setq org-src-window-setup 'current-window)

(setq org-log-done (quote time))
(setq org-log-into-drawer "LOGBOOK")

(setq org-startup-folded 'overview)

(setq org-alphabetical-lists t)

;;; 
;; Custom agenda command definitions

(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
	       ((org-agenda-overriding-header "Notes")
		(org-tags-match-list-sublevels t)))
	      ("h" "Habits" tags-todo "STYLE=\"habit\""
	       ((org-agenda-overriding-header "Habits")
		(org-agenda-sorting-strategy
		 '(todo-state-down effort-up category-keep))))
	      (" " "Agenda"
	       ((agenda "" nil)
		(tags "REFILE"
		      ((org-agenda-overriding-header "Tasks to Refile")
		       (org-tags-match-list-sublevels nil)))
		(tags-todo "-CANCELLED/!"
			   ((org-agenda-overriding-header "Stuck Projects")
			    (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
		(tags-todo "-WAITING-CANCELLED/!NEXT"
			   ((org-agenda-overriding-header "Next Tasks")
			    (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
			    (org-agenda-todo-ignore-scheduled t)
			    (org-agenda-todo-ignore-deadlines t)
			    (org-agenda-todo-ignore-with-date t)
			    (org-tags-match-list-sublevels t)
			    (org-agenda-sorting-strategy
			     '(todo-state-down effort-up category-keep))))
		(tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
			   ((org-agenda-overriding-header "Tasks")
			    (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
			    (org-agenda-todo-ignore-scheduled t)
			    (org-agenda-todo-ignore-deadlines t)
			    (org-agenda-todo-ignore-with-date t)
			    (org-agenda-sorting-strategy
			     '(category-keep))))
		(tags-todo "-HOLD-CANCELLED/!"
			   ((org-agenda-overriding-header "Projects")
			    (org-agenda-skip-function 'bh/skip-non-projects)
			    (org-agenda-sorting-strategy
			     '(category-keep))))
		(tags-todo "-CANCELLED+WAITING/!"
			   ((org-agenda-overriding-header "Waiting and Postponed Tasks")
			    (org-agenda-skip-function 'bh/skip-stuck-projects)
			    (org-tags-match-list-sublevels nil)
			    (org-agenda-todo-ignore-scheduled 'future)
			    (org-agenda-todo-ignore-deadlines 'future)))
		(tags "-REFILE/"
		      ((org-agenda-overriding-header "Tasks to Archive")
		       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
		       (org-tags-match-list-sublevels nil))))
	       nil)
	      ("r" "Tasks to Refile" tags "REFILE"
	       ((org-agenda-overriding-header "Tasks to Refile")
		(org-tags-match-list-sublevels nil)))
	      ("#" "Stuck Projects" tags-todo "-CANCELLED/!"
	       ((org-agenda-overriding-header "Stuck Projects")
		(org-agenda-skip-function 'bh/skip-non-stuck-projects)))
	      ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT"
	       ((org-agenda-overriding-header "Next Tasks")
		(org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
		(org-agenda-todo-ignore-scheduled t)
		(org-agenda-todo-ignore-deadlines t)
		(org-agenda-todo-ignore-with-date t)
		(org-tags-match-list-sublevels t)
		(org-agenda-sorting-strategy
		 '(todo-state-down effort-up category-keep))))
	      ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
	       ((org-agenda-overriding-header "Tasks")
		(org-agenda-skip-function 'bh/skip-project-tasks-maybe)
		(org-agenda-sorting-strategy
		 '(category-keep))))
	      ("p" "Projects" tags-todo "-HOLD-CANCELLED/!"
	       ((org-agenda-overriding-header "Projects")
		(org-agenda-skip-function 'bh/skip-non-projects)
		(org-agenda-sorting-strategy
		 '(category-keep))))
	      ("w" "Waiting Tasks" tags-todo "-CANCELLED+WAITING/!"
	       ((org-agenda-overriding-header "Waiting and Postponed tasks"))
	       (org-tags-match-list-sublevels nil))
	      ("A" "Tasks to Archive" tags "-REFILE/"
	       ((org-agenda-overriding-header "Tasks to Archive")
		(org-agenda-skip-function 'bh/skip-non-archivable-tasks)
		(org-tags-match-list-sublevels nil))))))


;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Org Mind Map Configuration:
;; Note:  Graphviz must be installed on your system before enabling these next
;;        Forms!!!

(setq org-mind-map-engine "dot")      ; Default. Directed Graph
(setq org-mind-map-engine "neato")    ; Undirected Spring Graph
(setq org-mind-map-engine "twopi")    ; Radial Layout
(setq org-mind-map-engine "fdp")      ; Undirected Spring Force-Directed
(setq org-mind-map-engine "sfdp")     ; Multiscale version of fdp for the 
                                      ; layout of large graphs
(setq org-mind-map-engine "twopi")    ; Radial layouts
(setq org-mind-map-engine "circo")    ; Circular Layout


;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   Alisha's Advanced Org-Mode Configurations:

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Org Outline Tree on left | content on 
;;  right Functions...
;;
;; Open sub elements in right window pane,
;; move cursor focus to right window pane...

(defun me_org-tree-open-in-right-win ()
  (interactive)
  (org-tree-to-indirect-buffer)
  (windmove-right))

;; Open sub elements in right window pane,
;; Leave cursor in left outline window pane...
;; This one is redundant... I could just assign a key
;; right? lol, but maybe I will expand this into something
;; useful later...

(defun me_org-tree-open-in-right-no-focus ()
  (interactive)
  (org-tree-to-indirect-buffer))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Org Mode Export Section - Settings:

;; Exporters:

(require 'ox-md)
(require 'ox-latex)

;;;
;; Hide the Emphasis Markup:
;; (e.g., /.../ for italics, *...* for bold, etc.)
;;
;; Disable this form if you want to see them...
;;
;; TIP: The invisible characters are easy enough to remove
;;      as there will be a small space in its place...
;;      You can backspace over it to remove it and then
;;      you will be able to see and remove the other one as well...

(setq org-hide-emphasis-markers t)

;;;
;; Position tags right after last char of headline:
;; (This is mostly to fix problems with variable-pitch
;; It does not seem to be a problem with fixed-pitch)
;; NOTE: I disabled this thing... It was causing an
;;       error on load!  I need to look into this
;;       later and fix it, or get rid of it...
;;       I am not even sure if I need it as I am
;;       not using Emacs in global scaled mode, or
;;       what ever that's called... lol...
;;       There may be more to this than I realized! %^)
;;       I will get to it later when I start messing with org tags...
;(org-tags-column 0)

;;;
;; speed keys for quick navigation:

(setq org-use-speed-commands 1)

;;;
;; set maximum indentation for org-mode description lists:

(setq org-list-description-max-indent 5)

;;;
;; prevent org-mode demoting heading also shifting text inside sections:

(setq org-adapt-indentation nil)

;;;
;; Stop Inline Images Being Too Big:

(setq org-image-actual-width '(500))

;;;
;; Automatically Refresh Inline Images:
;; REF: http://emacs.stackexchange.com/questions/3302/live-refresh-of-inline-images-with-org-display-inline-images

(defun shk-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(add-hook 'org-babel-after-execute-hook 'shk-fix-inline-images)


;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   Org Mode Export Options:

;;;
;; syntax highlight code blocks:

(setq org-src-fontify-natively t)

;;;
;; put caption below in tables:

(setq org-export-latex-table-caption-above nil)
(setq org-latex-table-caption-above nil)

;;;
;; don't export tags:
;;
;; Note: You may want to change this later if tags are important
;;       to your exported docs...

(setq org-export-with-tags nil)

;;;
;; Enable extra org-babel language-specific packages:

(require 'ob-lisp)

;;;
;; Make sure TEXINPUTS is set to: elpa/auctex-nn.nn.n/latex
;; require 'preview below should set this as long as auctex is installed...

(unless (require 'latex nil t)
  (message "MELPA package 'latex not yet installed..."))

(require 'preview)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Org Babel Active Language Configurations:
;;      t = enable     nil = disable
;;
;; Prerequisites:
;;   ABC requires: abc-mode and the following
;;   external programs:
;;     abcm2ps (Install with Macports on MacOS)
;;     ps2pdf (bundled with GhostScript)
;; Usage:
;;    Babel adds some new elements to code blocks.
;;    The basic structure becomes:
;;
;;      #+BEGIN_SRC language  org-switches header-arguments
;;      ,body
;;      #+END_SRC
;;
;;    Compile Babel Code blocks in the standard way using
;;    C-c C-c while the cursor is within the code block.
;;    For Example:
;;      C-c C-c within an ABC block will compile the block.
;;      into nice music notation...
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (abc . t)
   (asymptote . t)
   (awk . t)
   (calc . t)
   (css . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (haskell . nil)
   (java . t)
   (js . t)
   (latex . t)
   (ledger . t) ;This adds support for hledger hopefully...
   (lilypond . t)
   (lua . nil)
   (ocaml . nil)
   (octave . t)
   (org . t)
   (perl . t)
   (python . t)
   (ruby . t)
   (sass . t)
   (sed . t)
   (screen . nil)
   (shell . t)
   (sql . nil)
   (sqlite . t)))

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   END: [modular-emacs]:~/.emacs.d/lisp/modules/09-org-mode-pkg-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
