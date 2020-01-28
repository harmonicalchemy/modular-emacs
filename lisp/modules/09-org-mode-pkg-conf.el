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
  '(gnuplot-mode
    sass-mode
    abc-mode))

;;;
;; Install required packages:

(mapc (lambda (p) (package-install p))
      me--req-org-packages)

;;;
;; Define LOCAL USER MASTER ORG FILES DIRECTORY:
;; The place where you will be keeping all your private org files and
;; sub-directories... You may already have a special directory for your
;; org files.  In that case be sure to read all the notes below!
;;
;; IMPORTANT NOTE: Change the path in this first form below to match your
;; Org-Files Home Directory. All path definitions below this form will be
;; relative to this defined MY-ORG-DIR... (if you don't have a special
;; org-notes directory on your file system yet, create that directory now and
;; change this next form below to reflect your new directory's path)

(defconst my-org-dir
  (file-name-as-directory
   (expand-file-name "~/Path/To/Your/Private/Org-Docs")))

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

;;;
;; Define LOCAL USER ORG FILES SUB Directories:
;;
;; Note: These constants are also used by 15-Accounting-pkg-conf.el
;;       which gets loaded later in the module chain...

(defconst my-org-templates
  (file-name-as-directory
   (expand-file-name "02-Templates" my-org-dir)))

(defconst my-org-files
  (file-name-as-directory
   (expand-file-name "03-Private" my-org-dir)))

(defconst my-org-agenda-files
  (file-name-as-directory
   (expand-file-name "03-Private" my-org-dir)))

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   Advanced Org-Mode Configurations: (currently under development)
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(require 'org)
(require 'org-faces)

;;;
;; Make sure TEXINPUTS is set to: elpa/auctex-nn.nn.n/latex
;; require 'preview below should set this as long as auctex is installed...

(unless (require 'latex nil t)
  (message "MELPA package 'latex not yet installed..."))

(require 'preview)

;;;
;; Setup my Default Org-Mode Keywords:
;; (adjust this list to fit your planning style)

(setq org-todo-keyword-faces
      (setq org-todo-keywords
	    (quote ((sequence "NEXT(n)" "TODO(t)" "NOW(o)" "ACTIVE(a)" "REPEATING(r)" "WAITING(w)" "SOMEDAY(s)" "HOME(h)" "|" "DONE(d)" "CANCELLED(c)" "ARCHIVED(a)")
		    (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE")))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
	      ("NEXT" :foreground "blue" :weight bold)
	      ("DONE" :foreground "forest green" :weight bold)
	      ("WAITING" :foreground "orange" :weight bold)
	      ("HOLD" :foreground "magenta" :weight bold)
	      ("CANCELLED" :foreground "forest green" :weight bold)
	      ("PHONE" :foreground "forest green" :weight bold))))

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

(setq org-startup-folded 'content)

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

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   Alisha's Advanced Org-Mode Configurations:
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
;   (lisp . t)
   (lilypond . t)
   (abc . t)
   (asymptote . t)
   (org . t)
   (gnuplot . t)
   (haskell . nil)
   (latex . t)
   (ledger . t) ;This adds support for hledger hopefully...
   (ocaml . nil)
   (octave . t)
   (sass . t)
   (python . t)
   (js . t)
   (ruby . t)
   (screen . nil)
   (sql . nil)
   (sqlite . t)))


;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   END: [modular-emacs]:~/.emacs.d/lisp/modules/09-org-mode-pkg-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
