;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   [modular-emacs]:~/.emacs.d/lisp/modules/09-3-org-PM-conf.el
;;
;;   This is a sub module of 09-org-mode-pkg-conf.el which sets up my Org-Mode
;;   Project Management Process:
;;        (e.g., Capture, GTD, TODO, Tasks, Agenda, etc.)
;;   Schemes are based on the "Autofocus GTD Process" I use along with the
;;   Timesheets emacs package and a few other things like mind maps etc...
;;
;;   Override this file by placing a copy of it into "my-modules" then change
;;   settings below to suit your needs and planning style...
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;; Settings made within top level: 09-org-mode-pkg-conf.el
;; before this file was loaded:
;;
;;      my-org-dir            = Top Level Org Directory
;;      my-org-agenda-files   = my-org-dir/00-Agenda-files
;;      my-org-templates      = my-org-dir/02-Templates
;;      my-org-files          = my-org-dir/03-Private

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Define Custom Org-files:

(setq default-org-note "refile.org")
(setq default-org-diary "diary.org")
(setq private-org-note "private.org")
(setq work-log "logbook-work.org")
(setq personal-log "logbook-personal.org")
(setq autofocus-notebook "Autofocus-notebook.org")

;; Capture Template Files:

(setq org-todo-template "t-private-todo.txt")

;; Set my-org-files/refile.org as Default Org Note:

(setq org-default-notes-file (expand-file-name default-org-note my-org-files))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Set Up Capture templates for: TODO tasks, Notes,
;; appointments, phone calls, and org-protocol

(setq
 org-capture-templates
 (quote
  (
   ("p" "Private Templates")

   ("pt" "ToDo Entry" entry
    (file+headline
     (expand-file-name private-org-note my-org-files)
     "Capture")
    (file (expand-file-name org-todo-template my-org-templates))
    :empty-lines-before 1)

   ("wl" "Logbook entry" entry
    (file+datetree "logbook-work.org") "** %U - %^{Activity}  :LOG:")

   ;;; This section commented out until later configuration...
   ;; ("t" "todo" entry
   ;;  (file
   ;;   (expand-file-name default-org-note my-org-files)
   ;;   "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t))

   ;; ("r" "respond" entry
   ;;  (file
   ;;   (expand-file-name default-org-note my-org-files)
   ;;   "* TODO Respond to %:from on %:subject\n%U\n%a\n"
   ;;   :clock-in t :clock-resume t :immediate-finish t))

   ;; ("n" "note" entry
   ;;  (file (expand-file-name default-org-note my-org-files)
   ;;        "* %? :NOTE:\n%U\n%a\n"
   ;;        :clock-in t :clock-resume t))

   ;; ("j" "Journal" entry
   ;;  (file+datetree (expand-file-name default-org-diary my-org-files)
   ;;                 "* %?\n%U\n"
   ;;                 :clock-in t :clock-resume t))

   ;; ("w" "org-protocol" entry
   ;;  (file (expand-file-name default-org-note my-org-files))
   ;;  "* TODO Review %c\n%U\n"
   ;;  :immediate-finish t)

   ;; ("p" "Phone call" entry
   ;;  (file (expand-file-name default-org-note my-org-files))
   ;;  "* PHONE %? :PHONE:\n%U"
   ;;  :clock-in t :clock-resume t)

   ;; ("h" "Habit" entry
   ;;  (file (expand-file-name default-org-note my-org-files))
   ;;  "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")

   )))

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

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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


;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   Org Mind Map Configuration:
;;   Note:  Graphviz must be installed on your system in order for these
;;          Forms to work!!!  You may see warnings on Emacs startup but it
;;          will not hurt anything except trying to use Org Mind Map...

(setq org-mind-map-engine "dot")      ; Default. Directed Graph
(setq org-mind-map-engine "neato")    ; Undirected Spring Graph
(setq org-mind-map-engine "twopi")    ; Radial Layout
(setq org-mind-map-engine "fdp")      ; Undirected Spring Force-Directed
(setq org-mind-map-engine "sfdp")     ; Multiscale version of fdp for the 
                                      ; layout of large graphs
(setq org-mind-map-engine "twopi")    ; Radial layouts
(setq org-mind-map-engine "circo")    ; Circular Layout

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   END: [modular-emacs]:~/.emacs.d/lisp/modules/09-3-org-PM-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
