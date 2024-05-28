;;; init-org.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; org
(use-package
  org
  :ensure t
  :init
  (require 'org-indent)
  (require 'org-habit)
  (unless org-directory (setq-default org-directory "~/org"))
  (unless
    org-id-locations-file
    (setq org-id-locations-file (expand-file-name ".orgids" org-directory)))
  (unless org-agenda-files (setq-default org-agenda-files (list org-directory)))
  (setq-default
    ;; Different colors for different priority levels
    org-agenda-deadline-faces
    '
    ((1.001 . error)
      (1.0 . org-warning)
      (0.5 . org-upcoming-deadline)
      (0.0 . org-upcoming-distant-deadline))
    ;; Don't monopolize the whole frame just for the agenda
    org-agenda-window-setup 'current-window org-agenda-skip-unavailable-files t
    ;; Shift the agenda to show the previous 3 days and the next 7 days for
    ;; better context on your week. The past is less important than the future.
    org-agenda-span 10 org-agenda-start-on-weekday nil org-agenda-start-day "-3d"
    ;; Optimize `org-agenda' by inhibiting extra work while opening agenda
    ;; buffers in the background. They'll be "restarted" if the user switches to
    ;; them anyway (see `+org-exclude-agenda-buffers-from-workspace-h')
    org-agenda-inhibit-startup t)
  (setq org-log-done 'time)
  (setq org-closed-keep-when-no-todo t)
  (setq
    org-indirect-buffer-display
    'current-window
    org-enforce-todo-dependencies
    t
    org-entities-user
    '(("flat" "\\flat" nil "" "" "266D" "♭") ("sharp" "\\sharp" nil "" "" "266F" "♯"))
    org-fontify-done-headline
    t
    org-fontify-quote-and-verse-blocks
    t
    org-fontify-whole-heading-line
    t
    org-hide-leading-stars
    t
    org-image-actual-width
    nil
    org-imenu-depth
    6
    org-priority-faces
    '((?A . error) (?B . warning) (?C . success))
    org-startup-indented
    t
    org-tags-column
    0
    org-use-sub-superscripts
    '{}
    ;; `showeverything' is org's default, but it doesn't respect
    ;; `org-hide-block-startup' (#+startup: hideblocks), archive trees,
    ;; hidden drawers, or VISIBILITY properties. `nil' is equivalent, but
    ;; respects these settings.
    org-startup-folded
    nil)
  (setq
    org-refile-targets '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 3))
    ;; Without this, completers like ivy/helm are only given the first level of
    ;; each outline candidates. i.e. all the candidates under the "Tasks" heading
    ;; are just "Tasks/". This is unhelpful. We want the full path to each refile
    ;; target! e.g. FILE/Tasks/heading/subheading
    org-refile-use-outline-path 'file org-outline-path-complete-in-steps nil)
  (plist-put
    org-format-latex-options
    :scale 1.5) ; larger previews

  ;; HACK Face specs fed directly to `org-todo-keyword-faces' don't respect
  ;;      underlying faces like the `org-todo' face does, so we define our own
  ;;      intermediary faces that extend from org-todo.
  (with-no-warnings
    (custom-declare-face
      '+org-todo-active
      '((t (:inherit (bold font-lock-constant-face org-todo))))
      "")
    (custom-declare-face
      '+org-todo-project
      '((t (:inherit (bold font-lock-doc-face org-todo))))
      "")
    (custom-declare-face '+org-todo-onhold '((t (:inherit (bold warning org-todo)))) "")
    (custom-declare-face '+org-todo-cancel '((t (:inherit (bold error org-todo)))) ""))
  (setq
    org-todo-keywords
    '
    (
      (sequence
        "TODO(t)" ; A task that needs doing & is ready to do
        "PROJ(p)" ; A project, which usually contains other tasks
        "LOOP(r)" ; A recurring task
        "STRT(s)" ; A task that is in progress
        "WAIT(w)" ; Something external is holding up this task
        "HOLD(h)" ; This task is paused/on hold because of me
        "IDEA(i)" ; An unconfirmed and unapproved task or notion
        "|" "DONE(d)" ; Task successfully completed
        "KILL(k)") ; Task was cancelled, aborted, or is no longer applicable
      (sequence
        "[ ](T)" ; A task that needs doing
        "[-](S)" ; Task is in progress
        "[?](W)" ; Task is being held up or paused
        "|" "[X](D)") ; Task was completed
      (sequence "|" "OKAY(o)" "YES(y)" "NO(n)"))
    org-todo-keyword-faces
    '
    (("[-]" . +org-todo-active)
      ("STRT" . +org-todo-active)
      ("[?]" . +org-todo-onhold)
      ("WAIT" . +org-todo-onhold)
      ("HOLD" . +org-todo-onhold)
      ("PROJ" . +org-todo-project)
      ("NO" . +org-todo-cancel)
      ("KILL" . +org-todo-cancel)))
  (defvar
    +org-capture-todo-file "todo.org"
    "Default target for todo entries.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

  (defvar
    +org-capture-changelog-file "changelog.org"
    "Default target for changelog entries.

Is relative to `org-directory' unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

  (defvar
    +org-capture-notes-file "notes.org"
    "Default target for storing notes.

Used as a fall back file for org-capture.el, for templates that do not specify a
target file.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

  (defvar
    +org-capture-journal-file "journal.org"
    "Default target for storing timestamped journal entries.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

  (defvar
    +org-capture-projects-file
    "projects.org"
    "Default, centralized target for org-capture templates.")

  (defvar +org-habit-graph-padding 2 "The padding added to the end of the consistency graph")

  (defvar
    +org-habit-min-width
    30
    "Hides the consistency graph if the `org-habit-graph-column' is less than this value")

  (defvar
    +org-habit-graph-window-ratio
    0.3
    "The ratio of the consistency graphs relative to the window width")

  (defvar
    +org-startup-with-animated-gifs
    nil
    "If non-nil, and the cursor is over a gif inline-image preview, animate it!")
  (setq
    org-default-notes-file
    (expand-file-name +org-capture-notes-file org-directory)
    +org-capture-journal-file
    (expand-file-name +org-capture-journal-file org-directory)
    org-capture-templates
    '
    (
      ("t"
        "Personal todo"
        entry
        (file+headline +org-capture-todo-file "Inbox")
        "* [ ] %?\n%i\n%a"
        :prepend t)
      ("n"
        "Personal notes"
        entry
        (file+headline +org-capture-notes-file "Inbox")
        "* %u %?\n%i\n%a"
        :prepend t)
      ("j"
        "Journal"
        entry
        (file+olp+datetree +org-capture-journal-file)
        "* %U %?\n%i\n%a"
        :prepend t)

      ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
      ;; {todo,notes,changelog}.org file is found in a parent directory.
      ;; Uses the basename from `+org-capture-todo-file',
      ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
      ("p" "Templates for projects")
      ("pt" "Project-local todo" entry ; {project-root}/todo.org
        (file+headline +org-capture-project-todo-file "Inbox") "* TODO %?\n%i\n%a"
        :prepend t)
      ("pn" "Project-local notes" entry ; {project-root}/notes.org
        (file+headline +org-capture-project-notes-file "Inbox") "* %U %?\n%i\n%a"
        :prepend t)
      ("pc" "Project-local changelog" entry ; {project-root}/changelog.org
        (file+headline +org-capture-project-changelog-file "Unreleased") "* %U %?\n%i\n%a"
        :prepend t)

      ;; Will use {org-directory}/{+org-capture-projects-file} and store
      ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
      ;; support `:parents' to specify what headings to put them under, e.g.
      ;; :parents ("Projects")
      ("o" "Centralized templates for projects")
      ("ot"
        "Project todo"
        entry
        (function +org-capture-central-project-todo-file)
        "* TODO %?\n %i\n %a"
        :heading "Tasks"
        :prepend nil)
      ("on"
        "Project notes"
        entry
        (function +org-capture-central-project-notes-file)
        "* %U %?\n %i\n %a"
        :heading "Notes"
        :prepend t)
      ("oc"
        "Project changelog"
        entry
        (function +org-capture-central-project-changelog-file)
        "* %U %?\n %i\n %a"
        :heading "Changelog"
        :prepend t)))

  (add-hook 'org-after-refile-insert-hook #'save-buffer)

  :bind (("C-c a" . 'org-agenda) ("C-x c" . 'org-capture)))

(use-package
  evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-define-key
    'normal
    org-mode-map
    (kbd "TAB")
    'org-cycle
    ">"
    'org-shiftmetaright
    "<"
    'org-shiftmetaleft)
  (evil-org-agenda-set-keys))

;; markdown
(use-package
  markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map ("C-c C-e" . markdown-do)))

;; 记账用
(use-package
  beancount
  :straight '(beancount :host github :repo "beancount/beancount-mode")
  :init
  (add-hook 'beancount-mode-hook (lambda () (setq-local electric-indent-chars nil)))
  (add-hook 'beancount-mode-hook #'flymake-bean-check-enable)
  :mode (("\\.beancount\\'" . beancount-mode))
  :config
  (define-key beancount-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
  (define-key beancount-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading))

;; org-roam
(use-package
  org-roam-protocol
  :init
  (setq
    org-roam-capture-ref-templates
    '
    (
      ("a" "Annotation" plain "%U ${body}\n"
        :target
        (file+head
          "${slug}.org"
          "#+title: ${title}\n#+roam_key: ${ref}\n#+roam_alias:\n#+roam_tags:\n\n")
        :immediate-finish t
        :unnarrowed t)
      ("r" "ref" plain ""
        :target
        (file+head
          "${slug}.org"
          "#+title: ${title}\n#+roam_key: ${ref}\n#+roam_alias:\n#+roam_tags:\n\n")
        :immediate-finish t
        :unnarrowed t))))
(use-package
  org-roam
  :ensure t
  :custom (org-roam-directory (file-truename "~/org-roam/"))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
    ("C-c n f" . org-roam-node-find)
    ("C-c n g" . org-roam-graph)
    ("C-c n i" . org-roam-node-insert)
    ("C-c n c" . org-roam-capture)
    ;; Dailies
    ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq
    org-roam-node-display-template
    (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  (setq find-file-visit-truename t)
  ;; for org-roam-buffer-toggle
  ;; Use side-window like V1
  ;; This can take advantage of slots available with it
  (add-to-list
    'display-buffer-alist
    '
    ("\\*org-roam\\*"
      (display-buffer-in-side-window)
      (side . right)
      (slot . 0)
      (window-width . 0.25)
      (preserve-size . (t . nil))
      (window-parameters . ((no-other-window . t) (no-delete-other-windows . t)))))
  (setq
    org-roam-capture-templates
    '
    (
      ("m"
        "main"
        plain
        "%?"
        :if-new (file+head "main/${slug}.org" "#+title: ${title}\n")
        :immediate-finish t
        :unnarrowed t)
      ("r"
        "reference"
        plain
        "%?"
        :if-new (file+head "reference/${title}.org" "#+title: ${title}\n")
        :immediate-finish t
        :unnarrowed t)
      ("a"
        "article"
        plain
        "%?"
        :if-new (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
        :immediate-finish t
        :unnarrowed t))))
(provide 'init-org)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-org.el ends here
