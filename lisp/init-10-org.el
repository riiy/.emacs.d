;;; init-10-org.el --- Load the full configuration -*- lexical-binding: t -*-
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
  :bind (("C-c a" . 'org-agenda) :map org-mode-map ("C-c C-q" . counsel-org-tag)))

(provide 'init-10-org)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-10-org.el ends here
