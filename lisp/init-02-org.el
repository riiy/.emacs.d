;;; init-02-org.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; org

;;; Code:
(setq org-directory (file-truename "~/org/"))
(require 'org-habit)
(setq org-default-properties (cons "RESET_SUBTASKS" org-default-properties))
(defun org-reset-subtask-state-subtree ()
  "Reset all subtasks in an entry subtree."
  (interactive "*")
  (if (org-before-first-heading-p)
      (error "Not inside a tree")
    (save-excursion
      (save-restriction (org-narrow-to-subtree)
                        (org-fold-show-subtree) (goto-char (point-min))
                        (beginning-of-line 2)
                        (narrow-to-region (point) (point-max))
                        (org-map-entries
                         '(when (member (org-get-todo-state) org-done-keywords)
                            (org-todo (car org-todo-keywords))))
                        ))))
(defun org-reset-subtask-state-maybe ()
  "Reset all subtasks in an entry if the `RESET_SUBTASKS' property is set"
  (interactive "*")
  (if (org-entry-get (point) "RESET_SUBTASKS")
      (org-reset-subtask-state-subtree)))
(defun org-subtask-reset ()
  (when (member org-state org-done-keywords) ;; org-state dynamically bound in org.el/org-todo
    (org-reset-subtask-state-maybe)
    (org-update-statistics-cookies t)))
(add-hook 'org-after-todo-state-change-hook 'org-subtask-reset)
(provide 'org-subtask-reset)
(setq org-log-done 'time)
;; (setq org-log-done 'note)
(setq org-closed-keep-when-no-todo t)
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :config
  (org-roam-db-autosync-enable)
  :custom
  (org-roam-directory (file-truename "~/org-roam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))
(setq find-file-visit-truename t)
;; for org-roam-buffer-toggle
;; Use side-window like V1
;; This can take advantage of slots available with it
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.25)
               (preserve-size . (t . nil))
               (window-parameters . ((no-other-window . t)
                                     (no-delete-other-windows . t)))))
;; org-roam templates
(setq org-roam-capture-templates
      '(
        ("d" "default" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n#+roam_alias:\n#+roam_key:\n#+roam_tags:\n\n")
         :unnarrowed t)
        )
      )
(setq org-roam-capture-ref-templates
      '(
        ("a" "Annotation" plain
         "%U ${body}\n"
         :target (file+head "${slug}.org"
                            "#+title: ${title}\n#+roam_key: ${ref}\n#+roam_alias:\n#+roam_tags:\n\n")
         :immediate-finish t
         :unnarrowed t
         )
        ("r" "ref" plain ""
         :target (file+head "${slug}.org"
                            "#+title: ${title}\n#+roam_key: ${ref}\n#+roam_alias:\n#+roam_tags:\n\n")
         :immediate-finish t
         :unnarrowed t)
        )
      )


(provide 'init-02-org)
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-02-org.el ends here
