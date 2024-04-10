;;; init-treemacs.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; treemacs
(use-package
  treemacs
  :ensure t
  :defer t
  :init (with-eval-after-load 'winum (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq
      treemacs-collapse-dirs
      (if treemacs-python-executable 3 0)
      treemacs-deferred-git-apply-delay
      0.5
      treemacs-directory-name-transformer
      #'identity
      treemacs-display-in-side-window
      t
      treemacs-eldoc-display
      'simple
      treemacs-file-event-delay
      2000
      treemacs-file-extension-regex
      treemacs-last-period-regex-value
      treemacs-file-follow-delay
      0.2
      treemacs-file-name-transformer
      #'identity
      treemacs-follow-after-init
      t
      treemacs-expand-after-init
      t
      treemacs-find-workspace-method
      'find-for-file-or-pick-first
      treemacs-git-command-pipe
      ""
      treemacs-goto-tag-strategy
      'refetch-index
      treemacs-header-scroll-indicators
      '(nil . "^^^^^^")
      treemacs-hide-dot-git-directory
      t
      treemacs-indentation
      2
      treemacs-indentation-string
      " "
      treemacs-is-never-other-window
      nil
      treemacs-max-git-entries
      5000
      treemacs-missing-project-action
      'ask
      treemacs-move-forward-on-expand
      nil
      treemacs-no-png-images
      t
      treemacs-no-delete-other-windows
      t
      treemacs-project-follow-cleanup
      nil
      treemacs-persist-file
      (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
      treemacs-position
      'left
      treemacs-read-string-input
      'from-child-frame
      treemacs-recenter-distance
      0.1
      treemacs-recenter-after-file-follow
      nil
      treemacs-recenter-after-tag-follow
      nil
      treemacs-recenter-after-project-jump
      'always
      treemacs-recenter-after-project-expand
      'on-distance
      treemacs-litter-directories
      '("/node_modules" "/.venv" "/.cask")
      treemacs-project-follow-into-home
      nil
      treemacs-show-cursor
      nil
      treemacs-show-hidden-files
      t
      treemacs-silent-filewatch
      nil
      treemacs-silent-refresh
      nil
      treemacs-sorting
      'alphabetic-asc
      treemacs-select-when-already-in-treemacs
      'move-back
      treemacs-space-between-root-nodes
      t
      treemacs-tag-follow-cleanup
      t
      treemacs-tag-follow-delay
      1.5
      treemacs-text-scale
      nil
      treemacs-user-mode-line-format
      nil
      treemacs-user-header-line-format
      nil
      treemacs-wide-toggle-width
      70
      treemacs-width
      35
      treemacs-width-increment
      1
      treemacs-width-is-initially-locked
      t
      treemacs-workspace-switch-cleanup
      nil)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable (treemacs-git-commit-diff-mode t))

    (pcase
      (cons (not (null (executable-find "git"))) (not (null treemacs-python-executable)))
      (`(t . t) (treemacs-git-mode 'deferred))
      (`(t . _) (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map
    global-map
    ("M-0" . treemacs-select-window)
    ("C-c t 1" . treemacs-delete-other-windows)
    ("C-c t t" . treemacs)
    ("C-c t d" . treemacs-select-directory)
    ("C-c t B" . treemacs-bookmark)
    ("C-c t C-t" . treemacs-find-file)
    ("C-c t M-t" . treemacs-find-tag)))
(use-package treemacs-evil :ensure t :after (treemacs evil) :ensure t)
(use-package treemacs-projectile :ensure t :after (treemacs projectile) :ensure t)
(use-package treemacs-magit :ensure t :after (treemacs magit) :ensure t)
(use-package
  treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :ensure t
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))
(use-package treemacs-nerd-icons :ensure t :config (treemacs-load-theme "nerd-icons"))

(provide 'init-tremacs)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-treemacs.el ends here
