;;; init-09-develop.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package
  company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1) ; 只需敲 1 个字母就开始进行自动补全
  (setq company-tooltip-align-annotations t) (setq company-idle-delay 0.0)
  (setq company-show-quick-access t) ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence))) ; 根据选择的频率进行排序，读者如果不喜欢可以去掉

(use-package
  yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all)
  ;; add company-yasnippet to company-backends
  (defun
    company-mode/backend-with-yas (backend)
    (if
      (and (listp backend) (member 'company-yasnippet backend))
      backend
      (append (if (consp backend) backend (list backend)) '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  ;; unbind <TAB> completion
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  :bind (:map yas-minor-mode-map ("S-<tab>" . yas-expand)))

(use-package yasnippet-snippets :ensure t :after yasnippet)
(use-package
  flycheck
  :ensure t
  :config
  (setq truncate-lines nil) ; 如果单行信息很长会自动换行
  :hook (prog-mode . flycheck-mode))

(use-package
  lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c b" lsp-file-watch-threshold 500)
  :hook
  (lsp-mode . lsp-enable-which-key-integration) ; which-key integration
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-completion-provider :none) ;; 阻止 lsp 重新设置 company-backend 而覆盖我们 yasnippet 的设置
  (setq lsp-headerline-breadcrumb-enable t)
  :bind
  ("C-c b s" . lsp-ivy-workspace-symbol)) ;; 可快速搜索工作区内的符号（类名、函数名、变量名等）
(use-package
  lsp-ui
  :ensure t
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-position 'top))
(use-package lsp-ivy :ensure t)

(use-package
  projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c M-p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically nil))

(use-package counsel-projectile :ensure t :after (projectile) :init (counsel-projectile-mode))
(use-package magit :ensure t)
(use-package diff-hl :ensure t :config (global-diff-hl-mode) (diff-hl-margin-mode))
(use-package rg :ensure t)

(use-package
  c++-mode
  :functions ; suppress warnings
  c-toggle-hungry-state
  :hook (c-mode . lsp-deferred) (c++-mode . lsp-deferred) (c++-mode . c-toggle-hungry-state))
(use-package
  python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode))

(use-package
  pyvenv
  :ensure t
  :config
  ;; (setenv "WORKON_HOME" (expand-file-name "~/miniconda3/envs"))
  ;; (setq python-shell-interpreter "python3")  ; （可选）更改解释器名字
  (pyvenv-mode t)
  ;; （可选）如果希望启动后激活 miniconda 的 base 环境，就使用如下的 hook
  ;; :hook
  ;; (python-mode . (lambda () (pyvenv-workon "..")))
  )
(use-package
  lsp-pyright
  :ensure t
  :config
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred))))
(use-package
  ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls) (lsp))))
;; commentary
(use-package evil-nerd-commenter :ensure t :config (evilnc-default-hotkeys))

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

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

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

(use-package
  treemacs-icons-dired
  :ensure t
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit :ensure t :after (treemacs magit) :ensure t)

(use-package
  treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :ensure t
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package
  treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :ensure t
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(use-package treemacs-nerd-icons :ensure t :config (treemacs-load-theme "nerd-icons"))

(use-package restclient :ensure t :mode (("\\.http\\'" . restclient-mode)))

(provide 'init-dev)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-dev.el ends here
