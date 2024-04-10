;;; init-dev.el --- Load the full configuration -*- lexical-binding: t -*-
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
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map ("s-p" . projectile-command-map) ("C-c p" . projectile-command-map)))
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

(use-package restclient :ensure t :mode (("\\.http\\'" . restclient-mode)))

(use-package
  beancount
  :load-path "site-lisp/package/beancount-mode" ;; git@github.com:beancount/beancount-mode.git
  :init
  (add-hook 'beancount-mode-hook (lambda () (setq-local electric-indent-chars nil)))
  (add-hook 'beancount-mode-hook #'flymake-bean-check-enable)
  :mode (("\\.beancount\\'" . beancount-mode))
  :config
  (define-key beancount-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
  (define-key beancount-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading))

(provide 'init-dev)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-dev.el ends here
