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
  :config (yas-reload-all) (yas-global-mode 1)
  ;; ;; add company-yasnippet to company-backends
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
  projectile
  :ensure t
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map ("s-p" . projectile-command-map) ("C-c p" . projectile-command-map)))
(use-package counsel-projectile :ensure t :after (projectile) :init (counsel-projectile-mode))
(use-package magit :ensure t)
(use-package diff-hl :ensure t :config (global-diff-hl-mode) (diff-hl-margin-mode))
(use-package rg :ensure t)

;; commentary
(use-package evil-nerd-commenter :ensure t :config (evilnc-default-hotkeys))

(use-package restclient :ensure t :mode (("\\.http\\'" . restclient-mode)))

(provide 'init-dev)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-dev.el ends here
