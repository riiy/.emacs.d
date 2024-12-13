;;; init-utils.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; 设置内容比较少的插件

;;; Code:
;; amx 记录我们每次调用 M-x 时输入的命令历史，然后每次将最常用的显示在前面
(use-package amx :ensure t :init (amx-mode))
;; avy 快速光标跳转
(use-package avy :ensure t)
;; marginalia 为 Emacs minibuffer 中的选项添加注解
(use-package
  marginalia
  :ensure t
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map ("M-c" . marginalia-cycle)))
;; 这个插件可以高亮出当前 Buffer 中所有的、与光标所在处的符号相同的符号。
(use-package
  highlight-symbol
  :ensure t
  :init
  (add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode 1)))
  (add-hook 'prog-mode-hook (lambda () (highlight-symbol-nav-mode 1)))
  :config (setq highlight-symbol-idle-delay 1.0 highlight-symbol-on-navigation-p t)
  :diminish highlight-symbol-mode)
;; which-key
(use-package
  which-key
  :ensure t
  :init
  (which-key-mode)
  (which-key-add-key-based-replacements "C-c &" "yasniper")
  (which-key-add-key-based-replacements "C-c @" "hs-code-block")
  (which-key-add-key-based-replacements "C-c n" "org-roam")
  (which-key-add-key-based-replacements "C-x a" "abbrev")
  (which-key-add-key-based-replacements "C-x 8" "emoji")
  (which-key-add-key-based-replacements "C-x n" "narrow")
  (which-key-add-key-based-replacements "C-x r" "register")
  (which-key-add-key-based-replacements "C-x t" "TAB")
  (which-key-add-key-based-replacements "C-x w" "window")
  (which-key-add-key-based-replacements "C-x x" "buffer+utils")
  (which-key-add-key-based-replacements "C-x RET" "SETING")
  (which-key-add-major-mode-key-based-replacements 'python-ts-mode "C-c C-t" "skeleton")
  (which-key-add-major-mode-key-based-replacements 'python-ts-mode "C-c TAB" "imports")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode "C-c C-a" "insert-link")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode "C-c C-s" "inserts")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode "C-c C-a" "insert-head")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode "C-c C-x" "toggle")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode "C-c C-c" "export")
  (which-key-add-major-mode-key-based-replacements 'org-mode "C-c \"" "plot")
  (which-key-add-major-mode-key-based-replacements 'org-mode "C-c C-v" "babel")
  (which-key-add-major-mode-key-based-replacements 'org-mode "C-c C-x" "utils"))
;; unto-tree
(use-package
  undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :bind ("C-u" . undo-tree-visualize)
  :config (setq undo-tree-history-directory-alist `(("." . "~/.emacs.d/.cache/"))))

(use-package
  recentf
  :config (progn (setq recentf-max-saved-items 200 recentf-max-menu-items 15) (recentf-mode)))
(use-package savehist :ensure t :init (savehist-mode))
;; version contrl
(use-package magit :ensure t)
(use-package diff-hl :ensure t :config (global-diff-hl-mode) (diff-hl-margin-mode))
(use-package rg :ensure t)
;; restclient
(use-package restclient :ensure t :mode (("\\.http\\'" . restclient-mode)))
(use-package
  projectile
  :ensure t
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map ("C-x p" . projectile-command-map)))
(provide 'init-utils)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-utils.el ends here
