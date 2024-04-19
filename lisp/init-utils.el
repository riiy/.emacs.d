;;; init-utils.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; 设置内容比较少的插件

;;; Code:
;; amx 记录我们每次调用 M-x 时输入的命令历史，然后每次将最常用的显示在前面
(use-package amx :ensure t :init (amx-mode))
;; avy 快速光标跳转
(use-package avy :ensure t :bind (("C-;" . avy-goto-char-timer)))
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
  :init (highlight-symbol-mode)
  :bind ("<f3>" . highlight-symbol)) ;; 按下 F3 键就可高亮当前符号
;; which-key
(use-package which-key :ensure t :init (which-key-mode))
;; 输入法pyim-wbdict
(use-package popup :ensure t)
(use-package
  pyim-wbdict
  :ensure t
  :init
  (require 'popup)
  (setq pyim-default-scheme 'wubi)
  (setq default-input-method "pyim")
  :config (pyim-wbdict-v86-enable))
;; unto-tree
(use-package undo-tree :ensure t :init (global-undo-tree-mode) :bind ("C-u" . undo-tree-visualize))

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

(provide 'init-utils)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-utils.el ends here
