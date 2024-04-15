;;; init-utils.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; 设置内容比较少的插件

;;; Code:
;; amx 记录我们每次调用 M-x 时输入的命令历史，然后每次将最常用的显示在前面
(use-package amx :ensure t :init (amx-mode))
;; ace-window 对 C-x o 重新绑定，使用时可以为每个 window 编个号，用编号进行跳转
(use-package ace-window :ensure t :bind (("C-x o" . 'ace-window)))
;; avy 快速光标跳转
(use-package avy :ensure t :bind (("C-;" . avy-goto-char-timer)))
;; marginalia 为 Emacs minibuffer 中的选项添加注解
(use-package
  marginalia
  :ensure t
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle)))
;; hydra 主要功能是把一组特定场景的命令组织到一起， 通过简单按键来进行调用
(use-package hydra :ensure t)
(use-package use-package-hydra :ensure t :after hydra)
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
(use-package
  undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :after hydra
  :bind ("C-u" . hydra-undo-tree/body)
  :hydra
  (hydra-undo-tree
    (:hint nil)
    "
  _p_: undo  _n_: redo _s_: save _l_: load   "
    ("p" undo-tree-undo)
    ("n" undo-tree-redo)
    ("s" undo-tree-save-history)
    ("l" undo-tree-load-history)
    ("u" undo-tree-visualize "visualize" :color blue)
    ("q" nil "quit" :color blue)))
(use-package
  recentf
  :config (progn (setq recentf-max-saved-items 200 recentf-max-menu-items 15) (recentf-mode)))

(use-package
  flycheck
  :ensure t
  :config
  (setq truncate-lines nil) ; 如果单行信息很长会自动换行
  :hook (prog-mode . flycheck-mode))

;; version conctrl
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
