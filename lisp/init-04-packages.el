;;; init-04-packages.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; 设置内容比较少的插件

;;; Code:
;;C-z 切换 Emacs 按键模式和 Vim 按键模式
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))
;; amx 记录我们每次调用 M-x 时输入的命令历史，然后每次将最常用的显示在前面
(use-package amx
  :ensure t
  :init (amx-mode))
;; ace-window 对 C-x o 重新绑定，使用时可以为每个 window 编个号，用编号进行跳转
(use-package ace-window
  :ensure t
  :bind (("C-x o" . 'ace-window)))

;; avy 快速光标跳转
(use-package avy
  :ensure t
  :bind
  (("C-j" . avy-goto-char-timer)))
;; marginalia 为 Emacs minibuffer 中的选项添加注解
(use-package marginalia
  :ensure t
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))
;; hydra 主要功能是把一组特定场景的命令组织到一起， 通过简单按键来进行调用
(use-package hydra
  :ensure t)

(use-package use-package-hydra
  :ensure t
  :after hydra)

;; 这个插件可以高亮出当前 Buffer 中所有的、与光标所在处的符号相同的符号。
(use-package highlight-symbol
  :ensure t
  :init (highlight-symbol-mode)
  :bind ("<f3>" . highlight-symbol)) ;; 按下 F3 键就可高亮当前符号
;; 用不同颜色标记多级括号
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; counsel
(use-package counsel
  :ensure t)
(use-package which-key
  :ensure t
  :init (which-key-mode))
;;
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs!") ;; 个性签名，随读者喜好设置
  ;; (setq dashboard-projects-backend 'projectile) ;; 读者可以暂时注释掉这一行，等安装了 projectile 后再使用
  (setq dashboard-startup-banner 'official) ;; 也可以自定义图片
  (setq dashboard-items '((recents  . 5)   ;; 显示多少个最近文件
			  (bookmarks . 5)  ;; 显示多少个最近书签
			  (projects . 10))) ;; 显示多少个最近项目
  (dashboard-setup-startup-hook))
(provide 'init-04-packages)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
