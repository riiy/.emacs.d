;;; init-01-basic.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; 基本配置

;;; Code:
(menu-bar-mode -1)
;(setq confirm-kill-emacs #'yes-or-no-p)      ; 在关闭 Emacs 前询问是否确认关闭，防止误触
(electric-pair-mode t) ; 自动补全括号
(add-hook 'prog-mode-hook #'show-paren-mode) ; 编程模式下，光标在括号上时高亮另一个括号
(column-number-mode t) ; 在 Mode line 上显示列号
(global-auto-revert-mode t) ; 当另一程序修改了文件时，让 Emacs 及时刷新 Buffer
(delete-selection-mode t) ; 选中文本后输入文本会替换文本（更符合我们习惯了的其它编辑器的逻辑）
(setq inhibit-startup-message t) ; 关闭启动 Emacs 时的欢迎界面
(setq make-backup-files nil) ; 关闭文件自动备份
(add-hook 'prog-mode-hook #'hs-minor-mode) ; 编程模式下，可以折叠代码块
(global-display-line-numbers-mode 1) ; 在 Window 显示行号
(tool-bar-mode -1) ; （熟练后可选）关闭 Tool bar
; (when (display-graphic-p) (toggle-scroll-bar -1)) ; 图形界面时关闭滚动条
; (savehist-mode 1) ; （可选）打开 Buffer 历史记录保存
(save-place-mode 1)
(setq display-line-numbers-type 'relative) ; （可选）显示相对行号
; (global-set-key (kbd "C-j") nil)
;; 国内镜像
(require 'package)
(setq
  package-archives
  '
  (("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
    ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
    ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)
;;; packages
(provide 'init-basic)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-basic.el ends here