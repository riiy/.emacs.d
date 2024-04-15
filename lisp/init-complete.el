;;; init-complete.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; 设置内容比较少的插件

;;; Code:

(use-package
  yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  (yas-global-mode 1)
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
  company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1) ; 只需敲 1 个字母就开始进行自动补全
  (setq company-tooltip-align-annotations t) (setq company-idle-delay 0.0)
  (setq company-show-quick-access t) ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence))) ; 根据选择的频率进行排序，读者如果不喜欢可以去掉
;; counsel
(use-package
  counsel
  :ensure t
  :bind
  (("M-x" . 'counsel-M-x) ; 使用 counsel 替换命令输入，给予更多提示
    ("C-x C-f" . 'counsel-find-file) ; 使用 counsel 做文件打开操作，给予更多提示
    ("M-y" . 'counsel-yank-pop) ; 使用 counsel 做历史剪贴板粘贴，可以展示历史
    :map minibuffer-local-map ("C-r" . counsel-minibuffer-history)))

;; ivy
(use-package
  ivy
  :ensure t ; 确认安装，如果没有安装过 ivy 就自动安装
  :init ; 在加载插件前执行命令
  (ivy-mode 1) ; 启动 ivy-mode
  (counsel-mode 1)
  :custom ; 自定义一些变量，相当于赋值语句 (setq xxx yyy)
  (ivy-use-virtual-buffers t) ; 一些官网提供的固定配置
  (search-default-mode #'char-fold-to-regexp) (ivy-count-format "(%d/%d) ")
  :bind ; 以下为绑定快捷键
  (("C-s" . 'swiper-isearch) ; 绑定快捷键 C-s 为 swiper-search，替换原本的搜索功能
    ("C-x b" . 'ivy-switch-buffer) ; 使用 ivy 做 buffer 切换，给予更多提示
    ("C-c o" . 'ivy-occur)
    ("C-c v" . 'ivy-push-view) ; 记录当前 buffer 的信息
    ("C-c s" . 'ivy-switch-view) ; 切换到记录过的 buffer 位置
    ("C-c V" . 'ivy-pop-view) ; 移除 buffer 记录
    ("C-x C-SPC" . 'counsel-mark-ring) ; 使用 counsel 记录 mark 的位置
    ("<f1> f" . 'counsel-describe-function)
    ("<f1> v" . 'counsel-describe-variable)
    ("<f1> i" . 'counsel-info-lookup-symbol)))
(provide 'init-utils)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-complete.el ends here
