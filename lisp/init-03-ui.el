;;; init-03-ui.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; doom-themes

;;; Code:
;; M-x all-the-icons-install-fonts

(use-package all-the-icons)

(use-package doom-modeline :ensure t :init (doom-modeline-mode 1))

(use-package
  doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq
    doom-themes-enable-bold t ; if nil, bold is universally disabled
    doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;(doom-themes-neotree-config)
  ;; or for treemacs users
  ;(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;(doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
;; 用不同颜色标记多级括号
(use-package rainbow-delimiters :ensure t :hook (prog-mode . rainbow-delimiters-mode))

(use-package
  dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs!") ;; 个性签名，随读者喜好设置
  (setq dashboard-projects-backend 'projectile) ;; 读者可以暂时注释掉这一行，等安装了 projectile 后再使用
  (setq dashboard-startup-banner 'official) ;; 也可以自定义图片
  (setq
    dashboard-items
    '
    ((recents . 5) ;; 显示多少个最近文件
      (bookmarks . 5) ;; 显示多少个最近书签
      (agenda . 5) (registers . 5)
      (projects . 10))) ;; 显示多少个最近项目
  (setq
    dashboard-item-shortcuts
    '((recents . "r") (bookmarks . "m") (projects . "p") (agenda . "a") (registers . "e")))
  (setq dashboard-icon-type 'all-the-icons)
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  (dashboard-setup-startup-hook))
(provide 'init-03-ui)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-03-ui.el ends here
