;;; init-ui.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; doom-themes

;;; Code:

;; themes
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory)) ; 设定主题加载路径
(load-theme 'dracula t)
;; 用不同颜色标记多级括号
(use-package rainbow-delimiters :ensure t :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-ui)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-ui.el ends here
