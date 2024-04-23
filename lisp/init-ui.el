;;; init-ui.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; doom-themes

;;; Code:

;; themes
(load-theme 'modus-vivendi t)
;; 用不同颜色标记多级括号
(use-package rainbow-delimiters :ensure t :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-ui)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-ui.el ends here
