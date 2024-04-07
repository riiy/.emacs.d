;;; init-ui.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; doom-themes

;;; Code:

(use-package nerd-icons :ensure t :custom (nerd-icons-font-family "Symbols Nerd Font Mono"))

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
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; 用不同颜色标记多级括号
(use-package rainbow-delimiters :ensure t :hook (prog-mode . rainbow-delimiters-mode))
;; (use-package nerd-icons-dired :ensure t :hook (dired-mode . nerd-icons-dired-mode))

(provide 'init-ui)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-ui.el ends here
