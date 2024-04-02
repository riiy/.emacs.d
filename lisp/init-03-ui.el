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
  smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t) ; avoid asking when startup
  (sml/setup)
  :config
  (setq
    rm-blacklist
    (format
      "^ \\(%s\\)$"
      (mapconcat
        #'identity
        '("Projectile.*" "company.*" "Google" "Undo-Tree" "counsel" "ivy" "yas" "WK")
        "\\|"))))

(provide 'init-03-ui)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-03-ui.el ends here
