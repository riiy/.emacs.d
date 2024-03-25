;;; init-03-doom-themes.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; doom-themes

;;; Code:
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-monokai-octagon t)
  )

(provide 'init-03-doom-themes)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
