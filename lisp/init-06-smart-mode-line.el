;;; init-06-smart-mode-line.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; smart-mode-line

;;; Code:
(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)  ; avoid asking when startup
  (sml/setup)
  :config
  (setq rm-blacklist
        (format "^ \\(%s\\)$"
                (mapconcat #'identity
                           '("Projectile.*" "company.*" "Google"
                             "Undo-Tree" "counsel" "ivy" "yas" "WK")
                           "\\|")))
  )

(provide 'init-06-smart-mode-line)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
