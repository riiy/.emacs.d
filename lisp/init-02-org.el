;;; init-02-org.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:


;;; Code:

;; org
(use-package org
  :init
  (require 'org-indent)
  (require 'org-habit)
  :config
  (progn
    ;; The GTD part of this config is heavily inspired by
    ;; https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
    (setq org-directory "~/org") )
  :bind
  (("C-c a" . 'org-agenda)
   :map org-mode-map
   ("C-c C-q" . counsel-org-tag)))
(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
(provide 'init-02-org)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-02-org.el ends here
