;;; init-10-org.el --- Load the full configuration -*- lexical-binding: t -*-
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
(provide 'init-10-org)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-10-org.el ends here
