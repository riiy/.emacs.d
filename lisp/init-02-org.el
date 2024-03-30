;;; init-02-org.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:


;;; Code:

;; org
(use-package org
  :init
  (require 'org-indent)
  (require 'org-habit)
  :config
  :bind
  (("C-c a" . 'org-agenda)
   :map org-mode-map
   ("C-c C-q" . counsel-org-tag)))
(provide 'init-02-org)
;; coding: utf-8
;; no-byte-compile: t
;;; init-02-org.el ends here
