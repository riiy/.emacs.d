;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)) ; 设定源码加载路径

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer

;; Adjust garbage collection thresholds during startup, and thereafter
(let
  (
    (normal-gc-cons-threshold (* 20 1024 1024))
    (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

;; Bootstrap config
(setq custom-file (locate-user-emacs-file "custom.el"))

;;
(require 'init-basic)
(require 'init-ui)
(require 'init-utils)
(require 'init-evil)
(require 'init-general)
(require 'init-dev)
(require 'init-org)
(provide 'init)

;;; init.el ends here
