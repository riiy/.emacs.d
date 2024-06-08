;;; init-dev.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; python

(use-package
  python
  :custom (python-indent-guess-indent-offset-verbose . nil)
  :hook (python-ts-mode-hook . eglot-ensure)
  :init
  (add-hook 'python-mode-hook (lambda () (setq truncate-lines t)))
  (setq tab-width 4)
  (set-variable 'python-indent-offset 4)
  (set-variable 'python-indent-guess-indent-offset nil)
  (set-variable
    'python-check-command
    "pycodestyle --max-line-length=240 --ignore=E121,E122,E123,E126,E226,E24,E704,E721,W503,W504")
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

;; golang
(when
  (display-graphic-p)
  (use-package
    go-mode
    :ensure t
    :commands go-mode
    :config
    (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save)))
;; eglot
(use-package
  eglot
  :ensure t
  :defer t
  :bind (:map eglot-mode-map ("C-c M-n" . eglot-rename))
  :hook
  ((python-ts-mode . eglot-ensure)
    (go-mode . eglot-ensure)
    (python-ts-mode . hs-minor-mode)
    (python-ts-mode . (lambda () (set-fill-column 240))))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref nil)
  (eglot-ignored-server-capabilities
    '
    (:hoverProvider
      :documentHighlightProvider
      :documentFormattingProvider
      :documentRangeFormattingProvider
      :documentOnTypeFormattingProvider
      :colorProvider
      :foldingRangeProvider))
  (eglot-stay-out-of '(yasnippet))
  :config
  ;; Python specific
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  (when
    (display-graphic-p)
    (defun
      my/eglot-capf ()
      (setq-local
        completion-at-point-functions
        (list
          (cape-capf-super #'eglot-completion-at-point)
          #'cape-keyword
          #'cape-history
          #'cape-dabbrev
          #'cape-file)))
    (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
    (add-to-list ' eglot-server-programs '(go-mode . ("gopls")))
    (add-hook ' go-mode-hook #'eglot-ensure)))

(when
  (display-graphic-p)
  (use-package
    gptel
    :straight '(gptel :host github :repo "karthink/gptel")
    :init
    (gptel-make-ollama
      "Ollama" ;Any name of your choosing
      :host "localhost:11434" ;Where it's running
      :stream t ;Stream responses
      :models '("mistral:latest")) ;List of models
    :config
    (setq
      gptel-model
      "mistral:latest"
      gptel-backend
      (gptel-make-ollama "Ollama" :host "localhost:11434" :stream t :models '("mistral:latest")))))
(use-package
  flymake
  :ensure t
  :bind
  (nil
    :map
    flymake-mode-map
    ("C-c C-p" . flymake-goto-prev-error)
    ("C-c C-n" . flymake-goto-next-error)))

(use-package
  flymake-diagnostic-at-point
  :ensure t
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(provide 'init-dev)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-dev.el ends here
