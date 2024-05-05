;;; init-dev.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; python
(setq-default truncate-lines t)
(add-hook 'python-mode-hook (lambda () (setq truncate-lines t)))
(setq tab-width 4)
(set-variable 'python-indent-offset 4)
(set-variable 'python-indent-guess-indent-offset nil)
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
;; eglot
(use-package
  eglot
  :ensure t
  :defer t
  :bind (:map eglot-mode-map ("C-c M-n" . eglot-rename))
  :hook
  ((python-ts-mode . eglot-ensure)
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
  (setq lsp-pyright-use-library-code-for-types t)
  (setq lsp-pyright-stub-path (concat (getenv "HOME") "/wokspc/python-type-stubs"))
  (setq-default eglot-workspace-configuration '((:pyright . ((useLibraryCodeForTypes . t))))))

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
    (gptel-make-ollama "Ollama" :host "localhost:11434" :stream t :models '("mistral:latest"))))
(provide 'init-dev)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-dev.el ends here
