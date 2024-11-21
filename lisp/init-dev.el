;;; init-dev.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; tree siter

(setq
  treesit-language-source-alist
  '
  ((bash "https://github.com/tree-sitter/tree-sitter-bash")
    (cmake "https://github.com/uyha/tree-sitter-cmake")
    (css "https://github.com/tree-sitter/tree-sitter-css")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (make "https://github.com/alemuller/tree-sitter-make")
    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (toml "https://github.com/tree-sitter/tree-sitter-toml")
    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

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
(use-package
  go-mode
  :ensure t
  :commands go-mode
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save))
;; eglot
(use-package
  eglot
  :ensure t
  :defer t
  :bind (:map eglot-mode-map ("C-c M-n" . eglot-rename))
  :hook
  ((python-ts-mode . eglot-ensure)
    (python-ts-mode . hs-minor-mode)
    (python-ts-mode . (lambda () (set-fill-column 240)))
    (go-mode . eglot-ensure)
    (c-mode . eglot-ensure)
    (c++-mode . eglot-ensure))
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
  ;; Golang specific
  (add-to-list ' eglot-server-programs '(go-mode . ("gopls")))
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
    (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)))

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
;; web
(use-package
  web-mode
  :ensure t
  :defer t
  :mode
  (("\\.ios\\.js$" . web-mode)
    ("\\.android\\.js$" . web-mode)
    ("\\.react\\.js$" . web-mode)
    ("\\.js$" . web-mode))
  :config
  (add-to-list 'magic-mode-alist '("^import React" . web-mode))
  (add-to-list 'magic-mode-alist '("React.Component" . web-mode))
  (add-to-list 'magic-mode-alist '("from 'react';$" . web-mode))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))

  (with-eval-after-load 'flymake (flymake-add-mode 'javascript-eslint 'web-mode))

  (add-hook
    'web-mode-hook
    (lambda () (if (equal web-mode-content-type "javascript") (web-mode-set-content-type "jsx"))))
  (setq-local web-mode-enable-auto-quoting nil)

  (setq-default js-indent-level 4)

  (add-hook
    'web-mode-hook
    (lambda
      ()
      (setq web-mode-markup-indent-offset (symbol-value 'js-indent-level))
      (setq web-mode-attr-indent-offset (symbol-value 'js-indent-level))
      (setq web-mode-css-indent-offset (symbol-value 'js-indent-level))
      (setq web-mode-code-indent-offset (symbol-value 'js-indent-level)))))
(defun
  setup-tide-mode
  ()
  (interactive)
  (tide-setup)
  (flymake-mode +1)
  (setq flymake-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1))
;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
;; if you use treesitter based typescript-ts-mode (emacs 29+)
(add-hook 'typescript-ts-mode-hook #'setup-tide-mode)
(add-hook 'tsx-ts-mode-hook #'setup-tide-mode)
(use-package
  tide
  :ensure t
  :mode ((".tsx$" . tsx-ts-mode)(".ts$" . typescript-ts-mode))
  :hook
  ((typescript-ts-mode . tide-setup)
    (tsx-ts-mode . tide-setup)
    (typescript-ts-mode . tide-hl-identifier-mode)
    (before-save . tide-format-before-save)))
(provide 'init-dev)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-dev.el ends here
