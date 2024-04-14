;;; init-dev.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package
  eglot
  :ensure t
  :hook (((python-mode) . eglot-ensure) ((cider-mode eglot-managed-mode) . eglot-disable-in-cider))
  :preface
  (defun
    eglot-disable-in-cider ()
    (when
      (eglot-managed-p)
      (if
        (bound-and-true-p cider-mode)
        (progn
          (remove-hook 'completion-at-point-functions 'eglot-completion-at-point t)
          (remove-hook 'xref-backend-functions 'eglot-xref-backend t))
        (add-hook 'completion-at-point-functions 'eglot-completion-at-point nil t)
        (add-hook 'xref-backend-functions 'eglot-xref-backend nil t))))
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
  :config (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio"))))

(provide 'init-dev)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-dev.el ends here
