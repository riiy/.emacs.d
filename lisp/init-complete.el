;;; init-complete.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; 自动补全

;;; Code:
(use-package
  company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 2) ; 只需敲 2 个字母就开始进行自动补全
  (setq company-tooltip-align-annotations t) (setq company-idle-delay 0.0)
  (setq company-show-quick-access t) ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence))) ; 根据选择的频率进行排序，读者如果不喜欢可以去掉

(use-package
  yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  (yas-global-mode 1))
(use-package yasnippet-snippets :ensure t :after yasnippet)
;; Enable vertico
(use-package
  vertico
  :ensure t
  :init (vertico-mode)
  :config (setq vertico-preselect 'directory vertico-resize nil vertico-count 17 vertico-cycle t)
  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (define-key vertico-map (kbd "C-n") 'vertico-next)
  (define-key vertico-map (kbd "C-'") 'vertico-quick-jump)
  (define-key vertico-map (kbd "C-p") 'vertico-previous)
  (define-key vertico-map [backspace] #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "s-SPC") #'+vertico/embark-preview))
;; configuration for Consult
(use-package
  consult
  :ensure t
  :bind
  ( ;; C-c bindings in `mode-specific-map'
    ("C-c M-x" . consult-mode-command)
    ("C-c h" . consult-history)
    ("C-c k" . consult-kmacro)
    ("C-c m" . consult-man)
    ("C-c i" . consult-info)
    ([remap Info-search] . consult-info)
    ;; C-x bindings in `ctl-x-map'
    ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
    ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
    ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
    ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
    ("C-x t b" . consult-buffer-other-tab) ;; orig. switch-to-buffer-other-tab
    ("C-x r b" . consult-bookmark) ;; orig. bookmark-jump
    ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
    ;; Custom M-# bindings for fast register access
    ("M-#" . consult-register-load)
    ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
    ("C-M-#" . consult-register)
    ;; Other custom bindings
    ("M-y" . consult-yank-pop) ;; orig. yank-pop
    ;; M-g bindings in `goto-map'
    ("M-g e" . consult-compile-error)
    ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
    ("M-g g" . consult-goto-line) ;; orig. goto-line
    ("M-g M-g" . consult-goto-line) ;; orig. goto-line
    ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
    ("M-g m" . consult-mark)
    ("M-g k" . consult-global-mark)
    ("M-g i" . consult-imenu)
    ("M-g I" . consult-imenu-multi)
    ;; M-s bindings in `search-map'
    ("M-s d" . consult-find) ;; Alternative: consult-fd
    ("M-s c" . consult-locate)
    ("M-s g" . consult-grep)
    ("M-s G" . consult-git-grep)
    ("M-s f" . consult-ripgrep)
    ("M-s r" . consult-recent-file)
    ("M-s l" . consult-line)
    ("M-s L" . consult-line-multi)
    ("M-s k" . consult-keep-lines)
    ("M-s u" . consult-focus-lines)
    ;; Isearch integration
    ("M-s e" . consult-isearch-history)
    :map
    isearch-mode-map
    ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
    ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
    ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
    ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
    ;; Minibuffer history
    :map
    minibuffer-local-map
    ("M-s" . consult-history) ;; orig. next-matching-history-element
    ("M-r" . consult-history)) ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5 register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section, after lazily loading the package.
  :config
  (consult-customize
    consult-theme
    :preview-key
    '(:debounce 0.2 any)
    consult-ripgrep
    consult-git-grep
    consult-grep
    consult-bookmark
    consult-recent-file
    consult-xref
    consult--source-bookmark
    consult--source-file-register
    consult--source-recent-file
    consult--source-project-recent-file
    ;; :preview-key "M-."
    :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"
  (setq consult-async-min-input 2) ;; 只需敲 2 个字母就开始进行搜索
  )

(use-package
  embark
  :ensure t
  :bind
  (("C-." . embark-act) ;; pick some comfortable binding
    ("C-;" . embark-dwim) ;; good alternative: M-.
    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list
    'display-buffer-alist
    '
    ("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
      nil
      (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package
  embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook (embark-collect-mode . consult-preview-at-point-mode))
(use-package
  consult-dir
  :ensure t
  :bind (([remap list-directory] . consult-dir) :map vertico-map ("s-d" . consult-dir)))

;; Optionally use the `orderless' completion style.
(use-package
  orderless
  :ensure t
  :config
  (defvar
    +orderless-dispatch-alist
    '
    ((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))

  (defun
    +orderless-dispatch (pattern index _total)
    (cond
      ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
      ((string-suffix-p "$" pattern)
        `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
      ;; File extensions
      (
        (and
          ;; Completing filename or eshell
          (or minibuffer-completing-file-name (derived-mode-p 'eshell-mode))
          ;; File extension
          (string-match-p "\\`\\.." pattern))
        `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
      ;; Ignore single !
      ((string= "!" pattern)
        `(orderless-literal . ""))
      ;; Prefix and suffix
      (
        (if-let
          (
            x
            (assq (aref pattern 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 1))
          (when-let
            (
              x
              (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist))
            (cons (cdr x) (substring pattern 0 -1)))))))
  ;; Configure a custom style dispatcher (see the Consult wiki)
  (setq
    orderless-style-dispatchers
    '(+orderless-dispatch)
    orderless-component-separator
    #'orderless-escapable-split-on-space)
  (setq
    completion-styles
    '(orderless basic)
    completion-category-defaults
    nil
    completion-category-overrides
    '((file (styles partial-completion)))))

(provide 'init-complete)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-complete.el ends here
