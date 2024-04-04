;;; init-utils.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; 设置内容比较少的插件

;;; Code:
;; amx 记录我们每次调用 M-x 时输入的命令历史，然后每次将最常用的显示在前面
(use-package amx :ensure t :init (amx-mode))
;; ace-window 对 C-x o 重新绑定，使用时可以为每个 window 编个号，用编号进行跳转
(use-package ace-window :ensure t :bind (("C-x o" . 'ace-window)))

;; avy 快速光标跳转
(use-package avy :ensure t :bind (("C-;" . avy-goto-char-timer)))
;; marginalia 为 Emacs minibuffer 中的选项添加注解
(use-package
  marginalia
  :ensure t
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle)))
;; hydra 主要功能是把一组特定场景的命令组织到一起， 通过简单按键来进行调用
(use-package hydra :ensure t)

(use-package use-package-hydra :ensure t :after hydra)

;; 这个插件可以高亮出当前 Buffer 中所有的、与光标所在处的符号相同的符号。
(use-package
  highlight-symbol
  :ensure t
  :init (highlight-symbol-mode)
  :bind ("<f3>" . highlight-symbol)) ;; 按下 F3 键就可高亮当前符号

;; counsel
(use-package
  counsel
  :ensure t
  :bind
  (("M-x" . 'counsel-M-x) ; 使用 counsel 替换命令输入，给予更多提示
    ("C-x C-f" . 'counsel-find-file) ; 使用 counsel 做文件打开操作，给予更多提示
    ("M-y" . 'counsel-yank-pop) ; 使用 counsel 做历史剪贴板粘贴，可以展示历史
    :map minibuffer-local-map ("C-r" . counsel-minibuffer-history)))
(use-package which-key :ensure t :init (which-key-mode))

;; 输入法pyim-wbdict
(use-package popup :ensure t)
(use-package
  pyim-wbdict
  :ensure t
  :init
  (require 'popup)
  (setq pyim-default-scheme 'wubi)
  (setq default-input-method "pyim")
  :config (pyim-wbdict-v86-enable))

(use-package
  undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :after hydra
  :bind ("C-u" . hydra-undo-tree/body)
  :hydra
  (hydra-undo-tree
    (:hint nil)
    "
  _p_: undo  _n_: redo _s_: save _l_: load   "
    ("p" undo-tree-undo)
    ("n" undo-tree-redo)
    ("s" undo-tree-save-history)
    ("l" undo-tree-load-history)
    ("u" undo-tree-visualize "visualize" :color blue)
    ("q" nil "quit" :color blue)))
;; Enable vertico
(use-package
  vertico
  :ensure t
  :init (vertico-mode)

  ;; Different scroll margin
  (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist :ensure t :init (savehist-mode))
;; A few more useful configurations...
(use-package
  emacs
  :ensure t
  :init
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (setq text-mode-ispell-word-completion nil)
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun
    crm-indicator (args)
    (cons
      (format
        "[CRM%s] %s"
        (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator)
        (car args))
      (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p))
;; Optionally use the `orderless' completion style.
(use-package
  orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq
    completion-styles
    '(orderless basic)
    completion-category-defaults
    nil
    completion-category-overrides
    '((file (styles partial-completion)))))
;; Use Dabbrev with Corfu!
(use-package
  dabbrev
  :ensure t
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion) ("C-M-/" . dabbrev-expand))
  :config (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))
;; Consult users will also want the embark-consult package.
(use-package embark-consult :ensure t :defer t)
(use-package
  embark
  :ensure t
  :bind
  (:map
    minibuffer-local-map
    ("M-o" . embark-act)
    ("C-c C-c" . embark-export)
    ("C-c C-o" . embark-collect)))

(use-package
  consult
  :ensure t
  :bind
  (([remap imenu] . consult-imenu)
    ([remap goto-line] . consult-goto-line)
    ([remap bookmark-jump] . consult-bookmark)
    ([remap recentf-open-files] . consult-recent-file)
    ([remap repeat-complex-command] . consult-complex-command)
    ([remap jump-to-register] . consult-register-load)
    ([remap point-to-register] . consult-register-store))
  :config
  (with-no-warnings
    (consult-customize
      consult-ripgrep
      consult-git-grep
      consult-grep
      consult-bookmark
      consult-recent-file
      consult-buffer
      :preview-key nil))

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5 register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  :custom
  (consult-fontify-preserve nil)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1))

(provide 'init-utils)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-utils.el ends here
