;;; init-04-evil.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; 设置内容比较少的插件

;;; Code:
;;C-z 切换 Emacs 按键模式和 Vim 按键模式
(use-package
  evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config (evil-mode 1) (evil-define-key 'normal 'global (kbd "gss") 'evil-avy-goto-char-2))

(use-package
  evil-collection
  :after evil
  :ensure t
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

(use-package
  evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-define-key
    'normal
    org-mode-map
    (kbd "TAB")
    'org-cycle
    ">"
    'org-shiftmetaright
    "<"
    'org-shiftmetaleft)
  (evil-org-agenda-set-keys))

(provide 'init-04-evil)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-04-evil.el ends here
