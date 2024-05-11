;;; init-evil.el --- Load the full configuration -*- lexical-binding: t -*-
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
  :config (evil-mode 1) (evil-define-key 'normal 'global (kbd "gs") 'evil-avy-goto-char-2))

(use-package
  evil-collection
  :after evil
  :ensure t
  :custom (evil-collection-setup-minibuffer t)
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

;; commentary
(use-package evil-nerd-commenter :ensure t :config (evilnc-default-hotkeys))
(provide 'init-evil)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-evil.el ends here
