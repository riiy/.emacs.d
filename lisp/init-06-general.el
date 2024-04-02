;;; init-06-general.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;; 定制快捷键
;;; Code:
(use-package
  general
  :ensure t ; 确认安装，如果没有安装过, 就自动安装
  :config
  (general-create-definer
    rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "t"
    '(:ignore t :which-key "toggles")
    "tt"
    '(counsel-load-theme :which-key "choose theme"))
  (rune/leader-keys "`" '(evil-switch-to-windows-last-buffer :which-key "Last buffer"))
  (rune/leader-keys "b" '(:ignore b :which-key "Buffer") "bi" '(ibuffer :which-key "ibuffer"))
  (rune/leader-keys
    "g"
    '(:ignore g :which-key "Magit")
    "gg"
    '(magit-status :which-key "status")
    "gB"
    '(magit-blame-addition :which-key "Blame addition"))
  (rune/leader-keys
    "o"
    '(:ignore o :which-key "Open")
    "oa"
    '(org-agenda :which-key "org agenda")
    "ot"
    '(org-todo-list :which-key "org todo"))
  (rune/leader-keys "f" '(:ignore f :which-key "File") "fs" '(save-buffer :which-key "Save File")))

(provide 'init-06-general)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-06-general.el ends here
