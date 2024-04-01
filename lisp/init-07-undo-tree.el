;;; init-07-undo-tree.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; undo-tree

;;; Code:
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

(provide 'init-07-undo-tree)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-07-undo-tree.el ends here
