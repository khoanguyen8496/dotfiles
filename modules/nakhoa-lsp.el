(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (go-mode . lsp-deferred)
	 (c++-mode . lsp-deferred)
	 (c-mode . lsp-deferred)
	 (rust-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deffered))
(use-package tree-sitter
    :init
    (global-tree-sitter-mode)
    :hook
    (tree-sitter-after-on-hook . tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(provide 'nakhoa-lsp)
