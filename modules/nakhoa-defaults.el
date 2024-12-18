
(global-auto-revert-mode)
(show-paren-mode 1)
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))
;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one. 
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "C-S-o") 'open-previous-line)
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")

(use-package smartparens
  :config
  (show-smartparens-global-mode t)
  (smartparens-global-mode t))

(require 'ansi-color)

(defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :bind
  (("C-/" . undo-tree-undo)
   ("C-?" . undo-tree-redo)))
(use-package magit)
(use-package which-key
  :ensure t
  :init
  (which-key-mode))
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))
(use-package rg)
(use-package yasnippet
  :init
  (yas-global-mode 1))
(use-package yasnippet-snippets)
(use-package load-env-vars)
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(provide 'nakhoa-defaults)

