;;; c_setup --- setup for c
;;; Commentary:
;;; Code:

;; default c style : linux
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces.  (as IGNORED)."
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
	    (setq indent-tabs-mode t tab-width 8)
	    (setq show-trailing-whitespace t)
	    (c-set-style "linux-tabs-only")))

(defun my/emacs-c-mode-keybindings ()
  "Keybindings for c."
  (local-set-key (kbd "C-c C-c") 'compile))

(add-hook 'c-mode-hook 'my/emacs-c-mode-keybindings)
(add-hook 'c++-mode-hook 'my/emacs-c-mode-keybindings)
(provide 'c_setup)
;;; c_setup ends here
