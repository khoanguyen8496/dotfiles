;;; keybinding_setups --- Summary
;;; Commentary:
;;; Code:

(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta mac-option-modifier 'super))
(require 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "M-o") 'other-window)
(smex-initialize) ; Can be omitted. This might cause a (minimal)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(require 'ag)
(global-set-key (kbd "C-c ag") 'ag)
(global-set-key (kbd "C-c mg") 'magit)

;; This is your old M-x.
(global-set-key (kbd "C-c M-x") 'execute-extended-command)

(require 'expand-region)
(global-set-key (kbd "M-]") 'er/expand-region)

(provide 'keybindings_setup)
;;; keybindings_setup ends here
