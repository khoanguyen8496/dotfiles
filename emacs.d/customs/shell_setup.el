;;; shell_setup --- setup for bash shell
;;; Commentary:
;;; Code:

;; default shell style
(add-hook 'sh-mode-hook (lambda ()
			  (setq tab-width 8 indent-tabs-mode t)))
(provide 'shell_setup)
;;; shell_setup ends here
