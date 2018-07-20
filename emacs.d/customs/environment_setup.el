;;; environment_setup --- setup enviroment for emacs
;;; Commentary:
;;; Code:

;; set threshold for garbage collector
(setq gc-cons-threshold (* 256 1024 1024))
(setq vc-follow-symlinks t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq-default dired-listing-switches "-al")
(global-auto-revert-mode t)
(display-time-mode t)
(require 'projectile)
(projectile-mode t)
(require 'editorconfig)
(editorconfig-mode t)
(require 'base16-theme)
(load-theme 'base16-gruvbox-dark-hard t)
(require 'fzf)

;; user config
(add-to-list 'load-path "~/.emacs.d/better-defaults/")
(require 'better-defaults)
(global-linum-mode nil)
(xterm-mouse-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)
;; whitespace mode configure
(setq-default whitespace-style '(face trailing spaces tabs newline space-mark tab-mark))
(global-whitespace-mode nil)
(provide 'environment_setup)
;;; environment_setup ends here
