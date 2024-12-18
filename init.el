;; (require 'package)
;; (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                     (not (gnutls-available-p))))
;;        (proto (if no-ssl "http" "https")))
;;   (when no-ssl (warn "\
;; Your version of Emacs does not support SSL connections,
;; which is unsafe because it allows man-in-the-middle attacks.
;; There are two things you can do about this warning:
;; 1. Install an Emacs version that does support SSL and be safe.
;; 2. Remove this warning from your init file so you won't see it again."))
;;   (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
;;   ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;;   ;; and `package-pinned-packages`. Most users will not need or want to do this.
;;   ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
;;   )
;; (package-initialize)
;; (setq package-enable-at-startup nil)
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))


;; use-package to simplify the config file
;; (straight-use-package 'use-package)

;; (setq straight-use-package-by-default 't)
(add-to-list 'load-path '"~/.emacs.d/modules")

(require 'nakhoa-packages)
(require 'nakhoa-defaults)
(require 'nakhoa-lsp)
(require 'nakhoa-interface)
(use-package go-mode)
(use-package rust-mode)
(use-package org)

(load-theme 'whiteboard t)
(setq default-frame-alist '((font . "Source Code Pro-14")))
;; Behave like vi's o command

;; Autoindent open-*-lines

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(consult go-mode load-env-vars lsp-mode magit marginalia orderless
	     projectile rg rust-mode smartparens tree-sitter-langs
	     undo-tree vertico yasnippet-snippets))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
