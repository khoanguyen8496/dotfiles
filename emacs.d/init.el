(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.milkbox.net/packages/") t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3fa81193ab414a4d54cde427c2662337c2cab5dd4eb17ffff0d90bca97581eb6" "b050365105e429cb517d98f9a267d30c89336e36b109a1723d95bc0f7ce8c11d" "7366916327c60fdf17b53b4ac7f565866c38e1b4a27345fe7facbf16b7a4e9e8" "490644a43ad9f71848f067be117bab2839b7c010eb9ec439abf3908d9a63d1dd" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; theme-config
(load-theme 'gruvbox-light-hard)
;; user config
(add-to-list 'load-path "~/.emacs.d/better-defaults/")
(require 'better-defaults)
(global-linum-mode t)
(xterm-mouse-mode t)
(server-mode  t)
(setq ido-everywhere t)

;;; default c++ style : google
(add-hook 'c++-mode-hook 'google-set-c-style)
(add-hook 'c++--hook 'google-make-newline-indent)

;; default c style : linux
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
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
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "~/src/linux-trees")
                                       filename))
                (setq indent-tabs-mode t)
                (setq show-trailing-whitespace t)
                (c-set-style "linux-tabs-only")))))

(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal)
					; when Smex is auto-initialized on its first run.

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(defalias 'yes-or-no-p 'y-or-n-p)
;;; set font
(add-to-list 'default-frame-alist '(font . "Liberation Mono-11" ))
(set-face-attribute 'default t :font "Liberation Mono-11" )
(set-face-attribute 'default nil :font "Liberation Mono-11" )
(set-frame-font "Liberation Mono-11" nil t)
(put 'narrow-to-page 'disabled nil)
;;; company config
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;;; irony mode config with company
(require 'irony)
(require 'company-irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(require 'clang-format)
(blink-cursor-mode 0)
