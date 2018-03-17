(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)


(add-to-list 'load-path "~/.emacs.d/better-defaults/")
(require 'better-defaults)
(global-linum-mode t)
(xterm-mouse-mode t)
(server-mode  t)

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
