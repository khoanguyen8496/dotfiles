;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(use-package better-defaults)
(use-package go-mode
  :bind (:map go-mode-map
	      ("M-." . godef-jump)))
(use-package flycheck
  :config
  (global-flycheck-mode))
(use-package expand-region
  :bind ("C-=" . er/expand-region))
(use-package go-guru)
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package projectile-ripgrep)
(use-package magit)
(use-package which-key
  :config
  (which-key-mode))
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))
(use-package undo-tree
  :bind
  ("C-/" . undo-tree-undo)
  ("M-_" . undo-tree-redo)
  ("C-M-_" . undo-tree-visualize))

(use-package counsel
  :after ivy
  :config (counsel-mode))

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode t))
;; (ivy-set-display-transformer 'ivy-switch-buffer
;;'ivy-rich-switch-buffer-transformer))
;; (use-package all-the-icons-ivy
;;   :ensure t
;;   :config
;;   (all-the-icons-ivy-setup))
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package dockerfile-mode)
(use-package json-mode)

(load-theme 'wombat t)
(blink-cursor-mode 0)
(setq dired-listing-switches "-lah")
(add-to-list 'default-frame-alist '(font . "Monaco-12" ))
(set-face-attribute 'default t :font "Monaco-12" )
(set-face-attribute 'default nil :font "Monaco-12" )
(set-frame-font "Monaco-12" nil t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-rich-mode t)
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate (quote full))
 '(package-selected-packages
   (quote
    (json-mode dockerfile-mode all-the-icons-ivy ivy-rich counsel flycheck editorconfig fzf undo-tree expand-region better-defaults which-key magit projectile-ripgrep projectile go-guru go-mode use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
