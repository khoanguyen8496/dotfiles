;;; package -- init.el
;;; commentary:
;;; code:
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "https://marmalade-repo.org/packages/") t)

(defvar my-packages
  '(
    ace-window
    ag
    auctex
    base16-theme
    better-defaults
    browse-kill-ring
    clang-format
    company
    company-lsp
    cquery
    editorconfig
    expand-region
    flycheck
    fzf
    ggtags
    google-c-style
    lsp-java
    lsp-javascript-typescript
    lsp-mode
    lsp-python
    lsp-ui
    magit
    move-text
    projectile
    smex
    yasnippet
    yasnippet-snippets
    zenburn-theme
    restclient
    ))

;; emulate iterate packages and install all of them
(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-refresh-contents)
    (package-install p))
  (add-to-list 'package-selected-packages p))

;; add path for custom setups
(add-to-list 'load-path "~/.emacs.d/customs/")

(require 'environment_setup)
(require 'autocomplete_setup)
(require 'keybindings_setup)
(require 'c_setup)
(require 'c++_setup)
(require 'cmake-mode)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;;; set font
(add-to-list 'default-frame-alist '(font . "Monaco-14" ))
(set-face-attribute 'default t :font "Monaco-14" )
(set-face-attribute 'default nil :font "Monaco-14" )
(set-frame-font "Monaco-14" nil t)

(put 'narrow-to-page 'disabled nil)

;;; init.el ends here
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
