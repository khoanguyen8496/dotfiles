;;; autocomplete_config --- Setup for company and some language servers
;;; Commentary:
;;; Code:

;; company, flycheck and yasnippet
(require 'company)
(require 'flycheck)
(require 'yasnippet)

;;; turn on global mode for all previous packages
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'prog-mode-hook 'yas-global-mode)
(add-hook 'prog-mode-hook 'flycheck-mode)
(require 'lsp-mode)
(require 'company-lsp)

(delete 'company-backends '(company-semantic company-backends))
(add-to-list 'company-backends '(company-files company-dabbrev-code company-yasnippet company-lsp company-capf company-gtags))

;;; setup lsp and company integration

(setq company-lsp-cache-candidates t)
(setq company-lsp-async t)
(setq company-lsp-enable-snippet t)
(setq company-lsp-enable-recompletion t)
;;; set cquery variable
(require 'cquery)
;; enable for both c and c++
(add-hook 'c-mode-hook #'lsp-cquery-enable)
(add-hook 'c++-mode-hook #'lsp-cquery-enable)

(setq cquery-executable "/usr/local/bin/cquery")
(setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack" :completion (:detailedLabel t)))
(setq cquery-sem-highlight-method 'font-lock)

(require 'clang-format)
(blink-cursor-mode 0)

;;; config lsp python
(require 'lsp-python)
(add-hook 'python-mode-hook #'lsp-python-enable)

;;; config lsp javascript and typescript
(require 'lsp-javascript-typescript)
(add-hook 'js-mode-hook #'lsp-javascript-typescript-enable)
(add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable) ;; for typescript support
(add-hook 'js3-mode-hook #'lsp-javascript-typescript-enable) ;; for js3-mode support
(add-hook 'rjsx-mode #'lsp-javascript-typescript-enable) ;; for rjsx-mode support

;;; config lsp-java
(require 'lsp-java)
(add-hook 'java-mode-hook (lambda()
			    (setq company-lsp-cache-candidates nil)
			    (lsp-java-enable)
			    ))

;; set the projects that are going to be imported into the workspace.

(setq lsp-java-server-install-dir "/home/nakhoa/Downloads/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository")
(setq lsp-java--workspace-folders (list "~/work/nora_server/website"))
(require 'lsp-ui)
(provide 'autocomplete_setup)
;;; autocomplete_setup ends here
