(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

  (setq straight-use-package-by-default t)

;; install packages (local git repos cloned):

(straight-use-package 'use-package)
(straight-use-package 'poly-markdown)
(straight-use-package 'vertico)
(straight-use-package 'eglot)
(straight-use-package 'company)
(straight-use-package 'clojure-mode)
(straight-use-package 'haskell-mode)
(straight-use-package 'json-mode)
(straight-use-package 'markdown-mode)
(straight-use-package 'cider)
(straight-use-package 'intero) ;; Haskell
(straight-use-package 'treemacs)


(load (expand-file-name "~/quicklisp/slime-helper.el"))
(add-to-list 'auto-mode-alist '("\\.org" . poly-markdown-mode))

;;(setq inferior-lisp-program "/usr/local/bin/ccl64")
(setq inferior-lisp-program "/Users/markw_1/bin/lw")
;;(setq inferior-lisp-program "/Users/markw_1/bin/sbcl/bin/sbcl")

;; Disable the tool bar
(tool-bar-mode -1)

;; Disable the scroll bars
(scroll-bar-mode -1)

;; Enable LSP support by default in programming buffers
(vertico-mode t)
(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignore-case t)

;; Enable LSP support by default in programming buffers
(add-hook 'prog-mode-hook #'flymake-mode)

;; Pop-up auto-completion
;; Enable Company by default in programming buffers
(add-hook 'prog-mode-hook #'company-mode)

;; Miscellaneous options
;(setq confirm-kill-emacs #'yes-or-no-p)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)