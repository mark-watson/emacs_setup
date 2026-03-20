;; Minimal init.el — styling, indenting, and markdown support only

;; --- package setup --------------------------------------------------------
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; ensure use-package is available
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; --- UI / basic behavior --------------------------------------------------
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t
      inhibit-startup-message t)

;; sensible defaults
(setq-default indent-tabs-mode nil    ;; use spaces, not tabs
              tab-width 4)
(setq-default fill-column 80)

(show-paren-mode 1)
(electric-pair-mode 1)
(global-auto-revert-mode 1)
(save-place-mode 1)
(recentf-mode 1)
(savehist-mode 1)

;; frame/font tweaks (only in GUI)
(when (display-graphic-p)
  (add-to-list 'initial-frame-alist '(width . 120))
  (add-to-list 'initial-frame-alist '(height . 48))
  (when (facep 'default)
    (let ((h (face-attribute 'default :height)))
      (set-face-attribute 'default nil :height (truncate (* h 1.15))))))

;; --- Programming defaults -------------------------------------------------
(defun my/prog-defaults ()
  "Defaults for programming buffers."
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)
  (setq-local truncate-lines nil)
  (display-line-numbers-mode 1)
  (hs-minor-mode 1)) ;; code folding

(add-hook 'prog-mode-hook #'my/prog-defaults)

;; make sure text files wrap nicely
(add-hook 'text-mode-hook 'visual-line-mode)

;; --- Markdown -------------------------------------------------------------
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" "\\.markdown\\'")
  :commands markdown-mode
  :hook ((markdown-mode . flyspell-mode)
         (markdown-mode . visual-line-mode))
  :init
  (setq markdown-command "pandoc")) ;; optional: use pandoc if installed

;; ispell fallback
(setq ispell-program-name (or (executable-find "aspell")
                              (executable-find "hunspell")
                              "ispell"))

;; --- programming language support  ----------------------------------------

(use-package cider :ensure t)
(use-package clojure-mode :ensure t)
;; (use-package exec-path-from-shell :ensure t) ;; Moved to config section below
;;(use-package haskell-mode :ensure t)
(use-package hy-mode :ensure t)
;;(use-package intero :ensure t) ;; Haskell
(use-package json-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package swift-mode :ensure t)
(use-package treemacs :ensure t)
(use-package vertico :ensure t)

;; --- Python / uv / .venv handling -----------------------------------------

(use-package pyvenv :ensure t)

(defun my/find-venv-root ()
  "Walk up the directory tree looking for a .venv directory."
  (let ((root (locate-dominating-file default-directory ".venv")))
    (when root
      (expand-file-name ".venv" root))))

(defun my/pyvenv-auto-activate ()
  "Activate .venv if found in the directory tree."
  (interactive)
  (let ((venv-path (my/find-venv-root)))
    (if (and venv-path (file-directory-p venv-path))
        (progn
          ;; 1. Activate via pyvenv
          (pyvenv-activate venv-path)
          ;; 2. Explicitly set the interpreter for the python shell (C-c C-p)
          (setq-local python-shell-interpreter 
                      (expand-file-name "bin/python" venv-path))
          (message "Activated venv: %s" venv-path))
      
      ;; Fallback if no venv found
      (pyvenv-deactivate))))

(add-hook 'python-mode-hook #'my/pyvenv-auto-activate)

;; Python indentation preferences
(setq python-indent-offset 2
      python-guess-indent nil
      python-shell-completion-native-enable nil)

(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent-offset 2
                  tab-width 2)))

;; --- Common Lisp (slime/roswell) ------------------------------------------
(load (expand-file-name "~/.roswell/lisp/quicklisp/slime-helper.el"))
(load (expand-file-name "~/.roswell/helper.el"))
(setq inferior-lisp-program "/opt/homebrew/bin/sbcl")

;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; --- Small conveniences ---------------------------------------------------
(global-set-key (kbd "M-o") #'other-window) ;; minimal window switch
(setq-default sentence-end-double-space nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(provide 'init)

;; --- Custom Variables -----------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(cider exec-path-from-shell gptel hy-mode json-mode llm markdown-mode package-x
           pyvenv pyvenv-auto swift-mode transient-cycles treemacs vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )