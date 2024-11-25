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

(tool-bar-mode -1) ;; hide menubar
(setq inhibit-splash-screen t)

;; install packages (local git repos cloned):

(straight-use-package 'use-package)
;;(straight-use-package 'poly-markdown)


(straight-use-package 'treemacs) ;; like speedbar, but inside the frame by default
(require 'treemacs-project-follow-mode)
(treemacs-project-follow-mode t)

(straight-use-package 'vertico)
(straight-use-package 'eglot)
(straight-use-package 'company)
(straight-use-package 'clojure-mode)
(straight-use-package 'haskell-mode)
(straight-use-package 'swift-mode)
(straight-use-package 'json-mode)
(straight-use-package 'racket-mode)
(straight-use-package 'cider)
(straight-use-package 'intero) ;; Haskell
;;(straight-use-package 'dap-python) ;; Python debugging
(straight-use-package 'hy-mode) ;; hy mode
;;(use-package hy-mode :straight (:host github :repo "hylang/hy-mode" :files ("" "*.el"))  :ensure t)
;;(straight-use-package 'ielm)
;;(straight-use-package 'julia-mode)
;;(straight-use-package 'julia-repl)

(straight-use-package 'ellama)

(use-package ellama
    :ensure t
    :config
    ;;(setq ellama-provider 'ollama)
    (setq ellama-ollama-model "qwen2.5-coder:32b") ;; "mistral-nemo") ;;  ;;  ;; 12B ;; yi-coder:latest") ; Assuming you're using the 9B version

    ;; setup key bindings
    ;;;;(setopt ellama-keymap-prefix "C-c e")
    )

(global-set-key (kbd "C-c y") 'ellama-code-complete) ; For code completion with Yi-Coder

(use-package chatgpt-shell :straight (:host github :repo "xenodium/chatgpt-shell" :files ("" "*.el"))  :ensure t)
(setq chatgpt-shell-openai-key (getenv "OPENAI_API_KEY"))

;; Racket:
(straight-use-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(setq racket-program "/Applications/Racket/bin/racket")
;;(setq racket-logger-config '((racket-mode . debug)))

;;      window management:

;; Install ace-window
(straight-use-package 'ace-window)
;; Configure ace-window
(global-set-key (kbd "M-o") 'ace-window)  ; Bind ace-window to M-o
;; Optionally enable ace-window-display-mode
(ace-window-display-mode 1)

(straight-use-package 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'flyspell-mode) ; brew install aspell  and:
(setq ispell-program-name "aspell")
(add-hook 'markdown-mode-hook 'visual-line-mode)


;; this just spawns a web browser:
;;(use-package duckduckgo :straight (:host github :repo "akirak/duckduckgo.el" :files ("" "*.el"))  :ensure t)

;;(use-package copilot :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))  :ensure t)
;;(add-hook 'prog-mode-hook 'copilot-mode)
;;(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;;(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;;(load (expand-file-name "~/quicklisp/slime-helper.el"))
(load (expand-file-name "~/.roswell/lisp/quicklisp/slime-helper.el"))

;; when using roswell:
(load (expand-file-name "~/.roswell/helper.el"))
(setq inferior-lisp-program "ros -Q run --load ~/.sbclrc")

;;(Add-To-list 'auto-mode-alist '("\\.org" . poly-markdown-mode))

;;(setq inferior-lisp-program "/usr/local/bin/ccl64")
;;(setq inferior-lisp-program "/Users/markw/bin/lw-console")
;;(setq inferior-lisp-program "/Users/markwatson/bin/lw")
(setq inferior-lisp-program "ros -Q run --load ~/.sbclrc")
;;(setq inferior-lisp-program "/opt/homebrew/bin/sbcl")

;; Disable the tool bar
(tool-bar-mode -1)

;; Disable the scroll bars
;;(scroll-bar-mode -1)

;;(straight-use-package 'gerbil-mode)
;;(straight-use-package 'gambit)
;;(autoload 'gerbil-mode "gerbil-mode" "Gerbil editing mode." t)
;;(require 'gambit)
;;(add-hook 'inferior-scheme-mode-hook 'gambit-inferior-mode)
(setq scheme-program-name "/opt/homebrew/bin/gxi")

;; Enable LSP support by default in programming buffers
(vertico-mode t)
(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignore-case t)

;; Enable LSP support by default in programming buffers
;;(add-hook 'prog-mode-hook #'flymake-mode)

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
(custom-set-variables
 '(safe-local-variable-values
   '((encoding . utf-8)
     (syntax . Common-Lisp)
     (toc-org-max-depth . 3)
     (org-link-file-path-type . relative))))
(custom-set-faces
 )


(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
(add-to-list 'auto-mode-alist '("\\.s\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.hy\\'" . hy-mode))

(add-hook 'python-mode-hook '(lambda () 
			       (setq python-indent 2
				     python-guess-indent nil)))

(setq python-shell-interpreter "./venv/bin/python3")

(put 'upcase-region 'disabled nil)

(setq python-shell-completion-native-enable nil)

(when (display-graphic-p)
  ;; Set initial frame size
  (add-to-list 'initial-frame-alist '(width . 160))  ; Default is 80
  (add-to-list 'initial-frame-alist '(height . 60))  ; Default is 45

  ;; Increase default font size by 30%
  (let* ((current-height (face-attribute 'default :height))
         (new-height (truncate (* current-height 1.3))))
    (set-face-attribute 'default nil :height new-height))

  (setq initial-buffer-choice nil)
  (setq initial-scratch-message nil)
  (treemacs))
