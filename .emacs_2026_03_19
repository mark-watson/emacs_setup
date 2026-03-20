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
;;(straight-use-package 'ielm)
;;(straight-use-package 'julia-mode)
;;(straight-use-package 'julia-repl)
;;(straight-use-package 'xterm-mouse)

;;(load "/opt/homebrew/share/emacs/site-lisp/gerbil-scheme/gambit.el")
;;(load "/opt/homebrew/share/emacs/site-lisp/gerbil-scheme/gerbil-mode.el")
;;(require 'gambit)
;;(require 'gerbil-mode)
;;(add-to-list 'auto-mode-alist '("\\.ss\\'" . gerbil-mode))
;;

(use-package flycheck :ensure t :init (global-flycheck-mode))
;;(use-package flycheck-clj-kondo :ensure t)

;; Email Setup

;(setq gnus-select-method  
;      '(nnimap "proton"  
;               (nnimap-address "127.0.0.1")  
;               (nnimap-server-port 1143)  
;               (nnimap-stream starttls)  
;               (nnimap-user "mark_watson@protonmail.ch")  
;               (nnimap-password (getenv "PROT"))))  

;; For SMTP  
;(setq smtpmail-smtp-server "127.0.0.1"  
;      smtpmail-smtp-service 1025  
;      smtpmail-stream-type 'ssl  
;      smtpmail-smtp-user "mark_watson@protonmail.ch"  
;      smtpmail-smtp-password (getenv "PROT"))  


;; Sort by date, newest first  
;(setq gnus-thread-sort-functions  
;      '(gnus-thread-sort-by-date))  
;(setq gnus-sort-gathered-threads-function  
;      'gnus-thread-sort-by-date)  
;(setq gnus-thread-sort-by-date-function  
;      'gnus-thread-sort-by-date-descendingly)  

;; Disable threading if you want a flat list  
;(setq gnus-summary-thread-gathering-function   
;      'gnus-gather-threads-none)  


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

(straight-use-package 'treemacs) ;; like speedbar, but inside the frame by default
(require 'treemacs-project-follow-mode)
(treemacs-project-follow-mode t)


;; this just spawns a web browser:
;;(use-package duckduckgo :straight (:host github :repo "akirak/duckduckgo.el" :files ("" "*.el"))  :ensure t)

;;(use-package copilot :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))  :ensure t)
;;(add-hook 'prog-mode-hook 'copilot-mode)
;;(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;;(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(straight-use-package 'slime)
;;(load (expand-file-name "~/quicklisp/slime-helper.el"))
;;(add-to-list 'auto-mode-alist '("\\.org" . poly-markdown-mode))

;;(setq inferior-lisp-program "/usr/local/bin/ccl64")
;;(setq inferior-lisp-program "/Users/markw/bin/lw-console")
(setq inferior-lisp-program "/Users/markwatson/bin/lw")
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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages '((whisper :url "https://github.com/natrys/whisper.el")))
 '(safe-local-variable-values
   '((encoding . utf-8) (syntax . Common-Lisp) (toc-org-max-depth . 3)
     (org-link-file-path-type . relative))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
(add-to-list 'auto-mode-alist '("\\.s\\'" . scheme-mode))

(add-hook 'python-mode-hook '(lambda () 
			       (setq python-indent 2
				     python-guess-indent nil)))

;;(setq dap-python-debugger 'debugpy)
;;;; Enabling only some features
;;(setq dap-auto-configure-features '(sessions locals controls tooltip))

;; Terminal mouse support (makes wheel come in as mouse-4/mouse-5 in -nw)
(unless (display-graphic-p)
  (xterm-mouse-mode 1))

;; Ensure the wheel code is loaded so keymaps exist
(require 'mwheel nil t)

;; Pick the right map name (varies by Emacs/version/build)
(defvar my-wheel-map
  (cond ((boundp 'mwheel-mode-map)       mwheel-mode-map)
        ((boundp 'mouse-wheel-mode-map)  mouse-wheel-mode-map)
        (t nil)))

;; "Natural" scrolling: two-finger up => content moves up/down as in macOS/Windows
;; If this ends up still opposite on your system, swap the two commands below.
(when my-wheel-map
  ;; GUI events
  (define-key my-wheel-map [wheel-up]   #'scroll-down-line)
  (define-key my-wheel-map [wheel-down] #'scroll-up-line)
  ;; Terminal/older events
  (define-key my-wheel-map [mouse-4]    #'scroll-down-line)
  (define-key my-wheel-map [mouse-5]    #'scroll-up-line))

;; Fallback for -nw even if no wheel map exists
(unless (display-graphic-p)
  (global-set-key [mouse-4] #'scroll-down-line)
  (global-set-key [mouse-5] #'scroll-up-line))




(defun uv-run ()
  (interactive)
  (compile (format "uv run %s" (buffer-file-name))))

(global-set-key (kbd "C-c u") #'uv-run)

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

(defun my/treemacs-ignore-temp-files (filename _)
    ;;Ignore Emacs backup, auto-save, and lock files.
    (or (string-suffix-p "~" filename)      ; Backup files
        (string-prefix-p "#" filename)      ; Auto-save files
        (string-prefix-p ".#" filename)   ; Lock files
	(string-equal ".DS_Store" filename)))
  
(push #'my/treemacs-ignore-temp-files treemacs-ignored-file-predicates)

(require 'dired-x)
;; Define the regex for files to hide
(setq dired-omit-files
      (concat dired-omit-files "\\|^#.*#$\\|\\.~.*$\\|^\\.#.*$\\|^\\.DS_Store$"))
  
;; Automatically enable omit mode in Dired buffers
(add-hook 'dired-mode-hook #'dired-omit-mode)



