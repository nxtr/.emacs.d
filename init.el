;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds
(progn ;     startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (setq package-enable-at-startup nil)
  ;; (package-initialize)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq load-prefer-newer t)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0))

(progn ;    `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn ;    `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

(use-package subr-x
  :config
  (put 'if-let   'byte-obsolete-info nil)
  (put 'when-let 'byte-obsolete-info nil))

(use-package auto-compile
  :demand t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t)
  (add-hook 'auto-compile-inhibit-compile-hook
            'auto-compile-inhibit-compile-detached-git-head))

(use-package epkg
  :defer t
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package custom
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :config (or (server-running-p) (server-mode)))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;;; Long tail

(use-package ace-window
  :demand t
  :bind ("M-o" . ace-window))

(use-package avy
  :init (avy-setup-default)
  :bind (("C-:"     . avy-goto-char)
         ("C-'"     . avy-goto-char-timer)
         ;; ("C-'"     . avy-goto-char-2)
         ("M-g g"   . avy-goto-line)
         ("M-g w"   . avy-goto-word-1)
         ("M-g e"   . avy-goto-word-0)
         ("C-c C-j" . avy-resume)))

(use-package dash
  :config (dash-enable-font-lock))

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package dired
  :defer t
  :config (setq dired-listing-switches "-alh"))

(use-package eldoc
  :when (version< "25" emacs-version)
  :config (global-eldoc-mode))

(use-package eshell
  :bind (("C-x m" . eshell)
         ;; New eshell
         ("C-x M" . (lambda ()
                      (interactive)
                      (eshell t)))))

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package fill-column-indicator
  :config
  (setq fci-rule-width 2)
  (setq fci-rule-column 80)
  (add-hook 'prog-mode-hook 'fci-mode)
  (add-hook 'git-commit-setup-hook 'fci-mode))

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(use-package hl-todo
  :config (global-hl-todo-mode))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode)
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'lisp-interaction-mode-hook #'indent-spaces-mode))

(use-package magit
  :defer t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  ;; Window management
  (setq magit-display-buffer-function
        'magit-display-buffer-fullframe-status-topleft-v1)
  ;; Status buffer settings
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package multiple-cursors
  :bind (("M-RET"   . mc/edit-lines)
         ("C-<"     . mc/mark-previous-like-this)
         ("C->"     . mc/mark-next-like-this)
         ("C-M-<"   . mc/unmark-next-like-this)
         ("C-M->"   . mc/unmark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package no-littering
  :demand t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (setq delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)
  (setq backup-directory-alist
        `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq create-lockfiles nil))

(use-package paren
  :config (show-paren-mode))

(use-package paren-face
  :config (global-paren-face-mode))

(use-package pdf-tools
  :demand t
  :config (pdf-tools-install))

(use-package prog-mode
  :config (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left))

(use-package recentf
  :demand t
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :when (version< "25" emacs-version)
  :config (save-place-mode))

(use-package shell
  :defer t
  :bind ("C-x M-m" . shell)
  :config
  (require 'with-editor)
  (add-hook 'shell-mode-hook 'with-editor-export-editor))

(use-package simple
  :config (column-number-mode))

(use-package term
  :defer t
  :config
  (require 'with-editor)
  (add-hook 'term-exec-hook 'with-editor-export-editor))

(progn ;    `text-mode'
  (add-hook 'text-mode-hook #'indicate-buffer-boundaries-left))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil)))

(use-package undo-tree
  :config (global-undo-tree-mode))

(use-package which-key
  :config (which-key-mode))

(use-package ws-butler
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode))

(progn ;     startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

(progn ;     host-specific
  (let* ((host (substring (shell-command-to-string "hostname") 0 -1))
         (host-dir-name (concat user-emacs-directory "hosts/" host))
         (host-file (expand-file-name "init.el" host-dir-name)))
    (when (file-exists-p host-dir-name)
      (let ((default-directory host-dir-name))
        (setq load-path
              (append
               ;; Shadow
               (let ((load-path  (copy-sequence load-path)))
                 (append
                  (copy-sequence (normal-top-level-add-to-load-path '(".")))
                  (normal-top-level-add-subdirs-to-load-path)))
               load-path)))
      (when (file-exists-p host-file)
        (load host-file)))))

(progn ;     personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
