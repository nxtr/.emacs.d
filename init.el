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
  (when (< emacs-major-version 27)
    (setq package-enable-at-startup nil)
    ;; (package-initialize)
    (load-file (expand-file-name "early-init.el" user-emacs-directory)))
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
  (menu-bar-mode 0))

(progn ;    `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn ;    `use-package'
  (eval-when-compile
    (require 'use-package))
  ;; (require 'diminish)                  ;if you use :diminish
  (require 'bind-key)                   ;if you use any :bind variant
  (setq use-package-verbose t))

(use-package auto-compile
  :config
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t))

(use-package epkg
  :defer t
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package custom
  :no-require t
  :config (progn
            (setq custom-file
                  (expand-file-name "custom.el" user-emacs-directory))
            (when (file-exists-p custom-file)
              (load custom-file))))

(use-package server
  :config (or (server-running-p) (server-mode)))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;;; Long tail

(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package ace-window
  :demand t
  :bind ("M-o" . ace-window)
  :config
  (setq aw-background nil)
  (setq aw-dispatch-always 't)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package avy
  :init (avy-setup-default)
  :bind (("C-:"     . avy-goto-char)
         ("C-'"     . avy-goto-char-timer)
         ;; ("C-'"     . avy-goto-char-2)
         ("M-g g"   . avy-goto-line)
         ("M-g w"   . avy-goto-word-1)
         ("M-g e"   . avy-goto-word-0)
         ("C-c C-j" . avy-resume))
  :config
  (setq avy-background      t))

(use-package cc-mode
  :hook ((java-mode . (lambda ()
                        (c-set-offset 'arglist-intro '++)))))

(use-package company
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
              ("C-c h" . company-quickhelp-manual-begin)
              ("C-n"   . company-select-next)
              ("C-p"   . company-select-previous))
  :config
  (setq company-minimum-prefix-length   2)
  (setq company-show-numbers            t)
  (setq company-tooltip-flip-when-above t))

(use-package company-quickhelp)

(use-package counsel
  :after ivy
  :hook (ivy-mode . (lambda ()
                      (if ivy-mode
                          (counsel-mode +1)
                        (counsel-mode -1))))
  :bind (:map counsel-mode-map
              ("C-c c"  . counsel-compile)
              ("C-c g"  . counsel-git)
              ("C-c j"  . counsel-git-grep)
              ("C-c L"  . counsel-git-log)
              ("C-c k"  . counsel-rg)
              ("C-c o"  . counsel-outline)
              ("C-x l"  . counsel-locate)
              ("<f1> l" . counsel-find-library)
              ("C-c J"  . counsel-file-jump)
              ("C-c f"  . counsel-recentf)
              ("<f2> u" . counsel-unicode-char)))

(use-package dash
  :config (dash-enable-font-lock))

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package diff-mode
  :defer t
  :config
  (when (>= emacs-major-version 27)
    (set-face-attribute 'diff-refine-changed nil :extend t)
    (set-face-attribute 'diff-refine-removed nil :extend t)
    (set-face-attribute 'diff-refine-added   nil :extend t)))

(use-package dired
  :defer t
  :config (setq dired-listing-switches "-alh"))

(use-package disable-mouse
  :when (display-mouse-p))

(use-package display-line-numbers
  :no-require t
  :when (version<= "26.1" emacs-version)
  :hook (prog-mode . display-line-numbers-mode)
  :bind ([remap goto-line]
         . (lambda ()
             (interactive)
             (let* ((initial-buffer (current-buffer))
                    (buffer (if (consp current-prefix-arg)
                                (other-buffer (current-buffer) t)
                              initial-buffer))
                    numbers)
               (with-current-buffer buffer
                 (unwind-protect
                     (let ((display-line-numbers-width-start t)
                           display-line-numbers-grow-only)
                       (setq numbers display-line-numbers)
                       (setq display-line-numbers t)
                       ;; Force `buffer' to be other buffer
                       (set-buffer initial-buffer)
                       (call-interactively #'goto-line))
                   (setq display-line-numbers numbers)))))))

(use-package eldoc
  :when (version< "25" emacs-version)
  :config (global-eldoc-mode))

(use-package epa
  :config (setq epa-pinentry-mode 'loopback))

(use-package erc
  :bind (:map erc-mode-map
              ("RET"       . nil)
              ("C-c RET"   . erc-send-current-line)
              ("C-c C-RET" . erc-send-current-line))
  :config
  ;; Force TLS
  (setq erc-server-connect-function 'erc-open-tls-stream)
  (setq erc-server "chat.freenode.net")
  ;; Use auth-source as password store
  (advice-add 'erc-compute-port :before-until
              (lambda (&optional _)
                (plist-get(nth 0 (auth-source-search :host erc-server :max 1))
                          :port)))
  (advice-add 'erc-compute-nick :before-until
              (lambda (&optional _)
                (plist-get (nth 0 (auth-source-search :host erc-server :max 1))
                           :user)))
  (setq erc-prompt-for-password nil)
  (erc-services-mode 1)
  (setq erc-prompt-for-nickserv-password nil)

  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-rename-buffers t)

  (setq erc-autojoin-timing 'ident)
  (setq erc-autojoin-channels-alist '(("freenode.net" "#emacs")))

  (setq erc-kill-buffer-on-part t)
  (setq erc-kill-queries-on-quit t)
  (setq erc-kill-server-buffer-on-quit t))

(use-package eshell
  :bind (("C-x m" . eshell)))

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package files
  :config (defun find-file-sudo ()
            "Reopen the current file as root, preserving point position."
            (interactive)
            (let ((p (point)))
              (find-alternate-file
               (concat "/sudo:root@localhost:" buffer-file-name))
              (goto-char p))))

(use-package flycheck)

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(use-package hl-todo
  :config (global-hl-todo-mode))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package imenu
  :bind (("M-i" . imenu))
  :config
  (setq imenu-max-item-length nil))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-package ivy
  :commands ivy-mode
  :bind (:map ivy-mode-map
              ("C-c r"   . ivy-resume)
              ("C-x B"   . ivy-switch-buffer-other-window)
              ("C-c v"   . ivy-push-view)
              ("C-c V"   . ivy-pop-view)
              ("C-c C-v" . ivy-switch-view))

  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-extra-directories nil)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) "))

(use-package ivy-posframe
  :commands ivy-posframe-mode
  :config
  (setq ivy-posframe-display-functions-alist
        '((swiper . ivy-display-function-fallback)
          (swiper-all . ivy-display-function-fallback)
          (swiper-isearch . ivy-display-function-fallback)
          (counsel-rg . ivy-display-function-fallback)
          (counsel-git-grep . ivy-display-function-fallback)
          (counsel-locate . ivy-display-function-fallback)
          (t . ivy-posframe-display-at-point)))
  (setq ivy-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  (setq ivy-posframe-size-function
        (lambda ()
          (list
           :height ivy-posframe-height
           :width ivy-posframe-width
           :min-height (or ivy-posframe-min-height ivy-height)
           :min-width
           (or ivy-posframe-min-width
               (let* ((prompt (split-string
                               (with-temp-buffer
                                 (ivy--insert-prompt)
                                 (buffer-string))
                               "\n"))
                      (prompt-col (seq-max (seq-map #'length prompt))))
                 (max (+ prompt-col (length ivy-text) 2)))))))
  (define-advice ivy-posframe--display (:around (fun &rest args) nxtr/ivy-posframe--display)
    (let ((ivy-posframe-font (face-attribute 'default :font (selected-frame))))
      (apply fun args))))

(use-package linum
  :when (version< emacs-version "26.1")
  :hook (prog-mode . linum-mode)
  :bind ([remap goto-line]
         . (lambda ()
             (interactive)
             (let* ((initial-buffer (current-buffer))
                    (buffer (if (consp current-prefix-arg)
                                (other-buffer (current-buffer) t)
                              initial-buffer)))
               (with-current-buffer buffer
                 (cond ((or linum-mode
                            ;; Numeric prefix argument
                            (and current-prefix-arg (not (consp current-prefix-arg))))
                        ;; Force `buffer' to be other buffer
                        (set-buffer initial-buffer)
                        (call-interactively #'goto-line))
                       (t (unwind-protect
                              (let ((linum-format 'dynamic)
                                    (linum-eager linum-delay))
                                (linum-mode 1)
                                ;; Force `buffer' to be other buffer
                                (set-buffer initial-buffer)
                                (call-interactively #'goto-line))
                            (linum-mode -1)))))))))

(use-package lisp-mode
  :hook ((emacs-lisp-mode lisp-mode)
         . (lambda ()
             (add-hook 'after-save-hook 'check-parens nil t)))
  :config
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode)
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'lisp-interaction-mode-hook #'indent-spaces-mode))

(use-package lisp-mode
  :after paredit
  :bind (:map lisp-interaction-mode-map
              ("C-S-j" . eval-print-last-sexp)))

(use-package magit
  :defer t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config
  ;; Window management
  (setq magit-display-buffer-function
        'magit-display-buffer-fullframe-status-topleft-v1)
  ;; Status buffer settings
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append)
  (add-hook 'magit-section-movement-hook 'magit-status-maybe-update-blob-buffer))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package multiple-cursors
  :bind (("M-RET"         . mc/edit-lines)
         ("C-<"           . mc/mark-previous-like-this)
         ("C->"           . mc/mark-next-like-this)
         ("C-M-<"         . mc/unmark-next-like-this)
         ("C-M->"         . mc/unmark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-S-SPC"       . mc/toggle-cursor-at-point)
         ("<C-S-return>"  . multiple-cursors-mode)
         ("C-S-<mouse-1>" . 'mc/add-cursor-on-click))
  :config
  ;; https://stackoverflow.com/questions/39882624/setting-arbitrary-cursor-positions-with-multiple-cursors-in-emacs
  (defun mc/toggle-cursor-at-point ()
    "Add or remove a cursor at point."
    (interactive)
    (if multiple-cursors-mode
        (message "Cannot toggle cursor at point while `multiple-cursors-mode' is active.")
      (let ((existing (mc/fake-cursor-at-point)))
        (if existing
            (mc/remove-fake-cursor existing)
          (mc/create-fake-cursor-at-point)))))
  (add-to-list 'mc/cmds-to-run-once 'mc/toggle-cursor-at-point)
  (add-to-list 'mc/cmds-to-run-once 'multiple-cursors-mode))

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

(use-package paredit
  :hook ((emacs-lisp-mode
          ielm-mode
          lisp-mode
          eval-expression-minibuffer-setup) . paredit-mode))

(use-package paren
  :config (show-paren-mode))

(use-package paren-face
  :config (global-paren-face-mode))

(use-package pdf-tools
  :defer t
  :init (pdf-tools-install))

(use-package pdf-view
  :after pdf-tools
  :bind (:map pdf-view-mode-map
              ("C-r" . isearch-backward)
              ("C-s" . isearch-forward))
  :config (setq pdf-view-use-unicode-ligther nil)
  :hook (pdf-view-mode
         . (lambda ()
             (let ((oldmap (cdr (assoc 'ivy-mode minor-mode-map-alist)))
                   (newmap (make-sparse-keymap)))
               (set-keymap-parent newmap oldmap)
               (define-key newmap (kbd "C-r") nil)
               (define-key newmap (kbd "C-s") nil)
               (push `(ivy-mode . ,newmap) minor-mode-overriding-map-alist)))))

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

(use-package smex)

(use-package swiper
  :after ivy
  :config
  (define-key ivy-mode-map (kbd "C-s")
    (defalias (make-symbol "swiper-or-swiper-all")
      ;; Wrapped with `defalias' and uninterned SYMBOL so
      ;; `describe-key' displays command as a proper symbol
      ;; instead of byte-codes
      (lambda ()
        "Runs the command swiper.
With a prefix argument, run the command swiper-all."
        (interactive)
        (if current-prefix-arg
            (swiper-all)
          (swiper-isearch)))))
  (define-key ivy-mode-map (kbd "C-r")
    (lookup-key ivy-mode-map (kbd "C-s"))))

(use-package term
  :defer t
  :config
  (require 'with-editor)
  (add-hook 'term-exec-hook 'with-editor-export-editor))

(use-package smerge-mode
  :defer t
  :config (when (>= emacs-major-version 27)
            (set-face-attribute 'smerge-refined-removed nil :extend t)
            (set-face-attribute 'smerge-refined-added   nil :extend t)))

(progn ;    `text-mode'
  (add-hook 'text-mode-hook #'indicate-buffer-boundaries-left))

(progn ;     themes
  (defadvice load-theme (before theme-dont-propagate activate)
    (mapc #'disable-theme custom-enabled-themes)))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist
               '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil))
  (setq vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp)))

(use-package undo-tree
  :config (global-undo-tree-mode))

(use-package which-key
  :config (which-key-mode))

(use-package whitespace
  :hook ((prog-mode . whitespace-mode)
         (nxml-mode . whitespace-mode))
  :config (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package ws-butler
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  (add-hook 'nxml-mode-hook #'ws-butler-mode))

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

(progn ;     local packages
  (let ((dir (expand-file-name "local" user-emacs-directory)))
    (when (file-exists-p dir)
      (let ((default-directory dir))
        (add-to-list 'load-path dir)
        (normal-top-level-add-subdirs-to-load-path)))))

(progn ;     host-specific init
  (let* ((host (substring (shell-command-to-string "hostname") 0 -1))
         (host-dir (concat user-emacs-directory "hosts/" host))
         (host-file (expand-file-name "init.el" host-dir)))
    (when (file-exists-p host-dir)
      (let ((default-directory host-dir))
        (setq load-path
              (append
               (let ((load-path (copy-sequence load-path)))
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
