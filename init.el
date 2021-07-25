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
    (load-file (expand-file-name "early-init.el" user-emacs-directory))))

(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

(progn ;    `use-package'
  (eval-when-compile
    (require 'use-package))
  (require 'diminish)                   ;if you use :diminish
  (require 'bind-key)                   ;if you use any :bind variant
  (setq use-package-verbose t))

(use-package auto-compile)

(use-package epkg
  :defer t
  :init
  (setq epkg-repository (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package custom
  :no-require t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :commands (server-running-p)
  :config
  (or (server-running-p) (server-mode)))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time) before-user-init-time))))

;;; Long tail

(use-package abbrev
  :config
  (setq-default abbrev-mode t))

(use-package ace-window
  :demand t
  :bind
  (("M-o" . ace-window)
   ("M-O"
    . (lambda (&rest args)
        "Runs an alternative command `ace-window' that always dispatch everywhere."
        (interactive (advice-eval-interactive-spec
                      (cadr (interactive-form 'ace-window))))
        (let ((aw-dispatch-always t)
              aw-ignore-on)
          (apply 'ace-window args))))
   ("C-M-o"
    . (lambda (&rest args)
        "Runs an alternative command `ace-window' that always dispatch everywhere.

Scope will be opposite to `frame'/`global'."
        (interactive (advice-eval-interactive-spec
                      (cadr (interactive-form 'ace-window))))
        (let ((aw-scope (if (eq aw-scope 'frame)
                            'global
                          'frame))
              (aw-dispatch-always t)
              aw-ignore-on)
          (apply 'ace-window args))))))

(use-package avy
  :init (avy-setup-default)
  :bind
  (("C-:"     . avy-goto-char)
   ("C-'"     . avy-goto-char-timer)
   ;; ("C-'"     . avy-goto-char-2)
   ("M-g g"   . avy-goto-line)
   ("M-g w"   . avy-goto-word-1)
   ("M-g e"   . avy-goto-word-0)
   ("C-c C-j" . avy-resume)))

(use-package company
  :bind
  (:map company-active-map
        ("C-c h" . company-quickhelp-manual-begin)
        ("C-n"   . company-select-next)
        ("C-p"   . company-select-previous)))

(use-package company-quickhelp)

(use-package counsel
  :after ivy
  :diminish
  :hook
  (ivy-mode
   . (lambda ()
       (if ivy-mode
           (counsel-mode 1)
         (counsel-mode -1))))
  :bind
  (:map counsel-mode-map
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

(use-package dash)

(use-package eieio)

(use-package diff-hl)

(use-package diff-mode
  :defer t
  :config
  (when (>= emacs-major-version 27)
    (set-face-attribute 'diff-refine-changed nil :extend t)
    (set-face-attribute 'diff-refine-removed nil :extend t)
    (set-face-attribute 'diff-refine-added   nil :extend t)))

(use-package dired
  :defer t)

(use-package disable-mouse
  :when (display-mouse-p))

(use-package display-line-numbers
  :no-require t
  :when (version<= "26.1" emacs-version)
  :bind
  ([remap goto-line]
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
                 (call-interactively 'goto-line))
             (setq display-line-numbers numbers)))))))

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions 'dumb-jump-xref-activate))

(use-package eldoc
  :when (version< "25" emacs-version))

(use-package emacs-lisp-mode
  :defer t
  :hook
  (emacs-lisp-mode
   . (lambda ()
       (add-hook 'after-save-hook 'check-parens nil t))))

(use-package epa)

(use-package erc
  :bind
  (:map erc-mode-map
        ("RET"       . nil)
        ("C-c RET"   . erc-send-current-line)
        ("C-c C-RET" . erc-send-current-line))
  :config
  (advice-add 'erc-compute-port :before-until
              (lambda (&optional _)
                (plist-get(nth 0 (auth-source-search :host erc-server :max 1))
                          :port)))
  (advice-add 'erc-compute-nick :before-until
              (lambda (&optional _)
                (plist-get (nth 0 (auth-source-search :host erc-server :max 1))
                           :user))))

(use-package eshell
  :bind
  ("C-x M-m" . eshell))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package files
  :config
  (defun find-file-sudo ()
    "Reopen the current file as root, preserving point position."
    (interactive)
    (let ((p (point)))
      (find-alternate-file
       (concat "/sudo:root@localhost:" buffer-file-name))
      (goto-char p))))

(use-package flycheck)

(use-package help
  :defer t)

(use-package hl-todo)

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer))

(use-package imenu
  :bind
  ("M-i" . imenu))

(use-package ivy
  :demand t
  :diminish
  :bind
  (:map ivy-mode-map
        ("C-c r"   . ivy-resume)
        ("C-x B"   . ivy-switch-buffer-other-window)
        ("C-c v"   . ivy-push-view)
        ("C-c V"   . ivy-pop-view)
        ("C-c C-v" . ivy-switch-view))
  :config
  (ivy-mode 1))

(use-package ivy-posframe
  :after ivy
  :diminish
  :hook
  (ivy-mode
   . (lambda ()
       (if ivy-mode
           (ivy-posframe-mode 1)
         (ivy-posframe-mode -1))))
  :config
  (setq ivy-posframe-display-functions-alist
        '((swiper           . ivy-display-function-fallback)
          (swiper-all       . ivy-display-function-fallback)
          (swiper-isearch   . ivy-display-function-fallback)
          (counsel-rg       . ivy-display-function-fallback)
          (counsel-git-grep . ivy-display-function-fallback)
          (counsel-locate   . ivy-display-function-fallback)
          (t                . ivy-posframe-display-at-point)))
  (setq ivy-posframe-size-function
        (lambda ()
          (list
           :height (or ivy-posframe-height ivy-height)
           :width ivy-posframe-width
           :min-height (or ivy-posframe-min-height ivy-height)
           :min-width
           (or ivy-posframe-min-width
               (let* ((buf-rows (split-string
                                 (with-current-buffer ivy-posframe-buffer
                                   (buffer-string))
                                 "\n"))
                      (prompt (split-string
                               (with-temp-buffer
                                 (ivy--insert-prompt)
                                 (buffer-string))
                               "\n"))
                      (max-prompt-col (seq-max (seq-map 'length prompt))))
                 (max (seq-max (seq-map 'length buf-rows))
                      (+ max-prompt-col (length ivy-text) 2)))))))
  :config
  (define-advice ivy-posframe--display (:around (fun &rest args))
    (let ((ivy-posframe-font
           (face-attribute 'default :font (selected-frame))))
      (apply fun args)))
  (ivy-posframe-mode 1))

(use-package ivy-rich
  :after ivy
  :hook
  (ivy-mode
   . (lambda ()
       (if ivy-mode
           (ivy-rich-mode 1)
         (ivy-rich-mode -1))))
  :config
  (ivy-rich-mode 1))

(use-package magit
  :defer t
  :hook
  (magit-section-movement . magit-status-maybe-update-blob-buffer)
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package man
  :defer t)

(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'"       . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))

(progn ;     move
  ;; https://www.emacswiki.org/emacs/MoveLine
  ;; Modified with numeric prefix arg
  (defmacro save-column (&rest body)
    `(let ((column (current-column)))
       (unwind-protect
           (progn ,@body)
         (move-to-column column))))
  (put 'save-column 'lisp-indent-function 0)
  (defun move-line-up (n)
    "Move the current line up by N lines."
    (interactive "p")
    (save-column
      (dotimes (_ n)
        (transpose-lines 1)
        (forward-line -2))))
  (defun move-line-down (n)
    "Move the current line down by N lines."
    (interactive "p")
    (save-column
      (dotimes (_ n)
        (forward-line 1)
        (transpose-lines 1))
      (forward-line -1)))
  ;; https://www.emacswiki.org/emacs/MoveRegion
  ;; Modified to preserve point
  (defun move-region (start end n)
    "Move the current region up or down by N lines."
    (interactive "r\np")
    (let ((point-at-start (eql start (point)))
          (line-text (delete-and-extract-region start end)))
      (forward-line n)
      (let ((start (point)))
        (insert line-text)
        (setq deactivate-mark nil)
        (set-mark start)
        (when point-at-start
          (exchange-point-and-mark)))))
  (defun move-region-up (start end n)
    "Move the current line up by N lines."
    (interactive "r\np")
    (move-region start end (if (null n) -1 (- n))))
  (defun move-region-down (start end n)
    "Move the current line down by N lines."
    (interactive "r\np")
    (move-region start end (if (null n) 1 n)))
  ;; Key bindings for move region/line
  (defun move-up (n)
    "Move the current region/line up by N lines."
    (interactive "p")
    (if (use-region-p)
        (unless (region-noncontiguous-p)
          (move-region-up (region-beginning) (region-end) n))
      (move-line-up n)))
  (defun move-down (n)
    "Move the current region/line down by N lines."
    (interactive "p")
    (if (use-region-p)
        (unless (region-noncontiguous-p)
          (move-region-down (region-beginning) (region-end) n))
      (move-line-down n)))
  (global-set-key (kbd "C-S-p") 'move-up)
  (global-set-key (kbd "C-S-n") 'move-down))

(use-package multiple-cursors
  :bind
  (("M-RET"         . mc/edit-lines)
   ("C-<"           . mc/mark-previous-like-this)
   ("C->"           . mc/mark-next-like-this)
   ("C-M-<"         . mc/unmark-next-like-this)
   ("C-M->"         . mc/unmark-previous-like-this)
   ("C-c C-<"       . mc/mark-all-like-this)
   ("C-S-SPC"       . mc/toggle-cursor-at-point)
   ("<C-S-return>"  . multiple-cursors-mode)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click))
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
  (setq backup-directory-alist
        `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package paredit
  :hook
  (eval-expression-minibuffer-setup . paredit-mode)
  :bind
  (:map lisp-interaction-mode-map
        ("C-S-j" . eval-print-last-sexp)))

(use-package paren-face)

(use-package pdf-tools
  :defer t
  :init
  (pdf-tools-install))

(use-package pdf-view
  :after pdf-tools
  :bind
  (:map pdf-view-mode-map
        ("C-r" . isearch-backward)
        ("C-s" . isearch-forward))
  :hook
  (pdf-view-mode
   . (lambda ()
       (let ((oldmap (cdr (assoc 'ivy-mode minor-mode-map-alist)))
             (newmap (make-sparse-keymap)))
         (set-keymap-parent newmap oldmap)
         (define-key newmap (kbd "C-r") nil)
         (define-key newmap (kbd "C-s") nil)
         (push `(ivy-mode . ,newmap) minor-mode-overriding-map-alist)))))

(use-package queue)

(use-package recentf
  :demand t
  :config
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")
  (recentf-mode +1))

(use-package saveplace
  :config
  (save-place-mode +1))

(use-package savehist
  :config
  (savehist-mode +1))

(use-package smerge-mode
  :defer t
  :config
  (when (>= emacs-major-version 27)
    (set-face-attribute 'smerge-refined-removed nil :extend t)
    (set-face-attribute 'smerge-refined-added   nil :extend t)))

(use-package smex)

(use-package swiper
  :config
  (global-set-key (kbd "C-s")
                  (lambda ()
                    "Runs the command `swiper-isearch'.

With a prefix argument, run the command `swiper-all'."
                    (interactive)
                    (if current-prefix-arg
                        (swiper-all)
                      (swiper-isearch))))
  (global-set-key (kbd "C-r") (global-key-binding (kbd "C-s"))))

(progn ;     themes
  (defadvice load-theme (before theme-dont-propagate activate)
    (mapc 'disable-theme custom-enabled-themes)))

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

(use-package undo-tree)

(progn ;     unfill
  ;; https://www.emacswiki.org/emacs/UnfillParagraph
  ;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
  (defun unfill-paragraph (&optional region)
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
          ;; This would override `fill-column' if it's an integer.
          (emacs-lisp-docstring-fill-column t))
      (fill-paragraph nil region))))

(use-package vterm
  :defer t
  :bind
  ("C-x m" . vterm)
  :config
  (require 'with-editor)
  (add-hook 'vterm-mode-hook 'with-editor-export-editor)
  (define-advice counsel-yank-pop (:around (fun &rest args))
    (if (equal major-mode 'vterm-mode)
        (let ((counsel-yank-pop-action-fun (symbol-function
                                            'counsel-yank-pop-action))
              (last-command-yank-p (eq last-command 'yank)))
          (cl-letf (((symbol-function 'counsel-yank-pop-action)
                     (lambda (s)
                       (let ((inhibit-read-only t)
                             (last-command (if (memq last-command
                                                     '(counsel-yank-pop
                                                       ivy-previous-line
                                                       ivy-next-line))
                                               'yank
                                             last-command))
                             (yank-undo-function (when last-command-yank-p
                                                   (lambda (_start _end)
                                                     (vterm-undo)))))
                         (cl-letf (((symbol-function 'insert-for-yank)
                                    'vterm-insert))
                           (funcall counsel-yank-pop-action-fun s))))))
            (apply fun args)))
      (apply fun args))))

(use-package which-key
  :bind
  (("C-h b"     . #'which-key-show-full-major-mode)
   ("C-h C-b"   . #'which-key-show-full-minor-mode-keymap)
   ("C-h C-M-b" . #'which-key-show-top-level))
  :config
  (which-key-mode +1))

(use-package whitespace
  :hook
  (nxml-mode . whitespace-mode))

(use-package ws-butler
  :hook
  (nxml-mode . ws-butler-mode))

(use-package tramp-sh
  :defer t
  :config (cl-pushnew 'tramp-own-remote-path tramp-remote-path))

;;; Tequila worms

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
;; no-byte-compile: t
;; End:
;;; init.el ends here
