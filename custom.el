(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-width 80)
 '(auto-compile-display-buffer nil)
 '(auto-compile-mode-line-counter t)
 '(auto-compile-source-recreate-deletes-dest t)
 '(auto-compile-toggle-deletes-nonlib-dest t)
 '(auto-compile-update-autoloads t)
 '(avy-background t)
 '(aw-ignored-buffers
   '("*Calc Trail*" " *LV*" "*Message*" "*Messages*" "*Warnings*"))
 '(aw-keys '(97 115 100 102 103 104 106 107 108))
 '(aw-scope 'frame)
 '(column-number-mode t)
 '(company-minimum-prefix-length 2)
 '(company-show-numbers t)
 '(company-tooltip-flip-when-above t)
 '(completion-styles '(basic partial-completion emacs22 initials))
 '(confirm-kill-emacs 'y-or-n-p)
 '(create-lockfiles nil)
 '(default-text-scale-mode t nil (default-text-scale))
 '(delete-old-versions t)
 '(diff-hl-draw-borders nil)
 '(dired-listing-switches "-alh")
 '(emacs-lisp-mode-hook '(paredit-mode reveal-mode outline-minor-mode))
 '(enable-recursive-minibuffers t)
 '(epg-pinentry-mode 'loopback)
 '(erc-autojoin-channels-alist '(("freenode.net" "#emacs")))
 '(erc-autojoin-timing 'ident)
 '(erc-hide-list '("JOIN" "PART" "QUIT"))
 '(erc-kill-buffer-on-part t)
 '(erc-kill-queries-on-quit t)
 '(erc-kill-server-buffer-on-quit t)
 '(erc-prompt-for-nickserv-password nil)
 '(erc-prompt-for-password nil)
 '(erc-rename-buffers t)
 '(erc-server "chat.freenode.net")
 '(erc-server-connect-function 'erc-open-tls-stream)
 '(erc-services-mode t)
 '(global-dash-fontify-mode t)
 '(global-diff-hl-mode t)
 '(global-eldoc-mode t)
 '(global-hl-todo-mode t)
 '(global-paren-face-mode t)
 '(global-prettify-symbols-mode t)
 '(global-subword-mode t)
 '(global-undo-tree-mode t)
 '(ielm-mode-hook '(paredit-mode))
 '(imenu-auto-rescan t)
 '(imenu-max-item-length nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(initial-scratch-message nil)
 '(isearch-allow-scroll t)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-posframe-border-width 2)
 '(ivy-posframe-parameters '((left-fringe . 6) (right-fringe . 6)))
 '(ivy-use-selectable-prompt t)
 '(ivy-use-virtual-buffers t)
 '(ivy-wrap t)
 '(kept-new-versions 6)
 '(lisp-mode-hook '(paredit-mode))
 '(magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1)
 '(magit-post-refresh-hook '(diff-hl-magit-post-refresh))
 '(marginalia-mode t)
 '(menu-bar-mode nil)
 '(native-comp-async-report-warnings-errors 'silent)
 '(pdf-view-use-unicode-ligther nil)
 '(prog-mode-hook
   '((lambda nil
       (setq indicate-buffer-boundaries 'left))
     ws-butler-mode whitespace-mode display-line-numbers-mode company-mode))
 '(save-abbrevs 'silently)
 '(save-interprogram-paste-before-kill t)
 '(scroll-bar-mode nil)
 '(set-mark-command-repeat-pop t)
 '(show-paren-mode t)
 '(tab-always-indent 'complete)
 '(temp-buffer-resize-mode t)
 '(text-mode-hook '((lambda nil (setq indicate-buffer-boundaries 'left))))
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(use-dialog-box nil)
 '(vc-follow-symlinks t)
 '(version-control t)
 '(vertico-cycle t)
 '(vertico-resize t)
 '(visible-bell t)
 '(vterm-always-compile-module t)
 '(vterm-buffer-name-string "vterm %s")
 '(vterm-keymap-exceptions
   '("C-c" "C-x" "C-u" "C-g" "C-h" "C-l" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y" "M-O"))
 '(vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
 '(which-key-mode t)
 '(whitespace-style '(face trailing tabs lines-tail empty))
 '(winner-dont-bind-my-keys t)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka")))))
