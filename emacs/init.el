;;; init -- My config
;;; Commentary:
;;; masny ben
;;; Code:

;; Enable Evil
(use-package evil
  :init
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))
;; Load a custom theme
;;(load-theme 'darkburn t)
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (defun my/theme-init-daemon (frame)
                (with-selected-frame frame
                  (load-theme 'nord t))
                ;; Run this hook only once.
                (remove-hook 'after-make-frame-functions
                             #'my/theme-init-daemon)
                (fmakunbound 'my/theme-init-daemon)))
  (load-theme 'nord t))
(winner-mode 1)

(setq backup-directory-alist '((".*" . "~/.backup")))

;; Disable menu bar
(menu-bar-mode -1)

(put 'dired-find-alternate-file 'disabled nil)

;; Disable scrollbar
(toggle-scroll-bar -1)
(scroll-bar-mode -1)
   
;; Disable toolbar
(tool-bar-mode -1)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(eshell)
(setq electric-pair-preserve-balance nil)
(setq display-line-numbers-type 'relative)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Font
;;(set-frame-font "JetBrains Mono Nerd Font 13" nil t)
(add-to-list 'default-frame-alist '(font . "JetBrains Mono Nerd Font-13"))

;; Spaces over tabs
(setq-default indent-tabs-mode nil)

;; Set indentation level
(setq-default tab-width 2)

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)

(use-package typescript-mode
  :config
  (setq typescript-indent-level 2))

;; Helm configuration
(use-package helm
  :config
  (require 'helm-config)
  :init
  (helm-mode 1)
  :bind
  (("M-x"     . helm-M-x) ;; Evaluate functions
   ("C-x C-f" . helm-find-files) ;; Open or create files
   ("C-x b"   . helm-mini) ;; Select buffers
   ("C-x C-r" . helm-recentf) ;; Select recently saved files
   ("C-c i"   . helm-imenu) ;; Select document heading
   ("M-y"     . helm-show-kill-ring) ;; Show the kill ring
   :map helm-map
   ("C-z" . helm-select-action)
   ("<tab>" . helm-execute-persistent-action)))

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.5)
  (which-key-setup-side-window-bottom))

;; Auto completion
(use-package company
  :config
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-selection-wrap-around t))
(global-company-mode)

;; LSP
(use-package lsp-haskell)

(use-package lsp-mode
  :init
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (nix-mode . lsp)
         (haskell-mode . lsp)
         (python-mode . lsp)
         (typescript-mode . lsp)
         (javascript-mode . lsp)
         (web-mode . lsp)
         (c++-mode . lsp)
         (dart-mode . lsp)
         (elixir-mode . lsp)
         (tuareg-mode . lsp)
         (rustic-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-ui-mode))
  :commands lsp)

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))

(use-package elixir-mode
  :mode ("\\.ex\\'" "\\.exs\\'"))

(use-package haskell-mode
  :mode ("\\.hs\\'")
  :config
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  :hook
  (haskell-mode . haskell-collapse-mode))

(use-package company-capf)

(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'left-fringe)
  :hook
  (c++-mode . (lambda () (setq flycheck-clang-language-standard "c++11"))))

(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package lsp-python-ms
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp)))
  :init
  (setq lsp-python-ms-executable (executable-find "python-language-server")))

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package web-mode
  :mode
  (("\\.tsx\\'" . web-mode)
   ("\\.tpl\\'" . web-mode))
  :hook ((web-mode . (lambda ()
                       (when (string-equal "tsx" (file-name-extension buffer-file-name))
                         (tide-setup)))))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)
  (centaur-tabs-change-fonts "JetBrains Mono Nerd Font" 120)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-style "bar")
  :custom-face
  (centaur-tabs-default
   ((t (:background "#4C566A" :foreground "#ECEFF4"))))
  ;; (centaur-tabs-selected
  ;;  ((t (:background "#3F3F3F" :foreground "#DCDCCC"))))
  ;; (centaur-tabs-unselected
  ;;  ((t (:background "#2B2B2B" :foreground "#656555"))))
  ;; (centaur-tabs-selected-modified
  ;;  ((t (:background "#3F3F3F" :foreground "#F0DFAF"))))
  ;; (centaur-tabs-unselected-modified
  ;;  ((t (:background "#2B2B2B" :foreground "#F0DFAF"))))
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  (:map evil-normal-state-map
        ("g t" . centaur-tabs-forward)
        ("g T" . centaur-tabs-backward)))

(use-package all-the-icons-dired
  ;; :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (setq all-the-icons-dired-monochrome nil))

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(add-hook 'prog-mode-hook #'(lambda () (hs-minor-mode t)))

(use-package undo-fu)

(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; 
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;;If you use evil
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(show-paren-mode 1)

(use-package yasnippet
  :init
  (yas-reload-all)
  :hook (prog-mode . yas-minor-mode))

(use-package sublimity
  :config
  (require 'sublimity-scroll)
  (require 'sublimity-attractive)
  (sublimity-attractive-hide-vertical-border)
  (sublimity-attractive-hide-fringes)
  :hook (prog-mode . sublimity-mode))

(add-hook 'prog-mode-hook 'hl-line-mode)

(require 'dired-x)

(use-package indent-guide
  :hook
  (prog-mode . indent-guide-mode))

(use-package dimmer
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-helm)
  (dimmer-configure-company-box)
  (dimmer-configure-magit)
  (setq dimmer-use-colorspace :rgba)
  (dimmer-mode t))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package evil-nerd-commenter
  :config
  (evilnc-default-hotkeys))

(use-package git-gutter
  :hook
  (prog-mode . git-gutter-mode))

(use-package git-gutter-fringe
  :config
  (setq git-gutter-fr:side 'left-fringe))

(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  (sml/setup))

(use-package org
  :mode (("\\.org\\'" . org-mode))
  :config
  (setq org-startup-indented t
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-startup-with-inline-images t
        org-image-actual-width '(300))
  ;;(text-scale-increase 2)
  ;;(plist-put org-format-latex-options :scale 2)
  :hook
  (org-mode . (lambda () (sublimity-mode 0)))
  (org-mode . (lambda () (setq-default line-spacing 6))))

(use-package olivetti
  :hook
  (org-mode . olivetti-mode))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "/home/mikolaj/notes/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package deft
  :config
  (setq deft-directory "/home/mikolaj/notes/"
        deft-recursive t
        deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
        deft-use-filename-as-title t)
  :bind
  ("C-c n d" . deft))

(setq org-roam-capture-templates '(("d" "default" plain "%?"
                                    :if-new
                                    (file+head "${slug}.org"
                                               "#+title: ${title}\n#+date: %u\n#+lastmod: \n\n")
                                    :immediate-finish t))
      time-stamp-start "#\\+lastmod: [\t]*")

(use-package org-download
  :config
  (setq-default org-download-image-dir "~/notes/img"))

(use-package typo
  :hook
  (org-mode . typo-mode))

(use-package org-beautify-theme)

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package shell-switcher
  :config
  (shell-switcher-mode t))

(use-package shackle
  :config
  (progn
    (setq shackle-lighter "")
    (setq shackle-select-reused-windows nil) ; default nil
    (setq shackle-default-alignment 'right) ; default below
    
    (setq shackle-rules
          '(
            ("*shell*"  :select t :size 0.4 :align right :popup t)
            ))
    (shackle-mode 1))
  (provide 'setup-shackle))

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :hook (popper-open-popup . centaur-tabs-local-mode)
  :init
  (setq popper-reference-buffers
      '("\\*Messages\\*"
        "Output\\*$"
        "\\*Async Shell Command\\*"
        help-mode
        compilation-mode
        "\\*eshell.*\\*$"
        eshell-mode))
  (setq popper-mode-line nil)
  (popper-mode +1)
  (popper-echo-mode +1))

;; Assuming usage with dart-mode
(use-package dart-mode
  :hook
  (dart-mode . (lambda () (indent-guide-mode 0)))
  :custom
  (dart-sdk-path (concat (getenv "HOME") "/flutter/bin/cache/dark-sdk/")
   dart-format-on-save t))

(use-package lsp-treemacs
  :config
  (setq lsp-treemacs-symbols-position-params `((side . right)
                                               (slot . 2)
                                               (window-width . 35)))
  (lsp-treemacs-sync-mode 1))

(use-package treemacs)

(use-package merlin
  :config
  (custom-set-variables '(merlin-command "ocamlmerlin"))
  :hook
  (tuareg-mode . merlin-mode))

(use-package ocp-indent)

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(load "server")
(unless (server-running-p) (server-start))

(provide 'init)
;;; init.el ends here
