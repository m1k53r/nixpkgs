;;; init -- My config
;;; Commentary:
;;; masny ben
;;; Code:

(unless (server-running-p) (server-start))

;; Enable Evil
(use-package evil
  :init
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

;; Load a custom theme
(load-theme 'darkburn t)

;; Disable menu bar
(menu-bar-mode -1)

;; Disable scrollbar
(toggle-scroll-bar -1)
(scroll-bar-mode -1)

;; Disable toolbar
(tool-bar-mode -1)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(setq electric-pair-preserve-balance nil)
(setq display-line-numbers-type 'relative)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Font
(set-frame-font "JetBrains Mono Nerd Font 13" nil t)

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
  (setq company-idle-delay 0
        company-minimum-prefix-length 4
        company-selection-wrap-around t))
(global-company-mode)

;; LSP

(use-package lsp-haskell)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (nix-mode . lsp)
         (rust-mode . lsp)
         (haskell-mode . lsp)
         (python-mode . lsp)
         (typescript-mode . lsp)
         (javascript-mode . lsp)
         (web-mode . lsp)
         (c++-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)

(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))

(use-package haskell-mode
  :mode ("\\.hs\\'")
  :config
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  :hook
  (haskell-mode . turn-on-haskell-unicode-input-method)
  (haskell-mode . haskell-auto-insert-module-template)
  (haskell-mode . haskell-collapse-mode))

(use-package rust-mode)

(use-package company-capf)

(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'left-fringe))

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
  :mode (("\\.tsx\\'" . web-mode))
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
  (centaur-tabs-selected
   ((t (:background "#3F3F3F" :foreground "#DCDCCC"))))
  (centaur-tabs-unselected
   ((t (:background "#2B2B2B" :foreground "#656555"))))
  (centaur-tabs-selected-modified
   ((t (:background "#3F3F3F" :foreground "#F0DFAF"))))
  (centaur-tabs-unselected-modified
   ((t (:background "#2B2B2B" :foreground "#F0DFAF"))))
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

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

(use-package emmet-mode
  :hook (web-mode . emmet-mode))

(use-package sublimity
  :config
  (require 'sublimity-scroll)
  (require 'sublimity-map)
  (require 'sublimity-attractive)
  (sublimity-attractive-hide-vertical-border)
  (sublimity-attractive-hide-fringes)
  :hook (prog-mode . sublimity-mode))

(add-hook 'prog-mode-hook 'hl-line-mode)

(require 'dired-x)

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 4)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  :bind
  ("s-`" . eshell-toggle))

(use-package indent-guide
  :hook
  (prog-mode . indent-guide-mode))

(use-package dimmer
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-helm)
  (dimmer-configure-company-box)
  (dimmer-configure-magit)
  (dimmer-mode t))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package evil-nerd-commenter
  :config
  (evilnc-default-hotkeys))

(use-package dashboard
  :config
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5)))
  (dashboard-setup-startup-hook))

(use-package page-break-lines
  :config
  (setq dashboard-banner-logo-title "Welcome back to the planning center!")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  :hook
  (prog-mode . page-break-lines-mode))

(use-package git-gutter
  :hook
  (prog-mode . git-gutter-mode))

(use-package git-gutter-fringe
  :config
  (setq git-gutter-fr:side 'right-fringe))

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package smart-mode-line
  :init
  (sml/setup))

(use-package org
  :mode (("\\.org\\'" . org-mode))
  :config
  (setq org-startup-indented t
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-startup-with-inline-images t
        org-image-actual-width '(300))
  (text-scale-increase 2)
  (plist-put org-format-latex-options :scale 2)
  :hook
  (org-mode . (lambda () (sublimity-mode 0)))
  (org-mode . (lambda () (setq-default line-spacing 6))))

(use-package olivetti
  :config
  (setq olivetti-style fancy)
  :hook
  (org-mode . olivetti-mode))

(use-package org-roam
  :ensure t
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
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(let ((org-super-agenda-groups
       '(;; Each group has an implicit boolean OR operator between its selectors.
         (:name "Today"  ; Optionally specify section name
                :time-grid t  ; Items that appear on the time grid
                :todo "TODAY")  ; Items that have this TODO keyword
         (:name "Important"
                ;; Single arguments given alone
                :tag "bills"
                :priority "A")
         ;; Set order of multiple groups at once
         (:order-multi (2 (:name "Shopping in town"
                                 ;; Boolean AND group matches items that match all subgroups
                                 :and (:tag "shopping" :tag "@town"))
                          (:name "Food-related"
                                 ;; Multiple args given in list with implicit OR
                                 :tag ("food" "dinner"))
                          (:name "Personal"
                                 :habit t
                                 :tag "personal")
                          (:name "Space-related (non-moon-or-planet-related)"
                                 ;; Regexps match case-insensitively on the entire entry
                                 :and (:regexp ("space" "NASA")
                                               ;; Boolean NOT also has implicit OR between selectors
                                               :not (:regexp "moon" :tag "planet")))))
         ;; Groups supply their own section names when none are given
         (:todo "WAITING" :order 8)  ; Set order of this section
         (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                ;; Show this group at the end of the agenda (since it has the
                ;; highest number). If you specified this group last, items
                ;; with these todo keywords that e.g. have priority A would be
                ;; displayed in that group instead, because items are grouped
                ;; out in the order the groups are listed.
                :order 9)
         (:priority<= "B"
                      ;; Show this section after "Today" and "Important", because
                      ;; their order is unspecified, defaulting to 0. Sections
                      ;; are displayed lowest-number-first.
                      :order 1)
         ;; After the last group, the agenda will display items that didn't
         ;; match any of these groups, with the default order position of 99
         ))))

(add-hook 'prog-mode-hook 'cmake-ide-setup)

(provide 'init)
;;; init.el ends here
