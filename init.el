(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d2e0c53dbc47b35815315fae5f352afd2c56fa8e69752090990563200daae434" default))
 '(initial-frame-alist '((fullscreen . maximized)))
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(org-tempo visual-fill-column org-bullets forge evil-magit magit projectile hydra general ivy-rich rainbow-delimiters markdown-mode evil-collection ivy-prescient prescient doom-modeline yaml-mode counsel ivy which-key darktooth-theme key-chord evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'use-package)
(setq use-package-always-ensure t)

(use-package which-key
  :config
  (which-key-mode))

;; TODO: Set up more keys!
(use-package general
  :config
  (general-create-definer crw/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(use-package hydra)

;; font size
(set-face-attribute 'default nil :font "Hack" :height 220)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "ETBembo" :height 220)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))
(crw/leader-keys
  "s" '(hydra-text-scale/body :which-key "scale text"))

(load-theme 'darktooth)

;; Annoying
(setq ring-bell-function 'ignore)

(scroll-bar-mode -1)

;; TODO: Previous line doesn't work.
;; TODO: A better indicator? > instead of highlight?
;; TODO: Emacs bindings don't quite work and fail at startup.
(use-package counsel
  ;;:bind (("C-s" . swiper)
  ;;  :map ivy-minibuffer-map
  ;;  ("TAB" . ivy-alt-done)	
  ;;  ("C-l" . ivy-alt-done)
  ;;  ("C-j" . ivy-next-line)
  ;;  ("C-k" . ivy-previous-line-or-history)
  ;;  :map ivy-switch-buffer-map
  ;;  ("C-k" . ivy-previous-line)
  ;;  ("C-l" . ivy-done)
  ;;  ("C-d" . ivy-switch-buffer-kill)
  ;;  :map ivy-reverse-i-search-map
  ;;  ("C-k" . ivy-previous-line)
  ;;  ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy–regex-fuzzy t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(global-set-key [remap org-set-tags-command] #'counsel-org-tag)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; nice sorting
(use-package prescient)
(use-package ivy-prescient
  :config
  (ivy-prescient-mode 1))

;; evil mode muahaha
(use-package evil
  :init
  (setq evil-want-minibuffer t)
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Vim jk escape
(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map  "jk" 'evil-normal-state))

(column-number-mode)
(global-display-line-numbers-mode)
;; (setq display-line-numbers 'relative)
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; autocomplete paired brackets
(electric-pair-mode 1)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom (projectile-completion-system 'ivy)
  ;; :bind-keymap
  ;; ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'magit-status))

(crw/leader-keys
  "p" 'projectile-command-map)

(use-package magit)

(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

(crw/leader-keys
  "g" 'magit)

;; TODO: Authenticate
(use-package forge)

(setq explicit-shell-file-name "/usr/local/bin/zsh")

(crw/leader-keys
  "t" 'term)

(load-file "~/.emacs.d/bazel/bazel.el")
(add-to-list 'auto-mode-alist '("\\.star\\'" . bazel-starlark-mode))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package terraform-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode)))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package lsp-mode
      :commands (lsp lsp-deferred)
      :hook (lsp-mode . efs/lsp-mode-setup)
      :init
      (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
      :config
      (lsp-enable-which-key-integration t))

(crw/leader-keys
  "l" 'lsp-command-map)

(setq user-init-file "~/.emacs.d/init.el")
(defun open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))
(crw/leader-keys
  "i" 'open-init-file)

;; TODO: remove underline
(use-package org
  :hook (org-mode . crw/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t)

  (setq org-agenda-start-with-log-mode t)
  ;; Filter out agenda prefix and tags.
  (setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " ")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))
  (setq org-agenda-hide-tags-regexp ".")
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  
  ;;(setq org-capture-templates '(("t" "Todo [inbox]" entry
  ;;                             (file+headline "~/gtd/inbox.org" "Tasks")
  ;;                             "* TODO %i%?")))

  (setq org-refile-targets '(("~/org/projects.org" :maxlevel . 3)
			     ("~/org/someday.org" :level . 1)
                             ("~/org/tickler.org" :maxlevel . 2)))

  (setq org-agenda-files '("~/org/inbox.org"
  			   "~/org/projects.org"
                           "~/org/tickler.org"))
  (setq org-directory "~/org")

  (setq org-capture-templates
	`(("i" "Inbox" entry (file "inbox.org")
	   , (concat "* TODO %?\n"
		     "/Entered on/ %U"))))

  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-agenda-custom-commands 
      '(("w" "Work-related tasks" tags-todo "@work"
         ((org-agenda-overriding-header "Work")))
  	("h" "Personal tasks" tags-todo "@home"
         ((org-agenda-overriding-header "Home")))
  	))
  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)))

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (crw/org-font-setup))

;; TODO: Some of this doesn't work?
(defun crw/org-font-setup ()
    ;; Replace list hyphen with dot
    (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-]\\) "
                                (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
    ;; TODO: Different sizes not working?
    ;; Set faces for heading levels
    ;; (dolist (face '((org-level-1 . 1.2)
    ;;                 (org-level-2 . 1.1)
    ;;                 (org-level-3 . 1.05)
    ;;                 (org-level-4 . 1.0)
    ;;                 (org-level-5 . 1.1)
    ;;                 (org-level-6 . 1.1)
    ;;                 (org-level-7 . 1.1)
    ;;                 (org-level-8 . 1.1)))
    ;;   (set-face-attribute (car face) nil :font "ETBembo" :weight 'regular :height (cdr face)))

    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    ;;(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    ;;(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
    ;;(set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
    ;;(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    ;;(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    ;;(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    ;;(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
)

;; TODO: variable pitch breaks indent
(defun crw/org-mode-setup ()
    (org-indent-mode)
    ;;(variable-pitch-mode 1)
    (visual-line-mode 1))

  (use-package org-bullets
    :after org
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

  (defun efs/org-mode-visual-fill ()
    (setq visual-fill-column-width 100
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

  (use-package visual-fill-column
    :hook (org-mode . efs/org-mode-visual-fill))

(org-babel-do-load-languages
 'org-babel-load-languages
           '((emacs-lisp . t)
             (python . t)))

       (push '("conf-unix" . conf-unix) org-src-lang-modes)

     (require 'org-tempo)

     (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
     (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
     (add-to-list 'org-structure-template-alist '("py" . "src python"))

(defhydra hydra-org-tools (:timeout 4)
  "org tools"
  ("a" org-agenda "agenda":exit t)
  ("c" org-capture "capture":exit t))
(crw/leader-keys
  "o" '(hydra-org-tools/body :which-key "org tools"))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/projects/misc-configs/emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))


