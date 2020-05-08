(require 'package)

;; (require 'lb-datalog-mode)

(setq package-archives
      '(
        ("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("org"       . "http://orgmode.org/elpa/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(
       ("MELPA Stable" . 10)
       ("GNU ELPA"     . 5)
        ("ORG"          .  4)
        ("MELPA"        . 0)))

(package-initialize)

(defun clean-elc-files ()
  (shell-command "/bin/rm ~/.emacs.d/*.elc")
  (shell-command "/bin/rm ~/.emacs.d/config/*.elc"))
; (clean-elc-files)

(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Scratch")
(setq visible-bell t)             ;; Get rid of the beeps

(toggle-frame-fullscreen)
(menu-bar-mode 0)             ;; Disable bar mode
(scroll-bar-mode -1)            ;; Scrollbars are waste screen estate
(tool-bar-mode -1)            ;; Scrollbars are waste screen estate

(defun ch/emacs-subdirectory (d) (expand-file-name d "/home/chaker/.emacs.d/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(load "~/.emacs.d/config/project.el")

;; Enable undo tree for all buffers
(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; First, let’s increase the cache before starting garbage collection:
(setq gc-cons-threshold 100000000)


;; Always use spaces, and 4 spaces par tab :).
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq-default tab-always-indent 'complete)

;; Does anyone type yes anymore?
(fset 'yes-or-no-p 'y-or-n-p)

(set-face-background 'region "blue3")

(setq vc-make-backup-files t)
(defun save-all ()
  "Save all dirty buffers without asking for confirmation."
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

(require 'use-package)

(use-package hungry-delete
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function global-hungry-delete-mode "hungry-delete.el"))
  :config
  (global-hungry-delete-mode t)
  )

(use-package julia-mode
  :ensure t
  )
(use-package julia-repl
  :ensure t
  :hook (julia-mode . julia-repl-mode)
)

;; Very handy!!
(use-package which-key
  :defer 2
  :ensure t
  :diminish ""
  :config
  (which-key-mode t))

(use-package ace-window
  :defer 1
  :ensure t
  :init
    (setq aw-keys '(?q ?s ?d ?f ?j ?k ?l ?o))
    (global-set-key (kbd "C-x o") 'ace-window)
  :diminish ace-window-mode)

(use-package avy
  :ensure t
  :init (setq avy-background t))

(use-package winner
  :defer 1
  :ensure t
  :init (winner-mode 1))

(use-package swiper
  :ensure t
  :config
    (setq ivy-re-builders-alist
        '((t . ivy--regex-plus)))
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    ;; (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)

    ;; Backward search in minibuffer
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
    )

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize)
  )

(use-package hl-line
  :ensure t
  :config
  )

(use-package ag
  :ensure    t
  :commands  ag
  :init      (setq ag-highlight-search t)
  :config    (add-to-list 'ag-arguments "--word-regexp"))

;; Save backup files to one directory.
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 ;; TODO: Create this file if it doesn't exist
                (ch/emacs-subdirectory "backups")))))

(use-package tramp
  :defer t
  :config (setq tramp-backup-directory-alist backup-directory-alist)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; Enable "sudo su user" over ssh.
  (add-to-list 'tramp-methods
               '("ss"
                 (tramp-login-program "sudo")
                 (tramp-login-args
                  (("su")
                   ("-")
                   ("%u")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-login
                  ("-l"))
                 (tramp-remote-shell-args
                  ("-c"))
                 (tramp-connection-timeout 10)))
  )


;; Auto complete
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind ("C-:" . company-complete)  ; In case I don't want to wait
  :diminish company-mode)

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))

(use-package flycheck
  :defer 1
  :init (global-flycheck-mode))

(use-package flycheck-julia
  :ensure t
  :after flycheck
  )

(use-package flyspell
  :diminish flyspell-mode
  :after flycheck
  :defer 3
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)

  (dolist (hook '(text-mode-hook org-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))

  (dolist (hook '(change-log-mode-hook log-edit-mode-hook org-agenda-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))

  :config
  (setq
        ispell-program-name "aspell" ; better for aspell
                                      ))
(org-babel-do-load-languages
 'org-babel-load-languages '((shell . t)))

;; Line numbers
(use-package linum
  :ensure t
    )

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  :config
  )


(use-package json-mode
  :ensure t )

(use-package nix-mode
  :ensure t
  :mode ("\\.nix\\'")
  )

(use-package eldoc
  :defer 1
  :ensure t
  :diminish eldoc-mode
  :init  (setq eldoc-idle-delay 0.1))

(use-package dictionary
  :ensure t
  ;; Probably we won't need this to load until later on, in the session
  :defer 2)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)

  (evil-define-key 'normal global-map (kbd "C-p")     'counsel-projectile-find-file)
  (evil-define-key 'normal global-map (kbd "C-S-p")   'counsel-projectile-switch-project)
  (evil-define-key 'normal global-map (kbd "z d")     'dictionary-lookup-definition)
  )

(use-package evil-escape
  :ensure t
  :config
  (progn
    (evil-escape-mode)
    (setq-default evil-escape-key-sequence "jk")))

(use-package evil-collection
  :after evil
  :ensure t
  ;; :defer t
  :init
  (evil-collection-init))

(use-package evil-leader
  :ensure t
  :after evil
  :config
  (global-evil-leader-mode)
  "Configure evil leader mode."
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    ","  'other-window
    "<SPC>" 'counsel-projectile-find-file 
    ","  'other-window
    "b"  'ivy-switch-buffer             ;; Switch to another buffer
    "d"  'kill-this-buffer
    "l"  'whitespace-mode       ;; Show invisible characters
    "o"  'delete-other-windows  ;; C-w o, a Handy shortcut :)
    "p"  'helm-show-kill-ring
    "t"  'projectile-test-project
    "s"  'counsel-ag            ;; Ag search from the directory that contain this buffer.
    "f"  'counsel-projectile-ag ;; Ag search from the root of the project.
    "S"  'delete-trailing-whitespace
    "w"  'save-buffer
    "x"  'counsel-M-x
    )
  )

(use-package evil-surround
  :ensure t
  :after evil
  :defer 1
  :config
  (global-evil-surround-mode))

(use-package evil-commentary
  :ensure t
  :defer 1
  :after evil
  :config
  (evil-commentary-mode)
  )

(use-package paren
  :ensure nil
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode +1))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(package-selected-packages
   (quote
    (evil-commentary evil-surround evil-leader evil-collection evil-escape evil undo-tree which-key use-package org-plus-contrib nix-mode magit julia-repl julia-mode json-mode hungry-delete flycheck-julia exec-path-from-shell dictionary counsel-projectile company-quickhelp ag ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
