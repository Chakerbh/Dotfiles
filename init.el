(require 'package)

;; (require 'lb-datalog-mode)

(setq package-archives
      '(
        ;;("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("org"       . "http://orgmode.org/elpa/")
       ; ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(
      ; ("MELPA Stable" . 10)
        ;;("GNU ELPA"     . 5)
        ("ORG"          .  4)
        ("MELPA"        . 0)))

(package-initialize)

(require 'dash)

(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Scratch")
(setq visible-bell t)             ;; Get rid of the beeps

(menu-bar-mode 0)             ;; Disable bar mode
(scroll-bar-mode -1)            ;; Scrollbars are waste screen estate

(defun ch/emacs-subdirectory (d) (expand-file-name d "/home/chaker/.emacs.d/"))

(package-initialize)

;; Enable undo tree for all buffers
(global-undo-tree-mode)



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


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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

(use-package projectile
  :ensure t
  :init (projectile-mode 0)
  :config
  (defun my/add-projects (base)
    "Add all sub-directories of `base` to the projectile known projects"
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name) 
                   (not (equal f ".."))
                   (not (equal f ".")))
          (projectile-add-known-project name)))))

  (mapc (lambda (b) (my/add-projects b)) '("/home/chaker/work" "/home/chaker/bitbucket" "/home/chaker/git")))

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
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  :bind ("C-:" . company-complete)  ; In case I don't want to wait
  :diminish company-mode)

(use-package anaconda-mode
  :ensure t
  :commands anaconda-mode
  :after company
  :diminish anaconda-mode
  :init
  (progn
  (add-to-list 'company-backends 'company-anaconda)
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'eldoc-mode)))

(use-package company-quickhelp
  :defer 2
  :ensure t
  :config
  (company-quickhelp-mode 1))

(use-package flycheck
  :defer 3
  :ensure t
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
  (setq ispell-local-dictionary "en_US"
        ispell-dictionary "american" ; better for aspell
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")
        ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "['‘’]"
                                      t ; Many other characters
                                      ("-d" "en_US") nil utf-8))))
(org-babel-do-load-languages
 'org-babel-load-languages '((sh . t)))

;; Line numbers
(use-package linum
    :init
    (add-hook 'prog-mode-hook 'linum-mode)
    (add-hook 'linum-mode-hook (lambda () (set-face-attribute 'linum nil :height 110)))
    )

;; Syntax highlight org-mode source blocks.
(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  :config
  ;; (setq org-src-fontify-natively t)
  ;; (progn
  ;; (let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
  ;;                              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
  ;;                              ((x-list-fonts "Verdana")         '(:font "Verdana"))
  ;;                              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
  ;;                              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
  ;;        (base-font-color     (face-foreground 'default nil 'default))
  ;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  ;;   (custom-theme-set-faces 'user
  ;;                           `(org-level-8 ((t (,@headline ,@variable-tuple))))
  ;;                           `(org-level-7 ((t (,@headline ,@variable-tuple))))
  ;;                           `(org-level-6 ((t (,@headline ,@variable-tuple))))
  ;;                           `(org-level-5 ((t (,@headline ,@variable-tuple))))
  ;;                           `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
  ;;                           `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
  ;;                           `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
  ;;                           `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
  ;;                           `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))
  ;; )
  )


(use-package json-mode
  :ensure t )

(use-package nix-mode
  :ensure t
  :mode ("\\.nix\\'")
  )

(use-package flycheck
  :defer 1
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

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
  :config
  (evil-mode 1)

  (evil-define-key 'normal global-map (kbd "C-p")     'counsel-projectile-find-file)
  (evil-define-key 'normal global-map (kbd "C-S-p")   'counsel-projectile-switch-project)
  (evil-define-key 'normal global-map (kbd "z d")     'dictionary-lookup-definition)
  )

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
  (evil-leader/set-leader ",")
  (evil-leader/set-key
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

(use-package monky
  :defer 1
  :ensure t
  ;; I don't need C-x m for mails :/ 
  :bind (("C-x m" . monky-status)))

(use-package magit
  :defer 1
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package emacs-ycmd
:after company-mode
 :ensure company-ycmd
 :config
 (company-ycmd-setup)
 (set-variable 'ycmd-server-command '("python", "/nix/store/gfvfpshxl3bss2mhpzkqrj0kpq16g2j1-ycmd-2017-11-05/bin/ycmd"))
 )
