(use-package projectile
  :ensure t
  :init (projectile-mode 1)
  :config
  (setq projectile-sort-order 'recentf)
  (setq projectile-project-search-path '("~/repos/" "~/github/"))
  )

(use-package counsel-projectile
  :ensure t
  :bind (("C-M-o" . counsel-projectile-switch-project)
		 ("C-p" . counsel-projectile-find-file)
		 ("C-M-p" . counsel-projectile-find-dir)
		 ("C-S-f" . counsel-projectile-ag))
  :config (setq projectile-completion-system 'default))
