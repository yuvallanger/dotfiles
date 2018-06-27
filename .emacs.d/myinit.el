
(add-to-list 'auto-mode-alist '("\\.ipy\\'" . python-mode))

(use-package better-shell
  :bind
  (
   ("C-'" . better-shell-shell)
   ("C-;" . better-shell-remote-open)
   )
  )

(use-package bash-completion)

(setq column-number-mode t)
(setq display-battery-mode t)
(setq electric-indent-mode nil)
(setq indent-tabs-mode nil)
(setq semantic-mode t)
(setq show-paren-mode t)
(setq word-wrap t)
(setq inhibit-startup-message t)

(custom-set-faces
 '(default ((t (:inherit nil :stipple
                         nil :background "white" :foreground "black" :inverse-video
                         nil :box nil :strike-through nil :overline nil :underline
                         nil :slant normal :weight normal :height 200 :width
                         normal :foundry "PfEd" :family "Inconsolata" ))))
 '(aw-leading-char-face ((t (:inherit
                             ace-jump-face-foreground :height 3.0))))
 )

(global-set-key (kbd "<f5>") 'revert-buffer)

(defalias 'list-buffers 'ibuffer-other-window
  )

(setq gnus-group-update-tool-bar t)
(setq menu-bar-mode nil)
(setq tool-bar-mode nil)

(setq word-wrap t)
(setq truncate-lines t)

(use-package semantic
  :config
  (semantic-mode 1)
  )

(use-package multiple-cursors
  :bind
  (
   ("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  )

(setq global-ede-mode t)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(ido-mode 1)

(use-package projectile
  )

(winner-mode 1)
(windmove-default-keybindings)

(use-package counsel
  )

(use-package swiper-helm
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)

  :bind
  (
   ("<f1> f" . counsel-describe-function)
   ("<f1> l" . counsel-find-library)
   ("<f1> v" . counsel-describe-variable)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("<f6>" . ivy-resume)
   ("C-S-o" . counsel-rhythmbox)
   ("C-c C-r" . ivy-resume)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-r" . counsel-expression-history)
   ("C-x C-f" . counsel-find-file)
   ("C-x l" . counsel-locate)
   ("C-s" . swiper)
   )
  )

(use-package ace-window
  :init
  (global-set-key [remap other-window] 'ace-window)
  )

(use-package tabbar
  :config
  (tabbar-mode 1)
  )

'(use-package evil
  :config
  (evil-mode 1)
  (add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)
  (add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
  )

'(use-package evil-goggles
  :config
  (evil-goggles-mode)
  )

(use-package try
  )

(use-package which-key
  :config (which-key-mode)
  )

(use-package helm
  :bind
  (
   ("C-x c f" . helm-multi-files)
   ("M-x" . helm-M-x)
   )
  )

(use-package org
  :requires ob-ditaa ob-ipython htmlize

  :config
  ;; Don't prompt me to confirm every time I want to evaluate a block.
  (setq org-confirm-babel-evaluate nil)

  (setq org-directory "~/foo/my-org-mode-notes")
  (setq org-default-notes-file (concat org-directory "/main.org"))
  (setq org-export-html-postamble nil)
  (setq org-hide-leading-stars t)
  (setq org-startup-folded 'overview)
  (setq org-startup-indented t)

  (add-to-list 'org-agenda-files "~/foo/my-org-mode-notes/")
  (add-to-list 'org-agenda-files "~/mine/orgmode/")
  (add-to-list 'org-agenda-files "~/mine/syncthing/sg3/shared/orgmode/")
  (add-to-list 'org-agenda-files "~/mine/syncthing/sg3/shared/orgzly/")

  (setq org-habit-following-days 30)
  (setq org-habit-show-all-today t)
  (setq org-habit-show-habits-only-for-today nil)

  (add-to-list 'org-modules 'org-bbdb)
  (add-to-list 'org-modules 'org-bibtex)
  (add-to-list 'org-modules 'org-docview)
  (add-to-list 'org-modules 'org-gnus)
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-info)
  (add-to-list 'org-modules 'org-irc)
  (add-to-list 'org-modules 'org-mhe)
  (add-to-list 'org-modules 'org-rmail)
  (add-to-list 'org-modules 'org-w3m)

  (add-to-list 'org-babel-load-languages '(emacs-lisp . t))
  (add-to-list 'org-babel-load-languages '(ipython . t))
  (add-to-list 'org-babel-load-languages '(python . t))
  (add-to-list 'org-babel-load-languages '(ditaa . t))

  (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")


  (add-to-list 'org-babel-tangle-lang-exts '("haskell" . "hs"))
  (add-to-list 'org-babel-tangle-lang-exts '("ipython" . "ipy"))


  (defun org-babel-tangle-block ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'org-babel-tangle))
    )

  (setq org-use-property-inheritance (list "STYLE"))

  (defun yuvallanger-org-find-main-file ()
    (interactive)
    (find-file "~/foo/my-org-mode-notes/main.org")
    )

  :bind
  (
   ("C-c a" . org-agenda)
   ;; ("C-c b" . org-babel-tangle-block)
   ("C-c b" . org-iswitchb)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c o" . yuvallanger-org-find-main-file)
   )
  )

(use-package org-ref
  :config
  ;; The following is from the README.org
  ;; https://github.com/jkitchin/org-ref/blob/master/README.org
  (setq reftex-default-bibliography '("~/ownCloud/bibliography/references.bib"))
  (setq org-ref-bibliography-notes "~/ownCloud/bibliography/notes.org")
  (setq org-ref-default-bibliography '("~/ownCloud/bibliography/references.bib"))
  (setq org-ref-pdf-directory "~/ownCloud/bibliography/bibtex-pdfs/")
  (setq bibtex-completion-bibliography "~/ownCloud/bibliography/references.bib")
  (setq bibtex-completion-library-path "~/ownCloud/bibliography/bibtex-pdfs")
  (setq bibtex-completion-notes-path "~/ownCloud/bibliography/helm-bibtex-notes")
  )

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook
            (lambda () (org-bullets-mode 1)))
  )

'(use-package org-caldav
  :config
  (setq org-caldav-url "https://owncloud.kaka.farm/remote.php/caldav/calendars/yuvallanger")
  (setq org-caldav-calendar-id "org-mode")
  (setq org-caldav-calendars '((:url "https://owncloud.kaka.farm/remote.php/caldav/calendars/yuvallanger"
                                     :calendar-id "org-mode"
                                     :files ("~/mine/orgmode/calendar.org")
                                     :inbox "~/mine/orgmode/caldav-sync-calendar-inbox.org")

                               (:url "https://owncloud.kaka.farm/remote.php/dav/calendars/yuvallanger"
                                     :calendar-id "org-mode"
                                     :files ("~/mine/orgmode/tasks.org")
                                     :inbox "~/mine/orgmode/caldav-sync-tasks-inbox.org"))))

(use-package orgnav
  )

(use-package flymake
  :config
  (setq flymake-start-syntax-check-on-find-file nil)
  )

(use-package geiser
  )

'(use-package racket-mode
  )

'(use-package arduino-mode
  )

(use-package lispy
  :config
  (add-hook 'lisp-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
  )

(use-package smartparens
  :config
  (add-hook 'hy-mode-hook #'smartparens-strict-mode)
  )

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

(use-package kivy-mode
  )

(use-package yasnippet
  :config
  (setq yas-snippet-dirs "~/foo/myasnippets")
  (yas-reload-all)
  (yas-global-mode 1)
  )

(use-package paredit
  )

(use-package python-x
  :config
  (python-x-setup))

(use-package pyvenv
  :config
  (pyvenv-mode)
  (pyvenv-tracking-mode)
  ;; Let's try commenting this out.
  ;; (setq pyvenv-virtualenvwrapper-python "/usr/bin/env python")
  )

(use-package live-py-mode
  )

(use-package elpy
  :config
  (elpy-enable)
  
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt")

  (setq elpy-rpc-python-command "python3.6")
  (setq elpy-syntax-check-command "flake8")

  ;; XXX TODO
  ;; (setq elpy-disable-backend-error-display nil)
  )

(use-package ob-ipython
  :config
  ;; display/update images in the buffer after I evaluate.
  (add-hook 'org-babel-after-execute-hook
            'org-display-inline-images 'append
            )
  )

(use-package ein
  :requires markdown-mode
  )

(use-package hy-mode
  :config
  (add-hook 'hy-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'hy-mode-hook #'rainbow-delimiters-mode)
  )

(setq python-check-command "flake8")
(setq python-indent-offset 4)

(use-package haskell-mode
  :config
  (setq haskell-hoogle-url "https://www.fpcomplete.com/hoogle?q=%s")
  (setq haskell-stylish-on-save t)
  (setq haskell-tags-on-save t)
  )

'(use-package proof
  :config
  (setq proof-autosend-enable t)
  (setq proof-electric-terminator-enable t)
  (setq proof-shell-quiet-errors nil)
  )

(use-package magit
  :config
  (setq magit-log-section-arguments (quote ("--graph" "--color" "--decorate" "-n256")))

  :bind
  (
   ("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch-popup)
   )
  )

(use-package vc-fossil
  )

'(use-package flycheck
  :init (global-flycheck-mode)
  )

(use-package company
  :config
  (global-company-mode)
  )

(use-package erc
  :config
  (setq log-mode t)
  (setq erc-log-write-after-insert t)
  (setq erc-log-write-after-send t)
  )

'(use-package circe
  :config
  (setq log-mode t)
  (setq erc-log-write-after-insert t)
  (setq erc-log-write-after-send t)
  )

'(use-package helm-circe
  )

'(use-package matrix-client)

(use-package thumb-through
  )

'(use-package eloud
  :config
  (setq eloud-mode t)
  (setq eloud-speech-rate 200)
  )

'(use-package ereader
  )

(use-package spray
  :config
  (setq spray-save-point t)
  )

'(use-package emacspeak)

'(use-package elfeed-org
  :config
  (setq  elfeed-curl-max-connections 10)
  (setq rmh-elfeed-org-files '("~/foo/my-org-mode-notes/elfeed.org" "~/mine/elfeed/private.org"))
  (elfeed-org)

  :bind
  (
   ("C-x w" . elfeed)
   )
  )

'(use-package elfeed-goodies
  )

(use-package uptimes
  )
