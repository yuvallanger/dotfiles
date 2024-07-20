(setq gc-cons-threshold (* 1024 1024 1024))

(setq read-process-output-max (string-to-number
                               (with-temp-buffer
                                 (insert-file-contents "/proc/sys/fs/pipe-max-size")
                                 (buffer-string))))

(require 'server)
(unless (server-running-p)
  (server-start))

(require 'benchmark-init)

(load (locate-user-emacs-file "kakafarm-utils.el"))
(load (locate-user-emacs-file "local-stuff.el"))

(setq package-archives '())
(require 'use-package)

(dolist (x '("C-c m m"
             "C-c m <RET>"
             "C-c <RET> m"
             "C-c <RET> <RET>"))
  (keymap-global-set x 'multi-vterm))

'(use-package company
   :defer t
   :init
   (add-hook 'after-init-hook 'global-company-mode))

(use-package corfu
  :defer t
  :config
  (global-corfu-mode)
  :custom
  (corfu-auto t))

(use-package cus-edit
  :config
  ;; Use another file for the ``customize'' customisations.
  (setq custom-file (locate-user-emacs-file "custom-variables.el"))
  (load custom-file 'noerror 'nomessage))

(use-package dictionary
  :defer t
  :custom
  (dictionary-server "localhost"))

(use-package elfeed
  :defer t
  :init
  (load (locate-user-emacs-file "elfeed-feeds.el"))
  :custom
  (elfeed-curl-max-connections 10)
  (elfeed-search-filter "@2-months +unread")
  (elfeed-feeds kakafarm/elfeed-feeds))

'(use-package elfeed-goodies
   :config
   (elfeed-goodies/setup))

(use-package emacs
  :ensure nil
  :defer
  :bind
  ;;((";" . #'kakafarm/easy-underscore))
  ("M-x" . helm-M-x)
  )

(use-package fontset
  :defer t
  :config
  (set-fontset-font t 'hebrew "Noto Sans Hebrew"))

(use-package helm
  :defer t)

(use-package helpful
  :defer t)

(use-package orderless
  :defer t
  :custom
  (completion-styles
   '(orderless
     basic))
  (completion-category-overrides
   '((file (styles
            basic
            partial-completion)))))

(use-package erc
  :defer t
  :custom
  (erc-server "irc.libera.chat")
  (erc-nick "kakafarm")
  (erc-track-shorten-start 8)
  (erc-kill-buffer-on-part t)
  (erc-auto-query 'bury)
  (erc-autojoin-channels-alist '((libera-kakafarm "#systemcrafters")))
  :bind
  (("C-c i r c" . (lambda ()
                    (interactive)
                    (erc-tls :id 'libera-kakafarm))))
  ;; :config
  ;; (erc-tls :id 'libera-kakafarm)
  )

(use-package ffap
  :defer t
  :bind
  (
   ("C-c f a p" . ffap-menu)
   ("C-c f f a p" . kakafarm/ffap-browse-urls)
   ))

'(use-package mutli-vterm
   :defer t
   :bind
   (("C-q" . vterm-send-next-key)))

(use-package geiser
  :defer t
  :after (scheme-mode)
  :config
  '((define-key 'geiser-mode-map)
    (setq geiser-active-implementations '(guile))
    (global-unset-key "C-c C-a")
    (unbind-key "C-c C-e" geiser-mode-map)
    (unbind-key "C-c C-a" geiser-mode-map)))

(use-package greader
  :defer t
  :commands (greader-mode)
  :config
  (add-hook 'greader-mode-hook
            'kakafarm/sentence-end-double-nilify-for-read-only-buffers)
  :hook (
         Custom-mode
         Info-mode
         Man-mode
         elfeed-show
         elfeed-show-mode
         elpher
         eww-after-render
         fundamental-mode
         help-mode
         lisp-mode
         nov-mode
         text-mode
         w3m-mode
         ))

(use-package icomplete
  :defer t
  :config
  ;; Display completions continuously in minibuffer.
  (icomplete-mode 1))

(use-package magit-todos
  :defer t
  :after magit
  :config (magit-todos-mode 1))

(use-package mastodon
  :defer t
  :init
  (setq mastodon-active-user "kakafarm"
        mastodon-instance-url "https://emacs.ch/"))

(use-package modus-themes
  :defer t
  :init
  (setq modus-themes-mode-line '(borderless
                                 accented
                                 ;; 3d
                                 padded
                                 ;; moody
                                 )
        modus-themes-region '(;;accented
                              bg-only
                              ;;no-extend
                              )
        modus-themes-paren-match '(bold
                                   intense)
        ;;modus-themes-syntax '(alt-syntax)
        modus-themes-scale-headings t
        modus-themes-org-blocks 'tinted-background)
  (load-theme 'modus-vivendi)
  ;;(load-theme 'wheatgrass)
  )

(use-package mule
  :defer t
  :config
  ;;; https://emacs.stackexchange.com/questions/34322/set-default-coding-system-utf-8
  (set-language-environment "utf-8"))

(use-package info
  :defer t
  :custom
  (Info-additional-directory-list '("~/infopath/")))

'(use-package nano-tts
   :defer t
   :hook (eww-after-render nov-mode Info-mode))

(use-package nov
  :defer t
  :mode ("\\.epub\\'" . nov-mode))

(use-package org
  :defer t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((scheme . t)
     (emacs-lisp . t))))

(use-package org-roam
  :defer t
  :custom (org-roam-directory "~/mine/roam/")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  ;;(org-roam-db-autosync-enable)
  ;;(org-roam-db-autosync-mode)
  )

;; XXX: Disabled.
'(use-package parinfer
   :defer t
   :hook (emacs-lisp-mode
          lisp-mode
          scheme-mode))

(use-package paredit
  :defer t
  :hook (emacs-lisp-mode
         lisp-mode
         scheme-mode))

(use-package paren
  :defer t
  :config
  (show-paren-mode))

(use-package rainbow-delimiters
  :defer t
  :config
  (rainbow-delimiters-mode))

(use-package recentf
  :defer t
  :config
  (recentf-mode 1)
  :bind (("C-S-t" . recentf-open-files)
         ("C-c t" . recentf-open-files)
         ("C-c l" . dictionary-lookup-definition)))

(use-package undo-tree
  :defer
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil))

(use-package scheme-mode
  :defer t
  :mode "\\.\\(scm\\|sxml\\)\\'"
  ;; :bind (:map scheme-mode-map
  ;;             ("C-c C-e" . arei-mode-map)
  ;;             ("C-c C-a" . arei))
  )

(use-package simple
  :defer
  :init
  (advice-add 'kill-ring-save
              :before
              'kakafarm/pulse-current-region)
  :config
  (column-number-mode)
  ;; Don't want tabs in any of my source files.
  (setq-default indent-tabs-mode
                nil)
  '(advice-add 'scratch-buffer
               :after
               (lambda () "Switch to text-mode."
                 (text-mode)))
  )

(use-package window
  :defer t
  :config
  (advice-add 'recenter-top-bottom
              :around
              'kakafarm/recenter-top-bottom))

(progn
;;; Emacs From Scratch
;;; https://systemcrafters.net/emacs-from-scratch/
;;; https://www.youtube.com/playlist?list=PLEoMzSkcN8oPH1au7H6B7bBJ4ZO7BXjSZ

  (setq visible-cursor t
        visible-bell t)

;;; The Basics of Emacs Configuration
;;; https://systemcrafters.net/emacs-from-scratch/basics-of-emacs-configuration/
;;; https://www.youtube.com/watch?v=OaF-N-FuGtc

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  ;; (global-display-line-numbers-mode 1)
  (hl-line-mode 1)
  (blink-cursor-mode 1)

;;; https://systemcrafters.net/emacs-from-scratch/the-best-default-settings/
;;; https://www.youtube.com/watch?v=51eSeqcaikM
  ;;(recentf-mode 1)
  (setq history-length 25)
  (savehist-mode 1)
  (save-place-mode 1))

(progn
;;; Load org-roam stuff.

  (defvar kakafarm/org-roam-my-publish-time 0
    ;; TODO: Write docstring.
    "TODO"
    )

  (setq org-publish-project-alist
        `(("roam"
           :base-directory "~/mine/roam/publish/"
           :auto-sitemap t
           :sitemap-function kakafarm/org-roam-sitemap
           :sitemap-title "Roam Notes"
           :publishing-function kakafarm/org-roam-publication-wrapper
           :publishing-directory "~/mine/roam-export"
           :section-number nil
           :table-of-contents nil
           :include ,(directory-files "~/mine/roam/publish/" t ".*.org$")
           :html-head "<link rel=\"stylesheet\" href=\"/index.css\" type=\"text/css\">")))

  (setq org-roam-graph-link-builder
        'kakafarm/org-roam-custom-link-builder)

  (add-hook 'org-roam-graph-generation-hook
            (lambda (dot svg)
              (if (< (- (cadr (current-time))
                        kakafarm/org-roam-project-publish-time)
                     5)
                  (progn
                    (copy-file svg
                               "~/mine/roam-export/sitemap.svg"
                               't)
                    (setq kakafarm/org-roam-project-publish-time
                          0))))))

(setq gc-cons-threshold (* 200 1024 1024))
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
