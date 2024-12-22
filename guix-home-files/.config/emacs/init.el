;;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold (* 1024 1024 1024))

(setq read-process-output-max (string-to-number
                               (with-temp-buffer
                                 (insert-file-contents "/proc/sys/fs/pipe-max-size")
                                 (buffer-string))))

(require 'server)
(unless (server-running-p)
  (server-start))

(require 'benchmark-init)

(add-to-list 'load-path (concat user-emacs-directory
                                "local-packages/emacs-kakafarm/"))

(load (locate-user-emacs-file "local-stuff.el"))

(setq package-archives '())
(require 'use-package)

(setq major-mode-remap-alist '((emacs-lisp-mode . fundamental-mode)))

(autoload 'skribilo-mode "skribilo.el" "Skribilo mode." t)

'(progn
;;; Weird https://www.youtube.com/watch?v=TjMTNSdhUvk stuff.
   (setq inhibit-startup-message t)
   (menu-bar-mode 1)
   (fido-vertical-mode 1)
   ;; (icomplete-vertical-mode 1)
   (push 'flex completion-styles)
   '(eldoc-mode 1)
   (flymake-mode 1)
   ;; (load-theme 'modus-vivendi-tinted t)
   (setopt mode-line-end-spaces nil)
   (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?|))
   (xterm-mouse-mode 1))

(use-package browse-url
  :demand t
  :custom
  (browse-url-browser-function 'eww-browse-url)
  (browse-url-new-window-flag t)
  )

(use-package casual-calc
  :ensure nil
  :bind (:map calc-mode-map ("C-o" . casual-calc-tmenu)))

(use-package casual-dired
  :ensure nil
  :bind (:map dired-mode-map ("C-o" . casual-dired-tmenu)))

(use-package casual-info
  :ensure nil
  :bind (:map Info-mode-map ("C-o" . casual-info-tmenu)))

'(use-package company
   :defer t
   :init
   (add-hook 'after-init-hook 'global-company-mode))

(use-package compilation
  :demand t
  :requires (ansi-color)
  )

;; FIXME: Why can't I use it in the (use-package compilation :hook or :init)?
(add-hook 'compilation-filter-hook #'kakafarm/colorize-compilation)


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

(use-package detubifier
  :defer t
  )

(use-package dictionary
  :defer t
  :custom
  (dictionary-server "localhost"))

(use-package elfeed
  :defer t
  :config
  ;; XXX: Not sure if I want it to be a `kakafarm/elfeed-feeds'
  ;; variable that is then set to `elfeed-feeds' in the `:custom'
  ;; section, or loaded inside the `elfeed-feeds.el' file using the
  ;; `customize-set-value' function.
  (load (locate-user-emacs-file "elfeed-feeds.el"))
  (customize-set-value 'elfeed-curl-program-name
                       (expand-file-name "~/.guix-profile/bin/curl"))
  :custom
  ;; (elfeed-curl-max-connections 10)
  (elfeed-search-filter "@1-week-ago +unread")
  (elfeed-curl-program-name (expand-file-name "~/.guix-profile/bin/curl"))
  )

'(use-package elfeed-goodies
   :config
   (elfeed-goodies/setup))

(use-package emacs
  :ensure nil
  :bind
  (
   :map global-map
   ("C-c p p" . kakafarm/percent-read)
   ("C-c C-s" . (lambda (beg end)
                  (interactive (list (region-beginning) (region-end)))

                  (sort-lines '()
                              beg
                              end)))

   ;; (";" . #'kakafarm/easy-underscore)
   )
  :config
  (set-register ?i `(file . ,(locate-user-emacs-file "init.el")))
  '(ido-mode t)
  ;; (tab-bar-mode)
  :custom
  (inhibit-startup-screen t)
  )

(use-package ement
  :custom
  (ement-auto-sync nil)
  (ement-save-sessions t)
  )

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
   ("C-c f a p"   . ffap-menu)
   ("C-c f f a p" . kakafarm/ffap-browse-urls)
   ))

(use-package fontset
  :defer t
  :config
  (set-fontset-font t 'hebrew "Noto Sans Hebrew"))

(use-package geiser
  :defer t
  :after (scheme-mode)
  :config
  '((define-key 'geiser-mode-map)
    (setq geiser-active-implementations '(guile))
    (global-unset-key "C-c C-a")
    (unbind-key "C-c C-e" geiser-mode-map)
    (unbind-key "C-c C-a" geiser-mode-map))
  :custom
  (geiser-default-implementation 'guile)
  (geiser-mode-auto-p nil)
  (geiser-repl-per-project-p t)
  )

(use-package greader
  :defer t
  :commands (greader-mode)
  :config
  (add-hook 'greader-mode-hook
            'kakafarm/sentence-end-double-nilify-for-read-only-buffers)
  :bind
  (
   :map greader-mode-map
   ("C-c g b" . (lambda () (interactive) (kakafarm/greader-estimate-reading-time)))
   ("C-c g r" . (lambda () (interactive) (kakafarm/greader-estimate-reading-time (point))))
   )
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
         helpful-mode
         lisp-mode
         nov-mode
         ;; text-mode ;; It fucks up my magit commit message C-c C-c.
         w3m-mode
         )
  )

(use-package helm
  :defer t
  :bind
  (
   :map global-map
   ("M-x" . helm-M-x)
   )
  )

(use-package helpful
  :defer t
  :bind
  (
   ("C-h f" . helpful-function)
   ("C-h k" . helpful-key)
   ("C-h m" . helpful-mode)
   ("C-h v" . helpful-variable)
   )
  )

(use-package howm
  :init
  (setq howm-directory "~/mine/howm/")
  (setq howm-home-directory howm-directory)
  (setq howm-file-name-format "%Y-%m-%d-%H%M%S.org")
  (setq howm-view-title-header "*")
  (setq howm-dtime-format "<%Y-%m-%d %a %H:%M>")
  (setq howm-prefix (kbd "C-c ;"))
  :bind*
  ("C-c ; ;" . howm-menu)
  )

(use-package icomplete
  :defer t
  :config
  ;; Display completions continuously in minibuffer.
  (icomplete-mode 1))

(use-package info
  :defer t
  :custom
  (Info-additional-directory-list '("~/infopath/")))

(use-package kakafarm
  :demand t
  :bind
  (
   :map global-map
   ("C-c C-w" . kakafarm/multi-vterm-weechat)
   ("C-c w"   . kakafarm/multi-vterm-weechat)
   )
  )

(use-package magit
  :custom
  (magit-diff-refine-hunk t)
  (magit-diff-refine-ignore-whitespace nil)
  )

(use-package magit-todos
  :defer t
  :after magit
  :config
  (magit-todos-mode 1)
  :custom
  (magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
  )

(use-package mastodon
  :defer t
  :init
  ;; (setq mastodon-active-user "kakafarm"
  ;;       mastodon-instance-url "https://emacs.ch/")
  (setq mastodon-active-user "kakafarm"
        mastodon-instance-url "https://shitposter.world/")
  )

(use-package menu-bar
  :custom
  (menu-bar-mode nil)
  )

'(use-package mode-line
   :custom
   (mode-line-percent-position '(6 "%q"))
   )

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

(use-package multi-vterm
  ;; :demand t
  :bind
  (
   :map global-map
   ("C-c <RET> <RET>" . multi-vterm)
   ("C-c m m"         . multi-vterm)
   :map vterm-mode-map
   ("C-q" . vterm-send-next-key)
   )
  :commands
  (
   multi-vterm
   multi-vterm-buffer-exist-p
   )
  )

'(use-package nano-tts
   :defer t
   :hook (eww-after-render nov-mode Info-mode)
   :custom
   (nano-tts-words-per-minute 225)
   )

(use-package nov
  :defer t
  :mode ((rx ".epub" string-end) . nov-mode))

(use-package fundamental
  :mode ((rx ".el" string-end) . fundamental-mode))

'(use-package opml-to-elfeed-feeds
   :custom
   (opml-to-elfeed-feeds-elfeed-feeds nil t)
   )

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

(use-package org
  :defer t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (emacs-lisp . t)
     (scheme     . t)
     (shell      . t)
     ))
  :custom
  (org-agenda-files (list (expand-file-name "~/mine/org/")
                          (expand-file-name "~/Documents/notes/")))
  (org-default-notes-file (expand-file-name "~/mine/org/org.org"))
  (org-directory (expand-file-name "~/mine/org/"))
  (org-export-use-babel nil)
  (org-html-postamble t)
  (org-html-postamble-format '(("en" "<p class=\"author\">Author: %a</p>\12<p class=\"date\">Date: %d</p>\12<p class=\"creator\">%c</p>\12<p class=\"validation\">%v</p>")))
  (org-structure-template-alist '(
                                  ("a" . "export ascii")
                                  ("c" . "center")
                                  ("C" . "comment")
                                  ("e" . "example")
                                  ("E" . "export")
                                  ("h" . "export html")
                                  ("l" . "export latex")
                                  ("q" . "quote")
                                  ("s" . "src")
                                  ("v" . "verse")
                                  ("g" . "src scheme :session moo :results output :tangle eopl3.scm")
                                  ("z" . "src scheme")
                                  ))
  )

(use-package org-roam
  :defer t
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  ;;(org-roam-db-autosync-enable)
  ;;(org-roam-db-autosync-mode)
  :custom
  (org-roam-directory "~/mine/roam/")
  (org-roam-graph-viewer '(lambda (x) nil))
  )

(use-package paredit
  :defer t
  :hook (emacs-lisp-mode
         lisp-mode
         scheme-mode))

(use-package paren
  :defer t
  :config
  (show-paren-mode))

;; XXX: Disabled.
'(use-package parinfer
   :defer t
   :hook (emacs-lisp-mode
          lisp-mode
          scheme-mode))

'(use-package perspective
   :init
   (persp-mode)
   :bind
   ("C-c M-b" . persp-list-buffers)
   :custom
   (persp-mode-prefix-key (kbd "C-c M-p")))

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
         ("C-c l" . dictionary-lookup-definition))
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  )

(use-package scheme-mode
  :defer t
  :config
  ;; TODO: Did it even work?!
  (add-hook 'scheme-mode-hook 'hs-minor-mode)
  (progn ;; For SRFI-253:
    (put 'define-checked 'scheme-indent-function 1)
    (put 'lambda-checked 'scheme-indent-function 1))
  :mode (rx "." (| "scm" "sxml" "skb") string-end)
  ;; :bind (:map scheme-mode-map
  ;;             ("C-c C-e" . arei-mode-map)
  ;;             ("C-c C-a" . arei))
  )

(use-package shr
  :custom
  (shr-width 75)
  (shr-use-fonts nil)
  (shr-use-colors nil)
  (shr-indentation 2)
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
  :custom
  (global-visual-line-mode t)
  )

(use-package undo-tree
  :defer t
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil)
  :bind
  ("C-x u" . undo-tree-visualize))

(use-package vterm
  :custom
  (vterm-environment '("LC_ALL=en_IL.utf8"))
  )

'(use-package whitespace-cleanup
   :custom
   (global-whitespace-cleanup-mode t)
   )

(use-package window
  :defer t
  :config
  '(advice-add 'recenter-top-bottom
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
(put 'scroll-left 'disabled nil)
