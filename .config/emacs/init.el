;;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold (* 1024 1024 1024))

(setq read-process-output-max (string-to-number
                               (with-temp-buffer
                                 (insert-file-contents "/proc/sys/fs/pipe-max-size")
                                 (buffer-string))))

(require 'server)
(unless (server-running-p)
  (server-start))

(add-to-list 'load-path (concat user-emacs-directory "kakafarm"))

(load (locate-user-emacs-file "local-stuff.el"))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(require 'use-package)

(setq major-mode-remap-alist '((emacs-lisp-mode . fundamental-mode)))

(autoload 'skribilo-mode "skribilo.el" "Skribilo mode." t)

;;; Good for reading long files!
(save-place-mode t)

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

(use-package ace-window
;;; Used for quick window switching.
  :bind
  (:map global-map
        (([(f2)] . ace-window)))
  :custom
  (aw-scope 'frame)
  )

'(use-package auth-source
  :config
  (auth-source-pass-enable)
  )

'(use-package auth-source-pass
  :custom
  (auth-source-do-cache nil)
  (auth-sources '(password-store))
  :config
  (auth-source-pass-enable)
  )

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

'(use-package consult
   :defer t
   )

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

'(use-package corfu
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
  :bind
  (("C-c l" . dictionary-lookup-definition))
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
  (elfeed-curl-max-connections 5)
  (elfeed-search-filter "@0.000001-week-ago +unread")
  (elfeed-curl-program-name (expand-file-name "~/.guix-profile/bin/curl"))
  )

(setq epa-pinentry-mode 'loopback)
(pinentry-start)

(use-package epa
  :custom
  (epg-pinentry-mode 'loopback))

(use-package emacs
  :ensure nil
  :bind
  (
   :map global-map
   ("C-c C-x C-x" . redraw-display)
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

  ;; https://www.youtube.com/watch?v=_eBlxjtOnRA&lc=Ugyy_XVIXuF4cnYaRIV4AaABAg
  ;;
  ;; @frankwu9659
  ;;
  ;; > my largest org file is 2.3M,
  ;; > I use below setting on emacs(29, 30) on mac mini bought 10 years ago and the first generation of surface go,  scrolling experience is good
  ;; >   (pixel-scroll-precision-mode 1)
  ;; >   (setq redisplay-skip-fontification-on-input t)
  (setq redisplay-skip-fontification-on-input t)

  (which-key-mode)

  (setq mode-line-buffer-identification '(-70 "%b"))

  :custom
  ;; https://www.youtube.com/watch?v=_eBlxjtOnRA&lc=Ugyy_XVIXuF4cnYaRIV4AaABAg
  ;; (pixel-scroll-precision-mode -1)

  (enable-recursive-minibuffers t)
  (inhibit-startup-screen t)
  (read-extended-command-predicate #'command-completion-default-include-p)

  (global-so-long-mode t)

  (history-length t)

  :init
  ;; Probably got it from:
  ;;
  ;; https://alexforsale.github.io/posts/emacs-vertico/
  ;;
  ;; but I do not know really.  Alternate sources are:
  ;;
  ;; https://github.com/minad/vertico/blob/c3b788b6bea10e3493ebc05a96bbde294824cff6/README.org#completing-read-multiple
  ;; https://github.com/emaphis/emacs.d/blob/41a2c1be2d07fdbb17ae039a27daa77f2a43a8f3/custom/set-vertico.el#L50
  (when (< emacs-major-version 31)
    (defun crm-indicator (args)
      (cons (format "[CRM%s] %s"
                    (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'"
                                              ""
                                              crm-separator)
                    (car args))
            (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)))

'(use-package ement
  :custom
  (ement-auto-sync nil)
  (ement-save-sessions t)
  )

(use-package erc
  :defer t
  :ensure nil
  :commands (erc-tls)
  :custom
  (erc-ask-about-multiline-input nil)
  (erc-auto-query 'bury)
  (erc-autojoin-channels-alist '())
  (erc-fill-column 100)
  (erc-fill-function 'erc-fill-wrap)
  (erc-fill-static-center 8)
  (erc-header-line-format nil)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-inhibit-multiline-input nil)
  (erc-kill-buffer-on-part nil)
  (erc-log-channels-directory "~/mine/erc-logs/")
  (erc-log-insert-log-on-open t)
  (erc-log-write-after-insert t)
  (erc-log-write-after-send t)
  (erc-modules '(
                 bufbar
                 button
                 completion
                 fill
                 irccontrols
                 list
                 log
                 match
                 menu
                 netsplit
                 networks
                 nickbar
                 noncommands
                 notifications
                 readonly
                 ring
                 sasl
                 scrolltobottom
                 spelling
                 stamp
                 track
                 ))
  (erc-nick "krepalakh")                ; not "antti"
  (erc-port 6697)
  (erc-sasl-mechanism 'plain)
  (erc-scrollbottom t)
  (erc-scrollbottom-on-input t)
  (erc-scrollbottom-on-output t)
  ;; (erc-server "irc.libera.chat")
  (erc-track-shorten-start 8)
  (erc-use-tls t)
  :bind
  (("C-c i r c" . 'kakafarm/erc-soju))
  )

(use-package erc-hl-nicks
  :ensure nil
  :after erc
  :hook (erc-mode . erc-hl-nicks-mode)
  :config
  (setq erc-hl-nicks-colors
        [
         "#FF4500"
         "#32CD32"
         "#1E90FF"
         "#FFD700"
         "#FF69B4"
         "#00CED1"
         "#7FFF00"
         "#FF6347"
         ]))

(use-package erc-track
  :custom
  ;; Without this our MODE LINE is spammed with a list of buffer names
  ;; of active buffers.  Use erc-bufbar-mode to show buffer activity
  ;; in a better way!
  (erc-track-position-in-mode-line nil)
  )

(use-package eww
  :custom
  (eww-history-limit 999999)
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
  ;; :config
  ;; ((define-key 'geiser-mode-map)

  ;;  (global-unset-key "C-c C-a")
  ;;  (unbind-key "C-c C-e" geiser-mode-map)
  ;;  (unbind-key "C-c C-a" geiser-mode-map))
  :custom
  (geiser-active-implementations '(guile))
  (geiser-default-implementation 'guile)
  (geiser-mode-auto-p t)
  (geiser-repl-per-project-p t)
  )

(use-package gnus
  :config
  (setq gnus-secondary-select-methods
        '((nntp "gwene" (nntp-address "news.gwene.org"))
          (nnrss "xkcd.com/atom.xml")
          (nntp "yhetil" (nntp-address "news.yhetil.org")))))

(use-package greader
  :defer t
  :commands (greader-mode greader-read)
  :custom
  (greader-current-backend 'greader-espeak)
  (greader-piper-script-path "/home/yuval/bin/,tts.scm")
  (greader-piper-script-url "http://localhost/")
  :config
  (add-hook 'greader-mode-hook 'kakafarm/sentence-end-double-nilify-for-read-only-buffers)
  (greader-estimated-time-mode)
  ;; :bind
  ;; (
  ;;  :map greader-mode-map
  ;;  ("C-c g b" . (lambda () (interactive) (kakafarm/greader-estimate-reading-time)))
  ;;  ("C-c g r" . (lambda () (interactive) (kakafarm/greader-estimate-reading-time (point))))
  ;;  )
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
         ;; helpful-mode
         lisp-mode
         ;; text-mode ;; It fucks up my magit commit message C-c C-c.
         w3m-mode
         )
  )

'(use-package helm
   :defer t
   :bind
   (
    :map global-map
    ("M-x" . helm-M-x)
    )
   )

;; (use-package helpful
;;   :defer t
;;   :bind
;;   (
;;    ("C-h f" . helpful-function)
;;    ("C-h k" . helpful-key)
;;    ("C-h m" . helpful-mode)
;;    ("C-h v" . helpful-variable)
;;    )
;;   )

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
  ;; :bind
  ;; (
  ;;  :map global-map
  ;;  ("C-c C-w" . kakafarm/multi-vterm-weechat)
  ;;  ("C-c w"   . kakafarm/multi-vterm-weechat)
  ;;  )
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
  :custom
  ;; (mastodon-instance-url "https://emacs.ch/")
                                        ; <https://emacs.ch/>, RIP.
                                        ; Lost but not forgotten.
                                        ; Miss you, big man.
  (mastodon-active-user "kakafarm")
  (mastodon-instance-url "https://shitposter.world")
  (mastodon-media--hide-sensitive-media nil)
  (mastodon-tl--show-avatars t)
  (mastodon-auth-use-source nil)
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
  ;; '(setq modus-themes-mode-line '(borderless
  ;;                                 accented
  ;;                                 ;; 3d
  ;;                                 padded
  ;;                                 ;; moody
  ;;                                 )
  ;;        modus-themes-region '(;;accented
  ;;                              bg-only
  ;;                              ;;no-extend
  ;;                              )
  ;;        modus-themes-paren-match '(bold
  ;;                                   intense)
  ;;        ;;modus-themes-syntax '(alt-syntax)
  ;;        modus-themes-scale-headings t
  ;;        modus-themes-org-blocks 'tinted-background)
  (load-theme 'modus-vivendi)
  ;;(load-theme 'wheatgrass)
  :custom
  (modus-themes-mode-line '(borderless
                            accented
                            ;; 3d
                            padded
                            ;; moody
                            ))
  (modus-themes-region '(;;accented
                         bg-only
                         ;;no-extend
                         ))
  (modus-themes-paren-match '(bold
                              intense))
  ;; (modus-themes-syntax '(alt-syntax))
  (modus-themes-scale-headings t)
  (modus-themes-org-blocks 'tinted-background)
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

'(use-package nov
  :defer t
  :mode ((rx ".epub" string-end) . nov-mode)
  :hook (
         follow-mode
         greader-mode
         kakafarm/set-truncate-partial-width-windows-to-nil
         ))

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
  ;; (completion-category-defaults nil)
  (completion-category-overrides
   '((file . ((styles . (basic partial-completion)))))
   ))

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
  (org-tags-sort-function #'lessp)
  (org-todo-keywords '((sequence "TODO" "|" "DONE" "DNFT")))
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

(use-package pass :commands password-store-get)

(use-package rainbow-delimiters
  :defer t
  :config
  (rainbow-delimiters-mode))

(use-package rcirc
  :custom
  (rcirc-default-full-name "")
  (rcirc-default-nick "")
  (rcirc-default-part-reason "")
  (rcirc-default-port 0)
  (rcirc-default-quit-reason "")
  (rcirc-default-user-name "")
  :config
  (setq rcirc-server-alist
        (list (list "127.0.0.1"
                    :port 6667
                    :nick "krepalakh"
                    :user-name "krepalakh/libera"
                    :full-name "real name"
                    :password (kakafarm/krepalakh-soju-password)
                    ))
        )
  )

(use-package recentf
  :defer t
  :config
  (recentf-mode 1)
  :bind (("C-S-t" . recentf-open-files)
         ("C-c t" . recentf-open-files))
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  )

(use-package savehist
  :init
  (savehist-mode)
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
  ;;             ;; ("C-c C-e" . arei-mode-map)
  ;;             ;; ("C-c C-a" . arei)
  ;;             )
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
  (global-visual-line-mode nil)
  )

(use-package undo-tree
  :defer t
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil)
  :bind
  ("C-x u" . undo-tree-visualize))

'(use-package vertico
   :init
   (vertico-mode)
   ;; :config
   ;; (keymap-set vertico-map "?" #'minibuffer-completion-help)
   ;; (keymap-set vertico-map "M-RET" #'minibuffer-force-complete-and-exit)
   ;; (keymap-set vertico-map "M-TAB" #'minibuffer-complete)
   )

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
