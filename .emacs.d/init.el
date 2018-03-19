
;; start emacs server
;; (from https://github.com/edvorg/emacs-configs/blob/master/init-real.el)

(require 'server)
(unless (server-running-p)
  (server-start))

;; setup elpa addresses

(require 'package)

(setq package-enable-at-startup nil)

;; (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa-bleeding-edge" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'

(unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))


(require 'use-package)

(setq use-package-always-ensure t)

;(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
(load "~/.emacs.d/myinit.el")
