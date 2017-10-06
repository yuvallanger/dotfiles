(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives (cons "gnu" "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives (cons "melpa-bleeding-edge" "https://melpa.org/packages/"))
(add-to-list 'package-archives (cons "melpa-stable" "https://stable.melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))

(use-package req-package :ensure t)

(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
