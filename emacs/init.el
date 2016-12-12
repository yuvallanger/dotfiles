(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(evil-mode 1)


(global-set-key (kbd "M-x") 'helm-M-x)


(elpy-enable)
(elpy-use-ipython)


(setq org-confirm-babel-evaluate nil)   ;don't prompt me to confirm
                                        ;everytime I want to evaluate
                                        ;a block

;;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook
 'org-display-inline-images 'append)

(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-battery-mode t)
 '(electric-indent-mode nil)
 '(elpy-rpc-python-command "python3")
 '(erc-log-mode t)
 '(erc-log-write-after-insert t)
 '(erc-log-write-after-send t)
 '(haskell-hoogle-url "https://www.fpcomplete.com/hoogle?q=%s")
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(indent-tabs-mode nil)
 '(proof-autosend-enable t)
 '(proof-electric-terminator-enable t)
 '(proof-shell-quiet-errors nil)
 '(python-check-command "flake8"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
