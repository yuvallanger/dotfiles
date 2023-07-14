(setq gc-cons-threshold (* 1024 1024 1024))

;; https://emacs.stackexchange.com/questions/34322/set-default-coding-system-utf-8
(set-language-environment "utf-8")

'(use-package
   org-roam
   :ensure t
   :custom (org-roam-directory "~/mine/roam")
   :bind (("C-c n l" . org-roam-buffer-toggle)
          ("C-c n f" . org-roam-node-find)
          ("C-c n i" . org-roam-node-insert))
   ;;:config (org-roam-setup)
   )

(show-paren-mode)
(rainbow-delimiters-mode)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((scheme . t)
   (emacs-lisp . t)))

'(letrec moo ((x 20))
         (cond
          ((< x 0)
           (message "END"))
          (else
           (message (number-to-string x))
           (moo (- x 1)))))


(progn
  (defun kakafarm/set-key-bindings ()
    (global-set-key (kbd "C-S-t")
                    #'recentf-open-files)
    (global-set-key (kbd "C-c t")
                    #'recentf-open-files)
    (global-set-key (kbd "C-c l")
                    #'dictionary-lookup-definition))

  (kakafarm/set-key-bindings))


(progn
  (defun kakafarm/call-process-with-string-as-input (program &optional input-string &rest args)
    (with-temp-buffer
      (let ((our-output-buffer (current-buffer)))
        (if input-string
            (with-temp-buffer
              (let ((our-input-buffer (current-buffer)))
                (progn
                  (erase-buffer)
                  (insert input-string)
                  (apply 'call-process-region
                         (buffer-end -1)
                         (buffer-end 1)
                         program
                         nil
                         our-output-buffer
                         nil
                         args))))
          (apply 'call-process
                 program
                 nil
                 our-output-buffer
                 nil
                 args)))
      (buffer-string)))

  (list (kakafarm/call-process-with-string-as-input "cat"
                                                    "cat says moo")
        (kakafarm/call-process-with-string-as-input "echo"
                                                    nil
                                                    "-n"
                                                    "echo echo echo")))


(progn
  ;; Uploading README.html from README.org stuff.
  (defun kakafarm/srht-repo-id (repository-name)
    "Returns the unique numerical I Dentification associated with
every sourcehut repository.

https://www.tomsdiner.org/blog/post_0003_sourcehut_readme_org_export.html"

    (interactive "sRepo name: ")
    (let* ((srht (netrc-machine (netrc-parse "~/.netrc.gpg")
                                "repo.git.sr.ht"))
           (srht-token (netrc-get srht
                                  "password"))
           (our-response (with-temp-buffer
                           (call-process "curl"
                                         nil
                                         (list (current-buffer) nil)
                                         nil
                                         "--oauth2-bearer" srht-token
                                         "-G"
                                         "--data-urlencode"
                                         (concat "query=query { me { repository(name: \""
                                                 repository-name
                                                 "\") { id } } }")
                                         "https://git.sr.ht/query")
                           (buffer-string)))
           (repository-id (string-trim (kakafarm/call-process-with-string-as-input "jq"
                                                                                   our-response
                                                                                   ".data.me.repository.id"))))
      (if (called-interactively-p)
          (message "Repository ID: %S" repository-id)
        repository-id)))

  (defun kakafarm/srht-set-readme (repository-id)
    "Export the current file to HTML and set the result as README for
the sourcehut repo identified by ID.

https://www.tomsdiner.org/blog/post_0003_sourcehut_readme_org_export.html"

    (interactive "sRepository ID: ")
    (let* ((srht (netrc-machine (netrc-parse "~/.netrc.gpg")
                                "repo.git.sr.ht"))
           (srht-token (netrc-get srht
                                  "password"))
           (readme.html (org-export-as (org-export-get-backend 'html)
                                       nil
                                       nil
                                       t))
           (our-json-query (kakafarm/call-process-with-string-as-input
                            "jq"
                            readme.html
                            "-sR"
                            (concat "
{ \"query\": \"mutation UpdateRepo($id: Int!, $readme: String!) { updateRepository(id: $id, input: { readme: $readme }) { id } }\",
    \"variables\": {
        \"id\": " repository-id ",
        \"readme\": .
    }
}"))))
      (kakafarm/call-process-with-string-as-input "curl"
                                                  our-json-query
                                                  "--oauth2-bearer" srht-token
                                                  "-H" "Content-Type: application/json"
                                                  "-d@-"
                                                  "https://git.sr.ht/query"))))


(progn
  ;; Use another file for the ``customize'' customisations.
  (setq custom-file (locate-user-emacs-file "custom-variables.el"))
  (load custom-file
        'noerror
        'nomessage))


(progn
  (defun kakafarm/load-theme-stuff ()
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
    '(load-theme 'wheatgrass))

  (kakafarm/load-theme-stuff))


;; Display completions continuously in minibuffer.
(icomplete-mode 1)


(progn
  (defun kakafarm/load-emacs-from-scratch-stuff ()
    "Emacs From Scratch
  https://systemcrafters.net/emacs-from-scratch/
  https://www.youtube.com/playlist?list=PLEoMzSkcN8oPH1au7H6B7bBJ4ZO7BXjSZ"

    (setq visible-cursor t
          visible-bell t)

    ;; The Basics of Emacs Configuration
    ;; https://systemcrafters.net/emacs-from-scratch/basics-of-emacs-configuration/
    ;; https://www.youtube.com/watch?v=OaF-N-FuGtc
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (menu-bar-mode 1)
      (global-display-line-numbers-mode 1)
      (hl-line-mode 1)
      (blink-cursor-mode 1))

    ;; https://systemcrafters.net/emacs-from-scratch/the-best-default-settings/
    ;; https://www.youtube.com/watch?v=51eSeqcaikM
    (recentf-mode 1)
    (setq history-length 25)
    (savehist-mode 1)
    (save-place-mode 1))
  (kakafarm/load-emacs-from-scratch-stuff))


(set-fontset-font t 'hebrew "Noto Sans Hebrew")


;; Don't want tabs in any of my source files.
(setq-default indent-tabs-mode
              nil)


(setq geiser-active-implementations '(guile))


'(org-roam-db-autosync-mode)


(add-hook 'after-init-hook 'global-company-mode)


;; https://www.emacswiki.org/emacs/ParEdit#h5o-1
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp mode." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)


(defun kakafarm/percent-read ()
  "Display percent read by current cursor location vs. total characters in file."
  (interactive)
  (message "%.2f%%"
           (* 100
              (/ (float (- (point) 1))
                 (+ 1 (buffer-size))))))


(progn
  ;; Load org-roam stuff.

  (require 'org)

  (defun kakafarm/org-roam-keyword-is-filetags-p (keyword-node)
    (equal (org-element-property :key
                                 keyword-node)
           "FILETAGS"))

  (defun kakafarm/org-roam-filetags-keyword-is-publishable-p (filestags-keyword-node)
    (seq-contains-p (split-string (org-element-property :value
                                                        filestags-keyword-node)
                                  ":")
                    "publish"))

  (defun kakafarm/org-roam-publishable-node-p (org-filename)
    (with-temp-buffer
      (insert-file-contents org-filename)
      (org-element-map (org-element-parse-buffer) 'keyword
        (lambda (keyword)
          (and (kakafarm/org-roam-keyword-is-filetags-p keyword)
               (kakafarm/org-roam-filetags-keyword-is-publishable-p keyword)))
        nil
        t)))

  (defun kakafarm/org-roam-sitemap (title list-of-org-links)
    (message (format "kakafarm/org-roam-sitemap title: %S; list-of-links: %S\n"
                     title
                     list-of-org-links))
    ;; (let ((a-publishable-org-roam-node
    ;;        (seq-filter (lambda (org-link-list)
    ;;                      (pcase org-link-list
    ;;                        (`(,org-link)
    ;;                         (with-temp-buffer
    ;;                           (insert org-link)
    ;;                           (org-element-map (org-element-parse-buffer) 'link
    ;;                             (lambda (link)
    ;;                               ;; Check if file linked is publishable.
    ;;                               (kakafarm/org-roam-publishable-node-p
    ;;                                (concat "~/mine/roam/"
    ;;                                        (org-element-property :path
    ;;                                                              link))))
    ;;                             nil
    ;;                             t)))))
    ;;                    list-of-org-links)))
    ;;   (message "poop %S" a-publishable-org-roam-node))

    (concat
     "# -*- encoding: utf-8 -*-\n"
     "#+OPTIONS: ^:nil author:nil html-postamble:nil\n"
     ;;"#SETUPFILE: ./simple_inline.theme\n" ; No theme yet.
     "#+FILETAGS: publish\n"
     "#+TITLE: " title "\n\n"
     (org-list-to-org list-of-org-links) "\n"

     ;; TODO: No sitemap SVG yet because it shows all the fucking
     ;; files in the org-roam database.
     ;;
     ;;"file:sitemap.svg\n"
     ))

  (setq kakafarm/org-roam-my-publish-time 0)
  (defun kakafarm/org-roam-publication-wrapper (plist filename pubdir)
    ;; (when (kakafarm/org-roam-publishable-node-p filename)
    ;;   nil)
    ;;(org-roam-graph) ; How the fuck do I make this one not show every fucking node in the org-roam database?!
    (org-html-publish-to-html plist
                              filename
                              pubdir)
    (setq kakafarm/org-roam-project-publish-time
          (cadr (current-time))))

  (setq org-publish-project-alist
        (list
         (list "roam"
               :base-directory "~/mine/roam/publish/"
               :auto-sitemap t
               :sitemap-function 'kakafarm/org-roam-sitemap
               :sitemap-title "Roam Notes"
               :publishing-function 'kakafarm/org-roam-publication-wrapper
               :publishing-directory "~/mine/roam-export"
               :section-number nil
               :table-of-contents nil
               :include (directory-files "~/mine/roam/publish/" t ".*.org")
               :style "<link rel=\"stylesheet\" href=\"../other/mystyle.css\" type=\"text/css\">")))

  (defun kakafarm/org-roam-custom-link-builder (node)
    (let ((node-file (org-roam-node-file node)))
      ;; (when (kakafarm/org-roam-publishable-node-p node-file)
      ;;   nil)
      (message (format "kakafarm/org-roam-custom-link-builder: %S" node))
      (concat (file-name-base node-file)
              ".html")))

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


(setq dictionary-server "localhost")


(setq gc-cons-threshold (* 2 1024 1024))
