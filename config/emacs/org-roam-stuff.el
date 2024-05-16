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

(defun kakafarm/org-roam-publication-wrapper (plist filename pubdir)
  ;; (when (kakafarm/org-roam-publishable-node-p filename)
  ;;   nil)
  ;;(org-roam-graph) ; How the fuck do I make this one not show every fucking node in the org-roam database?!
  (org-html-publish-to-html plist
                            filename
                            pubdir)
  (setq kakafarm/org-roam-project-publish-time
        (cadr (current-time))))

(defun kakafarm/org-roam-custom-link-builder (node)
  (let ((node-file (org-roam-node-file node)))
    ;; (when (kakafarm/org-roam-publishable-node-p node-file)
    ;;   nil)
    (message (format "kakafarm/org-roam-custom-link-builder: %S" node))
    (concat (file-name-base node-file)
            ".html")))
