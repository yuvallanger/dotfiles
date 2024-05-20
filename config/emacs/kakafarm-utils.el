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
                                                  "echo echo echo"))

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
                                                "https://git.sr.ht/query")))

(defun kakafarm/percent-read ()
  "Display percent read by current cursor location vs. total characters in file."
  (interactive)
  (message "%.2f%%"
           (* 100
              (/ (float (- (point) 1))
                 (+ 1 (buffer-size))))))

(defun kakafarm/sentence-end-double-nilify-for-read-only-buffers ()
  "Set `sentence-end-double-space' in read-only buffer to `nil'."
  (when buffer-read-only
    (setq-local sentence-end-double-space
                nil)))

(defun kakafarm/elfeed-sort-feeds ()
  `(setq elfeed-feeds
         ,(sort
           (mapcar
            (lambda (a-feed)
              (let* ((feed-url (car a-feed))
                     (tags (cdr a-feed))
                     (tags-as-strings (mapcar #'symbol-name
                                              tags))
                     (sorted-tags (sort tags-as-strings
                                        #'string-lessp))
                     (tags-as-symbols (mapcar #'intern sorted-tags)))
                (cons feed-url sorted-tags)))
            elfeed-feeds)
           (lambda (feed-a feed-b)
             (string-lessp (car feed-a)
                           (car feed-b))))))

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
