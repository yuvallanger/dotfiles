;;; -*- lexical-binding:t -*-

(require 'cl-lib)

(defun kakafarm/advice-remove-all (function)
  "Remove every advice function from FUNCTION."
  (advice-mapc
   (lambda (advice-function properties-alist)
     (advice-remove function
                    advice-function))
   function))

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

(defun kakafarm/drop-while (lst predp)
  (named-let loop ((lst lst))
    (cond
     (()
      lst)
     ((predp (car lst))
      (cdr lst))
     (t
      (loop (cdr lst))))))

(defun kakafarm/easy-underscore (arg)
  "Convert all inputs of semicolon to an underscore
If given ARG, then it will insert an actual semicolon.

from https://www.youtube.com/watch?v=6R-73hsL5wk"
  (interactive "P")
  (message (format "%s" arg))
  (if arg
      (insert ";")
    (insert "_")))

(defun kakafarm/elfeed-sort-feed-tags (a-feed)
  (cond
   ((stringp a-feed)
    a-feed)
   (t
    (let* ((feed-url (car a-feed))
           (tags (cdr a-feed))
           (tags-as-strings (mapcar #'symbol-name
                                    tags))
           (sorted-tags (sort tags-as-strings
                              #'string-lessp))
           (tags-as-symbols (mapcar #'intern sorted-tags)))
      (cons feed-url tags-as-symbols)))))

(defun kakafarm/elfeed-compare-feeds-urls (feed-a feed-b)
  (string-lessp (car feed-a)
                (car feed-b)))

(defun kakafarm/elfeed-sort-feeds (feeds)
  "Sort A-FEED, an `elfeed-feeds' list."
  (sort (mapcar #'kakafarm/elfeed-sort-feed-tags
                feeds)
        #'kakafarm/elfeed-compare-feeds-urls))

(defun kakafarm/ffap-browse-urls ()
  "Open all visible URLs."
  (interactive)

  (let* ((urls (mapcar 'car (ffap-menu-rescan)))
         (urls-newlined (mapcar (lambda (url) (concat url "\n"))
                                urls))
         (prompt (format "Open URLs? [y/n]

%s"
                         (apply 'concat
                                urls-newlined))))
    (when (y-or-n-p prompt)
      (dolist (url urls)
        (browse-url url)))))

(defun kakafarm/kill-ring-save-unlines ()
  "Like `kill-ring-save', but also unlines and trims the newly killed stuff."
  (interactive)

  (kill-ring-save (point) (mark))

  (with-temp-buffer
    (yank-pop)
    (goto-char 1)
    (replace-regexp "\n+" " ")
    (let ((trimmed (string-trim (buffer-string))))
      (with-temp-buffer
        (insert trimmed)
        (kill-region (point-min) (point-max))))))

(defun kakafarm/list-all-http-or-https ()
  (interactive)
  (dolist (url (let* ((list-of-lines
                       (split-string (substring-no-properties (buffer-string))
                                     "[ \n]")))
                 (cl-reduce (lambda (accumulator line)
                              (if (string-match-p "https?://.+"
                                                  line)
                                  (cons line accumulator)
                                accumulator))
                            list-of-lines
                            :initial-value '())))
    (message "%s" url)))

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

(defun kakafarm/percent-read ()
  "Display percent read by current cursor location vs. total characters in file."
  (interactive)
  (message "%.2f%%"
           (* 100
              (/ (float (- (point) 1))
                 (+ 1 (buffer-size))))))

(defun kakafarm/recenter-top-bottom (original-function &rest arguments)
  "Move view such that point is 4 lines from the top of the frame when function is `recenter-top-bottom'."

  (cond
   ((null (car arguments))
    (apply original-function '(4)))
   (t
    (apply original-function arguments))))

(defun kakafarm/pulse-current-region (&rest _)
  "Pulse the selected bit, either the marked region or if there's no
mark, the bit between mark and point... or something like
that... I don't even work here.

From Goparism (https://www.youtube.com/@goparism)
https://www.youtube.com/watch?v=oQ9JE9kRwG8 which is from the
user 0xMii on R***** which copied it from
who-knows-where-and-who."
  (if mark-active
      (pulse-momentary-highlight-region (region-beginning)
                                        (region-end))
    (pulse-momentary-highlight-region (mark)
                                      (point))))

(defun kakafarm/multi-vterm-weechat ()
  "Either start a weechat vterm buffer, or switch to it if it already exists."

  (interactive)

  (let* ((vterm-shell (expand-file-name "~/bin/w"))
         (weechat-buffer-name "weechat")
         (maybe-weechat-buffer (get-buffer "weechat")))
    (cond
     ((multi-vterm-buffer-exist-p maybe-weechat-buffer)
      (switch-to-buffer maybe-weechat-buffer))
     (t
      (multi-vterm)
      (rename-buffer weechat-buffer-name)))))

(defun kakafarm/sentence-end-double-nilify-for-read-only-buffers ()
  "Set `sentence-end-double-space' in read-only buffer to `nil'."
  (when buffer-read-only
    (setq-local sentence-end-double-space
                nil)))

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

(defun kakafarm/take-while (lst predp)
  (named-let loop ((lst lst)
                   (accumulator '()))
    (cond
     ((null lst)
      (reverse accumulator))
     ((predp (car lst))
      (reverse accumulator))
     (t
      (loop (cdr lst)
            accumulator)))))

(defun kakafarm/url-response-to-body (response)
  (cdr (kakafarm/drop-while
        (string-split response
                      "\n")
        (lambda (line)
          (not (string-blank-p line))))))

(defun kakafarm/yank-unlines ()
  "`yank' with each consecutive newlines converted to a single space, and trim both ends."
  (interactive)

  (insert
   (string-trim
    (with-temp-buffer
      (yank)
      (goto-char 1)
      (replace-regexp "\n+" " ")
      (buffer-string)))))
