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
