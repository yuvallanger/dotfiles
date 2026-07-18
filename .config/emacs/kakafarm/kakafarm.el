;;; kakafarm.el --- Various functions for my GNU Emacs configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Yuval Langer

;; Author: Yuval Langer <yuval.langer@gmail.com>
;; Version: 0.0.0
;; Keywords: Personal, Auxiliary
;; URL: https://codeberg.org/kakafarm/dotfiles/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Do not expect any of these functions to remain stable.
;;
;; The functions are ordered in an alphabetic order.  I do not know
;; if it makes much sense, but so be it.

;;; Code:

(require 'cl-lib)

(defun kakafarm/substack-video-transcript ()
  (interactive)
  (save-excursion
    (set-buffer (get-buffer-create "*substack-video-transcript*"))
    (insert (read-string "Please enter base64 of transcript: "))
    (let* ((thing (read-string "Please enter base64 of transcript: "))
           (thing (base64-decode-string thing))
           (thing (json-parse-string thing))
           (thing (cl-map 'list (lambda (text)
                                  (cl-map 'list
                                          (lambda (x)
                                            (list (gethash "speaker" x)
                                                  (gethash "word" x)))
                                          (gethash "words" text)))
                          thing))
           (thing (apply 'append thing))
           (thing
            (named-let loop ((words thing)
                             current-speaker
                             accumulator)
              (cond
               ((null words) (apply 'concat (reverse accumulator)))
               (t
                (loop (cdr words)
                      (cl-first (car words))
                      (cons
                       (cond
                        ((equal (cl-first (car words)) current-speaker)
                         (cl-second (car words)))
                        (t
                         (concat
                          "\n\n"
                          (cl-first (car words))
                          ":\n\n"
                          (cl-second (car words)))))
                       accumulator
                       )))))))
      thing
      )))

(defun kakafarm/greaderise-weechat-log ()
  (interactive)
  (let ((old-speaker nil)
        (original-buffer (current-buffer))
        (new-buffer (get-buffer-create "*Weechatlog*")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^........................ <.\\([a-zA-Z0-9]+\\)> \\(.*\\)" nil t)
        (let ((new-speaker (match-string 1))
              (new-message (match-string 2)))
          (when (not (string-equal new-speaker old-speaker))
            (setq old-speaker new-speaker)
            (with-current-buffer new-buffer
              (insert "Nickname " old-speaker " said:\n\n")))
          (with-current-buffer new-buffer
            (insert new-message "\n\n"))))
      (with-current-buffer new-buffer
        (fill-region (point-min) (point-max)))
      (switch-to-buffer new-buffer)
      (goto-char (point-min))
      (greader-mode 1))))

(defun kakafarm/convert-hash-tags-to-haunt-tags ()
  (interactive)
  (unless (use-region-p) (error "A region must be marked."))
  (let ((beginning (region-beginning))
        (end (region-end)))
    (save-excursion
      (goto-char beginning)
      (replace-string "#" "" nil 0 1)
      (while (search-forward "#" nil t)
        (replace-match "" nil nil nil 0))
      (goto-char beginning)
      (while (search-forward "\n" nil t)
        (replace-match ", " nil nil nil 0))
      (goto-char (- (point-at-eol) 2))
      (while (search-forward ", " nil t)
        (replace-match "" nil t)))))

(defun kakafarm/convert-haunt-tags-to-hash-tags ()
  (interactive)
  (unless (use-region-p) (error "A region must be marked."))
  (let ((beginning (region-beginning))
        (end (region-end)))
    (save-excursion
      (goto-char beginning)
      (insert "#")
      (while (search-forward ", " nil t)
        (replace-match "\n#" nil t nil 0)))))

(defun kakafarm/programmatic-replace-regexp (regexp to-string)
  (while (re-search-forward regexp nil t)
    (replace-match to-string nil nil)))

(defun kakafarm/programmatic-replace-string (from-string to-string)
  (while (search-forward from-string nil t)
    (replace-match to-string nil t)))

(defun kakafarm/denote-export ()
  (interactive)
  (let* ((source-directory (expand-file-name "~/foo/kaka.farm/src/denote/"))
         (target-html-directory (expand-file-name "~/foo/kaka.farm/site/pub/denote/html/"))
         (target-org-directory (expand-file-name "~/foo/kaka.farm/site/pub/denote/src/")))
    (mkdir target-html-directory t)
    (mkdir target-org-directory t)
    (cl-loop
     for source-org-file-name in (directory-files source-directory)
     if (string-suffix-p ".org" source-org-file-name)
     do (let* ((source-org-file-path (concat source-directory source-org-file-name))
               (target-org-file-path (concat target-org-directory source-org-file-name))
               (target-html-file-path (concat target-html-directory
                                              (file-name-base source-org-file-name)
                                              ".html"))
               (source-org-file-modification-time (file-attribute-modification-time (file-attributes source-org-file-path)))
               (target-org-file-modification-time (file-attribute-modification-time (file-attributes target-org-file-path))))
          (when (or (not target-org-file-modification-time)
                    (time-less-p target-org-file-modification-time source-org-file-modification-time))
            (save-excursion
              (let* ((source-org-buffer (find-file-noselect source-org-file-path)))
                (with-current-buffer source-org-buffer
                  (let* ((exported-html-file-name (org-html-export-to-html))
                         (exported-html-file-path exported-html-file-name))
                    (rename-file exported-html-file-path target-html-file-path t))))
              (copy-file source-org-file-path target-org-file-path t)
              (dolist (path (list source-org-filepath
                                  target-html-file-path
                                  target-org-file-path))
                (set-file-modes path #o644))))))))

(defun kakafarm/double-spaceify-period ()
  (interactive)
  (save-excursion
    (kakafarm/programmatic-replace-string ". "
                                          ".   "))
  (save-excursion
    (while (search-forward ".   " nil t)
      (replace-match ".  " nil t)
      (while (search-forward ".   " nil t)
        (replace-match ".  " nil t))
      (goto-char (point-min)))))

(defun kakafarm/double-spaceify-question-mark ()
  (interactive)
  (save-excursion
    (kakafarm/programmatic-replace-string "? "
                                          "?   "))
  (save-excursion
    (while (search-forward "?   " nil t)
      (replace-match ".  " nil t)
      (while (search-forward ".   " nil t)
        (replace-match ".  " nil t))
      (goto-char (point-min)))))

(defun kakafarm/double-spaceify ()
  (interactive)
  (kakafarm/double-spaceify-period)
  (kakafarm/double-spaceify-question-mark))

(defun kakafarm/youtube-transcript-untimestamps ()
  (interactive)
  (save-excursion (kakafarm/programmatic-replace-regexp "\n[[:digit:]]+:[[:digit:]]+:[[:digit:]]+\n" " "))
  (save-excursion (kakafarm/programmatic-replace-regexp "\n[[:digit:]]+:[[:digit:]]+\n" " "))
  (save-excursion (kakafarm/double-spaceify))
  (save-excursion (kakafarm/programmatic-replace-string ".  " ".\n"))
  (save-excursion (kakafarm/programmatic-replace-string "?  " "?\n"))
  (save-excursion (while (= (forward-paragraph) 0) (fill-paragraph)))
  )

;;;###autoload
'(defun kakafarm/0x0-region (&optional start end)
  "Paste the marked region onto 0x0.st.

START and END are the start and end of the region in the buffer
you would like to paste to 0x0.st."
  (interactive "r")
  '(cl-letf ((make-part (lambda (boundary key-values &optional body content-type)
                         (concat part-boundary
                                 "Content-Disposition: form-data; "

                                 "name=\"file\"; filename=\"buffer-region.txt\"\r\n"
                                 "Content-Type: text/plain\r\n"
                                 "\r\n"
                                 body)
                         (concat )))))
  (let* ((start (or start (min (point) (mark))))
         (end (or end (max (point) (mark))))
         (data (buffer-substring-no-properties start end))
         (boundary (format "%X-%X-%X"
                           (random)
                           (random)
                           (random)))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . ,(concat "multipart/form-data; boundary=" boundary))))
         (part-boundary (concat "--" boundary "\r\n"))
         (secret-part
          (concat
           part-boundary
           "Content-Disposition: form-data; name=\"secret\"\r\n\r\n"))
         (data-part (concat part-boundary
                            "Content-Disposition: form-data; name=\"file\"; filename=\"buffer-region.txt\"\r\n"
                            "Content-Type: text/plain\r\n"
                            "\r\n"
                            data))
         (end-boundary (concat "\r\n--" boundary "--"))
         (url-request-data (concat
                            secret-part
                            data-part
                            end-boundary
                            )))
    (with-current-buffer (url-retrieve-synchronously "https://0x0.st/")
      (let* ((response (buffer-string))
             (url (cadr (string-split response "\n\n" t "\n"))))

        (message "%s" response)

        ;; Displays URL.
        (message "0x0 URL: %s" url)

        ;; Copies URL to clipboard.
        (with-temp-buffer
          (insert url)
          (kill-region (point-min) (point-max)))))))

(defun kakafarm/0x0-region (&optional start end)
  "Paste the marked region onto 0x0.st.

START and END are the start and end of the region in the buffer
you would like to paste to 0x0.st."
  (interactive "r")
  (let* ((start (or start (min (point) (mark))))
         (end (or end (max (point) (mark))))
         (filename (make-temp-file "0x0-prefix-" nil)))
    (write-region start end filename)
    (let* ((it (format "curl -Fsecret=poop -Ffile=@%s 0x0.st 2> /dev/null | grep http" filename))
           (it (shell-command-to-string it))
           (it (string-clean-whitespace it))
           (address-of-0x0 it))
      (message "0x0 URL: %s" address-of-0x0)
      (with-temp-buffer
        (insert address-of-0x0)
        (kill-region (point-min) (point-max))))))

;;;###autoload
(defun kakafarm/advice-remove-all (function)
  "Remove every advice function from FUNCTION."
  (advice-mapc
   (lambda (advice-function _properties-alist)
     (advice-remove function
                    advice-function))
   function))

(defun kakafarm/ansi-color-filter-buffer ()
  (interactive)
  "Remove ANSI color from buffer.

TODO: Is it only working in the narrowed part?"
  (ansi-color-filter-region (point-min) (point-max)))

;;;###autoload
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

;; (list (kakafarm/call-process-with-string-as-input "cat"
;;                                                   "cat says moo")
;;       (kakafarm/call-process-with-string-as-input "echo"
;;                                                   nil
;;                                                   "-n"
;;                                                   "echo echo echo"))

;;;###autoload
(defun kakafarm/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'.

Used to convert raw ANSI color codes into visually (sometimes dis-)pleasing colors.

From:

https://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html"
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start
                                (point))))

;;;###autoload
(defun kakafarm/copy-elfeed-links ()
  (interactive)

  (cl-letf* ((elfeed-entry-to-url-nl (lambda (entry)
                                       (concat (elfeed-entry-link entry) "\n")))
             (all-urls-string (apply 'concat
                                     (mapcar elfeed-entry-to-url-nl
                                             (elfeed-search-selected)))))
    (with-temp-buffer
      (insert all-urls-string)
      (kill-region (point-min)
                   (point-max)))))

;;;###autoload
(defun kakafarm/drop-while (lst predp)
  (named-let loop ((lst lst))
    (cond
     (()
      lst)
     ((predp (car lst))
      (cdr lst))
     (t
      (loop (cdr lst))))))

;;;###autoload
(defun kakafarm/easy-underscore (arg)
  "Convert all inputs of semicolon to an underscore
If given ARG, then it will insert an actual semicolon.

from https://www.youtube.com/watch?v=6R-73hsL5wk"
  (interactive "P")
  (message (format "%s" arg))
  (if arg
      (insert ";")
    (insert "_")))

;;;###autoload
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

;;;###autoload
(defun kakafarm/elfeed-compare-feeds-urls (feed-a feed-b)
  (string-lessp (car feed-a)
                (car feed-b)))

(defun kakafarm/elfeed-feeds-sort (elfeed-feeds)
  (sort (mapcar (lambda (feed)
                  (cond*
                   ((match* (cons url tags) feed)
                    (cons url (cl-remove-duplicates (sort tags))))
                   ((match* (stringp url) feed)
                    (list url))))
                elfeed-feeds)))

;;;###autoload
(defun kakafarm/elfeed-feeds-pretty-print-insert ()
  (interactive)
  (insert "(customize-set-value 'elfeed-feeds
                     '(")
  (let ((sorted-feeds (kakafarm/elfeed-feeds-sort elfeed-feeds)))
    (cl-loop
     for feed in sorted-feeds
     do
     (cond*
      ((match* (cons url tags) feed)
       (insert (format "\n                       (%S" url))
       (cl-loop
        for tag in tags
        do
        (insert (format " %s" tag)))
       (insert ")")))))
  (insert "\n                       ))"))

;;;###autoload
(defun kakafarm/elfeed-sort-feeds (feeds)
  "Sort A-FEED, an `elfeed-feeds' list."
  (sort (mapcar #'kakafarm/elfeed-sort-feed-tags
                feeds)
        #'kakafarm/elfeed-compare-feeds-urls))

(defun kakafarm/krepalakh-soju-password ()
  (interactive)
  (auth-info-password
   (nth 0 (auth-source-search
           :host "localhost"
           :user "krepalakh"
           :port "6667"))))

;;;###autoload
(defun kakafarm/erc-libera-chat ()
  (interactive)
  (let ((auth-source-debug 'trivia)
        (erc-sasl-user "krepalakh")
        (erc-sasl-auth-source-function 'erc-auth-source-search))
    (erc-tls :id 'libera-kakafarm)))

;;;###autoload
(defun kakafarm/erc-soju-krepalakh ()
  (interactive)
  (erc :id 'soju-krepalakh
       :server "localhost"
       :port 6667
       :nick "krepalakh"
       :user "krepalakh"
       :password (kakafarm/krepalakh-soju-password)
       ))

;;;###autoload
(defun kakafarm/erc-soju-libera ()
  (interactive)
  (erc :id 'libera-krepalakh
       :server "localhost"
       :port 6667
       :nick "krepalakh"
       :user "krepalakh/libera"
       :password (kakafarm/krepalakh-soju-password)
       ))

;;;###autoload
(defun kakafarm/erc-soju-quakenet ()
  (interactive)
  (erc :id 'quakenet-krepalakh
       :server "localhost"
       :port 6667
       :nick "krepalakh"
       :user "krepalakh/quakenet"
       :password (kakafarm/krepalakh-soju-password)
       ))

;;;###autoload
(defun kakafarm/erc-soju-rizon ()
  (interactive)
  (erc :id 'rizon-krepalakh
       :server "localhost"
       :port 6667
       :nick "krepalakh"
       :user "krepalakh/rizon"
       :password (kakafarm/krepalakh-soju-password)
       ))

;;;###autoload
(defun kakafarm/erc-soju ()
  (interactive)
  (kakafarm/erc-soju-krepalakh)
  (kakafarm/erc-soju-libera)
  (kakafarm/erc-soju-quakenet)
  (kakafarm/erc-soju-rizon)
  )

;;;###autoload
(defun kakafarm//ffap-browse-urls ()
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

;;;###autoload
(defun kakafarm/ffap-browse-urls ()
  "Open wanted visible URLs.  Ya gotta type a bit of em and then ta tab complete and separate by commas ta actually select em."
  (interactive)

  (with-restriction (window-start) (window-end)
    (let ((urls (mapcar 'car (ffap-menu-rescan)))
          (crm-separator "\n"))
      (let ((urls-to-open (completing-read-multiple "Which URLs should I open?
"
                                                    urls)))
        (dolist (url urls-to-open)
          (browse-url url))))))

;;;###autoload
(defun kakafarm/find-init-el ()
  (interactive)
  (find-file "~/.config/emacs/init.el")
  (emacs-lisp-mode))

(defun kakafarm/format-startstop-to-durations (start-stop-list)
  (mapcar (pcase-lambda (`(,start ,stop))
            (time-subtract
             (iso8601-parse stop)
             (iso8601-parse start)))
          start-stop-list))

;;;###autoload
(defun kakafarm/greader-estimate-reading-time (&optional start end)
  (interactive "r")
  (let ((wpm greader-espeak-rate)
        (number-of-words (count-words (if start start (point-min))
                                      (if end end (point-max)))))
    (message "%s" (/ (float number-of-words) wpm))))

;;;###autoload
(defun kakafarm/greaderize ()
  "Make the text in the current buffer friendlier to greader.

TODO: This is shite."
  (interactive)
  (save-excursion
    (whitespace-cleanup)
    (goto-char (point-min))
    ;; Replace all consecutive newline sequences with exactly two
    ;; newlines.
    (while (search-forward "\n" nil t)
      (replace-match "\n\n" nil t))
    (goto-char (point-min))
    (while (search-forward "\n\n\n" nil t)
      (replace-match "\n\n" nil t)
      (while (search-forward "\n\n\n" nil t)
        (replace-match "\n\n" nil t))
      (goto-char (point-min)))
    (fill-region (point-min) (point-max))))

;;;###autoload
(defun kakafarm/insert-char-to-vterm ()
  (interactive)
  (vterm-insert (read-char-by-name "Choose char: ")))


;;; `kakafarm/insert-mu', `kakafarm/insert-mu-history', and
;;; `kakafarm/insert-mu-to-vterm', are based on the following post
;;; by Corwin Brust:
;;;
;;; https://corwin.bru.st/2025-09-18-emacsconf-cfp-ending-and-a-completing-read-example/
(defvar kakafarm/insert-mu-history nil "History for `kakafarm/insert-mu'.")

(defcustom kakafarm/insert-mu-mus '(("mu" . "無"))
  "Things for `kakafarm/insert-mu' to insert, plist.")

(defmacro kakafarm/insert-mu-macro (name insertion-function)
  `(defun ,name (&optional arg)
     (interactive
      (list (completing-read "Emit: "
                             kakafarm/insert-mu-mus
                             nil
                             t
                             nil
                             kakafarm/insert-mu-history
                             "mu")))
     (,insertion-function (format "%s" (or (alist-get arg
                                                      kakafarm/insert-mu-mus
                                                      nil
                                                      nil
                                                      'string=)
                                           "")))))

(kakafarm/insert-mu-macro kakafarm/insert-mu insert)

(kakafarm/insert-mu-macro kakafarm/insert-mu-to-vterm vterm-insert)


;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun kakafarm/org-create-ids ()
  "Create ID properties to each header in the file."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (let ((p t))
      (while p
        (let ((x (outline-previous-heading)))
          (when (not x) (setq p x))
          (org-id-get-create))))))

;;;###autoload
(defun kakafarm/org-html-export-to-html-file (file)
  (let ((buffer (org-export-as 'html)))
    (with-temp-buffer
      (insert buffer)
      (write-file file t))))

;;;###autoload
(defun kakafarm/org-roam-keyword-is-filetags-p (keyword-node)
  (equal (org-element-property :key
                               keyword-node)
         "FILETAGS"))

;;;###autoload
(defun kakafarm/org-roam-filetags-keyword-is-publishable-p (filestags-keyword-node)
  (seq-contains-p (split-string (org-element-property :value
                                                      filestags-keyword-node)
                                ":")
                  "publish"))

;;;###autoload
(defun kakafarm/org-roam-publishable-node-p (org-filename)
  (with-temp-buffer
    (insert-file-contents org-filename)
    (org-element-map
        (org-element-parse-buffer)
        'keyword
      (lambda (keyword)
        (and (kakafarm/org-roam-keyword-is-filetags-p keyword)
             (kakafarm/org-roam-filetags-keyword-is-publishable-p keyword)))
      nil
      t)))

;;;###autoload
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

;;;###autoload
(defun kakafarm/org-roam-publication-wrapper (plist filename pubdir)
  ;; (when (kakafarm/org-roam-publishable-node-p filename)
  ;;   nil)
  ;;(org-roam-graph) ; How the fuck do I make this one not show every fucking node in the org-roam database?!
  (org-html-publish-to-html plist
                            filename
                            pubdir)
  (setq kakafarm/org-roam-project-publish-time
        (cadr (current-time))))

;;;###autoload
(defun kakafarm/org-roam-custom-link-builder (node)
  (let ((node-file (org-roam-node-file node)))
    ;; (when (kakafarm/org-roam-publishable-node-p node-file)
    ;;   nil)
    (message (format "kakafarm/org-roam-custom-link-builder: %S" node))
    (concat (file-name-base node-file)
            ".html")))

;;;###autoload
(defun kakafarm/percent-read ()
  "Display percent read by current cursor location vs. total characters in file."

  (interactive)

  (message "%.2f%%"
           (* 100
              (/ (float (- (point) 1))
                 (+ 1 (buffer-size))))))

;;;###autoload
(defun kakafarm/percent-read-point-min-max ()
  "Display percent read by current cursor location vs. place within (point-min) and (point-max)."

  (interactive)

  (let* ((our-location (point))
         (our-location-0-indexed (- our-location 1)))
    (message "%.2f%%"
             (* 100
                (/ (float (- our-location-0-indexed
                             (point-min)))
                   (- (point-max)
                      (point-min)))))))

(defun kakafarm/goto-random-line ()
  (interactive)
  (let* ((beginning (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (first-line-number (line-number-at-pos beginning))
         (number-of-possible-lines (count-lines beginning end))
         (random-line-number (+ (random number-of-possible-lines)
                                first-line-number)))
    (message "use-region-p: %s
beg: %s
end: %s
first-line-number: %s
number-of-possible-lines: %s
random-line-number: %s"
             (use-region-p)
             beginning
             end
             first-line-number
             number-of-possible-lines
             random-line-number)
    (goto-line random-line-number)))

(defun kakafarm/random-shuffle (number-or-lst &optional print-message)
  "Return a shuffled list.

NUMBER-OR-LIST can be either a number or a list.

If NUMBER-OR-LIST is a number, the function returns a shuffled list
containing all integer numbers from 0 to NUMBER-OR-LIST, not including
NUMBER-OR-LIST.

If NUMBER-OR-LIST is a list, the result would be a shuffled shallow copy
of NUMBER-OR-LIST.

If called interactively, the user is prompted for a number.  A message
is printed showing the shuffled list of all integer numbers from 0 to
the entered number, not including the entered number.
"
  (interactive "nNumber of options: \np")
  (let* ((number-or-lst (if (numberp number-or-lst)
                            (cl-loop for i to (1- number-or-lst) collect i)
                          number-or-lst))
         (len (length number-or-lst))
         (tmp (cl-copy-list number-or-lst))
         new-lst
         (result
          (dotimes (i len new-lst)
            (let* ((random-index (cl-random (length tmp)))
                   (random-element (elt tmp random-index)))
              (setq tmp (seq-remove-at-position tmp random-index))
              (setq new-lst (cons random-element new-lst))))))
    (when print-message
      (message "kakafarm/random-shuffle: %s" result))
    result))

;;;###autoload
(defun kakafarm/recenter-top-bottom (original-function &rest arguments)
  "Move view such that point is 4 lines from the top of the frame when function is `recenter-top-bottom'."

  (cond
   ((null (car arguments))
    (apply original-function '(4)))
   (t
    (apply original-function arguments))))

;;;###autoload
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

;;;###autoload
(defun kakafarm/mastodon-thread-item-source ()
  (let* ((x (mastodon-tl--property 'item-json))
         (x (assq 'pleroma x))
         (x (assq 'content x))
         (x (assq 'text/plain x))
         (x (cdr x)))
    x))

;;;###autoload
(defun kakafarm/mastodon-thread-item-source ()
  (interactive)
  (let ((item-source (kakafarm/mastodon-get-thread-item-source)))
    (with-current-buffer (get-buffer-create "*pleroma-item-source*")
      (erase-buffer)
      (insert item-source))))

;;;###autoload
(defun kakafarm/multi-vterm-weechat ()
  "Either start a weechat vterm buffer, or switch to it if it already exists."

  (interactive)

  (let* ((weechat-buffer-name "weechat")
         (maybe-weechat-buffer (get-buffer weechat-buffer-name)))
    (cond
     ((multi-vterm-buffer-exist-p maybe-weechat-buffer)
      (switch-to-buffer maybe-weechat-buffer))
     (t
      (let ((vterm-shell (expand-file-name "~/bin/,w")))
        (multi-vterm)
        (rename-buffer weechat-buffer-name))))))

;;;###autoload
(defun kakafarm/sentence-end-double-nilify-for-read-only-buffers ()
  "Set `sentence-end-double-space' in read-only buffer to `nil'."
  (when buffer-read-only
    (setq-local sentence-end-double-space nil)))

;;;###autoload
(defun kakafarm/set-truncate-partial-width-windows-to-nil ()
  (setq-local truncate-partial-width-windows nil))

;;;###autoload
(defun kakafarm/shell-command-with-string-to-string-XXX (string command)
  ;; XXX: Not good yet.
  "XXX: Not good yet."
  ;; (interactive "Mstring: \nMcommand: ")
  (with-temp-buffer ;; start output-buffer.
    (let ((output-buffer (current-buffer)))
      (with-temp-buffer ;; start input-buffer.
        (insert string)
        (call-process "xsel"(point-min)
                      (point-max)
                      command
                      output-buffer)
        )
      ;; end input-buffer.
      ;; continue output-buffer.
      )
    (buffer-string)
    ) ;; end output buffer.
  )

;; Uploading README.html from README.org stuff.
;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun kakafarm/string-to-clipboard (str)
  "Takes an input string STR and sticks it into the clipboard using xsel -b."
  (interactive "Mstring: ")
  (with-temp-buffer
    (insert str)
    (shell-command-on-region (point-min)
                             (point-max)
                             "xsel -ib")))

;;;###autoload
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

;;;###autoload
(defun kakafarm/unfill-region (&optional start end)
  (interactive "r")
  (let ((start (if start start (point-min)))
        (end (if end end (point-max))))
    (let ((fill-column nil))
      (fill-region start end))))

;;;###autoload
(defun kakafarm/url-response-to-body (response)
  (cdr (kakafarm/drop-while
        (string-split response
                      "\n")
        (lambda (line)
          (not (string-blank-p line))))))

;;;###autoload
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

(provide 'kakafarm)

;;; Local Variables:
;;; read-symbol-shorthands: (("kf/" . "kakafarm/"))
;;; End:
;;; kakafarm.el ends here.
