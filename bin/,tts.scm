#!/usr/bin/env sh
exec guile -s "$0" "$*"
!#

(use-modules
 (scheme base)

 (srfi srfi-1)
 (srfi srfi-11)

 (ice-9 buffered-input)
 (ice-9 match)
 (ice-9 popen)
 (ice-9 pretty-print)

 (web client)

 (json)
 )

(define piper-server-address "http://localhost:5000")

(define (failed? pid)
  (not (zero? (status:exit-val (cdr (waitpid pid))))))

(define (tts input-text)
  ;; TODO: This one just hangs for some yet unknown reason.
  (let-values (((response-headers response-body)
                (http-post piper-server-address
                           #:body (scm->json-string `((text . ,input-text)))
                           #:headers '((Content-Type . "application/json"))
                           #:decode-body? #f
                           #:streaming? #t
                           )))
    (let* ((aplay-pid (spawn "aplay" '("aplay" "-")
                             #:input response-body)))
      (when (failed? aplay-pid)
        (format #t "Failed.~%")
        (exit EXIT_FAILURE)))))

(define (tts-pipeline input-text)
  (let ((commands `(("curl"
                     "--silent"
                     "--request" "POST"
                     "--header" "Content-Type: application/json"
                     "--data"
                     ,(scm->json-string `((text . ,input-text)))
                     ,piper-server-address)
                    ("aplay" "-"))))
    (let-values (((pipeline-input-port pipeline-output-port pids)
                  (pipeline commands)))
      (close pipeline-input-port)
      (close pipeline-output-port)
      (when (any failed? pids)
        (format #t "Failed.~%")
        (exit EXIT_FAILURE)))))

(match (command-line)
  ((arg0 "")
   (format #t "Usage:
    ~A [INPUT-TEXT]~%" arg0)
   (exit EXIT_FAILURE))
  ((arg0 input-text)
   (format #t "~S~%" input-text)
   (tts-pipeline input-text)))
