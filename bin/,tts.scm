#!/usr/bin/env sh
exec guile -s "$0" "$*"
!#

(use-modules
 (scheme base)

 (srfi srfi-11)

 (ice-9 pretty-print)
 (ice-9 match)
 (ice-9 popen)

 (json)
 )

(define debug? #f)
(define output-file-port (when debug? (open-file "/home/yuval/tts.scm-command-line.log" "a")))

(define piper-server-address "http://localhost:5000")

(define (debug x)
  (when debug?
    (with-output-to-port output-file-port
      (lambda ()
        (pretty-print x)))
    (flush-all-ports)))

(define (tts input-text)
  (debug input-text)
  (let ((commands `(("curl"
                     "--silent"
                     "--request" "POST"
                     "--header" "Content-Type: application/json"
                     "--data"
                     ,(scm->json-string `((text . ,input-text)))
                     ,piper-server-address)
                    ("aplay" "-")))
        (success? (lambda (pid)
                    (zero?
                     (status:exit-val (cdr (waitpid pid)))))))
    (let-values (((pipeline-input-port pipeline-output-port pids)
                  (pipeline commands)))
      (close pipeline-input-port)
      (close pipeline-output-port)
      (debug 'FIN))))


(match (command-line)
  ((arg0 arg1)
   (tts-pipeline arg1)))
