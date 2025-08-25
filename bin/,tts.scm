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

(define piper-server-address "http://localhost:5000")

(define (tts input-text)
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
      (for-each waitpid pids))))


(match (command-line)
  ((arg0 arg1)
   (tts arg1)))
