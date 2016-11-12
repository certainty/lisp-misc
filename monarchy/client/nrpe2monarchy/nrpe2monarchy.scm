;; (module nrpe2monarchy
;;   (parse-nrpe-config-file)
;;   (import chicken scheme)
;;   (use comparse lazy-seq srfi-1 scsh-process)

(define white-space
  (in #\space #\tab))

(define command-name
  (enclosed-by (is #\[)
               (one-or-more (none-of* (is #\]) item))
               (is #\])))

(define command-contents
  (one-or-more item))

(define command-prefix
  (char-seq "command"))

(define nrpe-command
  (sequence
    command-prefix
    command-name
    (is #\=)
    command-contents))

(define comment
  (sequence
    (zero-or-more white-space)
    (is #\#)
    (zero-or-more item)))

(define (parse-comment input)
  (and (comment input) 'comment))

(define (parse-empty-line input)
  (and ((zero-or-more white-space) input) 'empty))

(define (parse-command input)
  (and-let* ((res (nrpe-command input))
             (matches (car res)))
     (cons (list->string (second matches))
           (list->string (fourth matches)))))

(define (parse-line line)
  (let ((input (list->lazy-seq (string->list line))))
    (or (parse-comment input)
        (parse-command input)
        (parse-empty-line input))))

(define (parse-nrpe-config-file filename)
  (with-input-from-file filename
    (lambda ()
      (remove
       (lambda (elt)
         (member elt '(empty comment)))
       (map parse-line (read-lines))))))

(define (call-with-timeout timeout proc)
  (proc))

;; executes the command in a subprocess with a timeout
(define (run-command cmdline)
  (receive (in out pid) (process cmdline)
    (let ((output (read-line in)))
      (receive (_ normal? status) (process-wait pid)
        (if normal?
            (cons status output)
            (cons 3 output))))))

;; executes the command and returns the result
;; as a triple holding the exit-code, the output and performance data
(define (execute-nrpe-command cmdline)
  (let ((command-name (car cmd))
        (cmdline (cdr cmd)))
    (run-command cmdline)))

;; runs all commands in the config file and collects the results
(define (run-nrpe configfile)
  (let ((commands (parse-nrpe-config-file configfile)))
    (map (lambda (cmd)
           (list (car cmd) (run-command (cdr cmd))))
     commands)))


;)
