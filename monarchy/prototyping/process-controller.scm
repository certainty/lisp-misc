;;  =license=
;;  =/license=


(define (start-controller program-name) #t)
(define (start-program path) #t)

(define (start-program path)
  (daemonize (program path)))

(define (start-controller)
  (daemonize (controller path)))

(define (daemonize program)
  (change-directory "/")
  (setup-descriptors)
  (let ((pid (fork program)))
    (unless (zero? pid)
      (exit 0))
    (create-session)
    (duplicate-fileno 1 2)))

(define (setup-descriptors)
  (let ((fd-r (file-open "/dev/null" open/rdonly))
        (fd-w (file-open "/dev/null" open/wronly)))
    (duplicate-fileno fd-r 0)
    (duplicate-fileno fd-w 1)
    (file-close fd-r)
    (file-close fd-w)))

(define (program path)
  ;; start the program

  )

(define (controller path)
  ;; start a controller that does the work

  )

(define (control-cli arguments)
  ;; run the controller cli
  #t
  )
