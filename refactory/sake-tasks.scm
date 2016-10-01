(module sake-tasks
  ((define-task add-task! make-task) run-task current-sheduled-task task-lambda no-deps depends-on)
  (import chicken scheme)
  (use srfi-69
       matchable
       (only posix current-directory glob)
       (only srfi-1 append-map reverse! delete-duplicates fold)
       (only srfi-13 string-append substring string-length)
       (only extras fprintf pp read-line)
       (only data-structures flatten topological-sort constantly))
  (import-for-syntax matchable)

  (define current-sheduled-task (make-parameter #f))

  (define *tasks* (make-hash-table))

  (define-record task name action deps documentation)

  (define-record-printer (task t out)
    (fprintf out "#task<~a - ~a>"
             (task-name t)
             (excerpt (or (task-documentation t) ":nodoc"))))

  (define (excerpt str #!optional (len 30))
    (if (> (string-length str) len)
        (string-append (substring str 0 len) "...")
        str))

  (define (task-dependencies task)
    (let ((deps (task-deps task)))
      (deps)))

  (define (add-task! name task)
    ;; TODO if override of tasks is disallowed
    ;; raise an error
    (hash-table-set! *tasks* name task))

  (define (run-task task . args)
    (cond
     ((symbol? task)
      (let ((task (hash-table-ref/default *tasks* task #f)))
        (if task
            (apply run-task task args)
            (signal
             (make-composite-condition
              (make-property-condition
               'exn
               'message "Could not find given task"
               'arguments (list task))
              (make-property-condition 'sake)
              (make-property-condition 'task-not-found task))))))
     ((task? task)
      (run-tasks (resolve-dependencies task) args))
     (else
      (error "Invalid argument given. Exepected symbol or task" task))))

  (define (run-tasks tasks args)
    ;; dependencies have been resolved at this point and
    ;; are guaranteed to be in the correct order, containing no cycles

    ;; TODO: think about passing the result of one task to the other
    ;; What would be the seed?
    (let ((run (lambda (task) ((task-action task)))))
      (for-each run tasks)))

  (define (resolve-dependencies task)
    (reverse! (topological-sort (dependency-graph task) eq?)))

  (define (task-direct-dependencies task)
    (delete-duplicates (task-dependencies task)))

  (define (dependency-graph task)
    (dependency-graph-helper task (list) (make-hash-table eq?)))

  (define (dependency-graph-helper task graph visited)
    (when (hash-table-ref/default visited (task-name task) #f)
      (signal
       (make-composite-condition
        (make-property-condition
         'exn
         'message "Detected cyclic dependencies"
         'arguments (list task graph))
        (make-property-condition 'sake)
        (make-property-condition 'cycle task graph))))
    (let* ((deps (task-direct-dependencies task))
           (node (cons task deps)))
      (cond
       ((null? deps) (cons node graph))
       (else
        (hash-table-set! visited (task-name task) #t)
        (cons node (fold (lambda (task graph)
                           (dependency-graph-helper task graph visited))
                         graph
                         deps))))))

  ;; the following macros are just convenience. You don't need these
  ;; you can just as well work with make-task
  ;; example:
  ;; (define t (make-task 'test (lambda () (print "test task")) (no-deps) "test task"))
  ;; (define t2 (make-task 'test (lambda () (print "test2 task")) (depends-on t) "test2 task")
  ;; use these procedures if you have difficulties with the macros, for example
  ;; when you want to compose tasks

  ;; (define-task name dep-proc doc-string proc)
  ;; (define-task name dep-proc proc)

  (define-syntax define-task
    (syntax-rules ()
      ((_ ?name ?dep-proc ?doc ?proc)
       (begin
         (define ?name (make-task '?name ?proc ?dep-proc ?doc))
         (add-task! '?name ?name)))
      ((_ ?name ?dep-proc ?proc)
       (define-task ?name ?dep-proc #f ?proc))
      ((_ ...)
       (syntax-error "Invalid syntax for define-task"))))

  ;; this can be used to create anonymous tasks
  ;; in dependency procedures
  ;; (task-lambda (dep ...) (arg ...) code ...)
  (define-syntax task-lambda
    (ir-macro-transformer
     (lambda (exp inj cmp)
       (match exp
         ((_ ?dep-proc  ?lambda-list ?code ?more-code ...)
          `(make-task ,(quote (gensym "task"))
                      (lambda ,?lambda-list ,?code ,@?more-code)
                      ,?dep-proc "task-lambda"))
         (else (syntax-error "Invalid syntax for task-lambda"))))))


  (define (no-deps) (lambda () (list)))
  (define (depends-on . tasks) (lambda () tasks))

)


;; example usage
;;

(import sake-tasks)


;; example
(define-task foo (no-deps)
  "This is my description"
  (lambda ()
    (print "foo called")))

(define-task bar (depends-on foo)
  (lambda ()
    (print "bar called")))

(define (baz-deps)
  (list bar foo))

(define-task baz baz-deps
  "depends on foo and bar"
  (lambda ()
    (print "baz called")))

(define (dynamic-tasks)
  (list (task-lambda (no-deps) () (print "I'm a dynamically generated task"))))

(define-task bar-baz dynamic-tasks
  "it generates dependent tasks on the fly"
  (lambda ()
    (print "foo")))

(run-task baz)
(print "================")
(run-task bar)
(print "================")
(run-task bar-baz)
