
(module system-peep
*

(import chicken scheme foreign data-structures extras)
(use srfi-69 srfi-1 srfi-13 foreigners procter)

(define *facts* (make-hash-table))

(define (register-fact-retriever! key thunk)
  (unless (procedure? thunk)
    (error "Expected a procedure" thunk))
  (unless (symbol? key)
    (error "Expected symbol" key))
  (hash-table-set! *facts* key thunk))



(define-syntax define-fact-retriever
  (syntax-rules ()
    ((_ (name) body more-body ...)
     (register-fact-retriever! (quote name)
                               (lambda ()
                                 body more-body ...)))))

(define-syntax define-fact-retriever/m
  (syntax-rules ()
    ((_ (name) body more-body ...)
     (register-fact-retriever! (quote name)
                               (let ((memo 'nil))
                                 (lambda ()
                                   (when (eq? memo 'nil)
                                     (set! memo (begin body more-body ...)))
                                   memo))))))

(define-syntax define-fact-retriever/c
  (syntax-rules ()
    ((_ (name) body more-body ...)
     (register-fact-retriever! (quote name) (constantly  body more-body ...)))))


(define (facts)
  (hash-table-map *facts*
                  (lambda (key retriever)
                    (cons key (retriever)))))

(define (fact key)
  (let ((retriever (hash-table-ref *facts* key)))
    (and retriever (retriever))))

(register-fact-retriever! 'operatingsystem
                          (cond-expand
                            (linux   (constantly "linux"))
                            (macosx  (constantly "macosx"))
                            (freebsd (constantly "freebsd"))
                            (openbsd (constantly "openbsd"))
                            (netbsd  (constantly "netbsd"))
                            (else    (constantly #f))))

(register-fact-retriever! 'hardwaremodel
                          (cond-expand
                            (x86-64 (constantly "x86_64"))
                            (else   (constantly #f))))

(cond-expand
  (linux
   (include "linux.scm"))
  (macosx
   (include "macosx.scm"))
  (win32
   (include "window.scm"))
  ((or netbsd openbsd freebsd)
   (include "bsd.scm"))
  (else (error "Your system is not supported")))



)
