(module procter
*

(import chicken scheme ports extras)

(use srfi-1 irregex)



(define (make-exn-condition loc msg . args)
  (make-property-condition 'exn 'location loc 'message msg 'arguments args))

(define (make-procter-condition type)
  (make-property-condition 'procter ' sta))

(define (make-procter-error-condition  type loc msg  . args)
  (make-composite-condition
   (apply make-exn-condition loc msg args)
   (make-property-condition 'procter)
   (make-property-condition type)))

(define (make-fully-qualified-reader path reader)
  (let ((reader (make-reader reader)))
    (lambda ()
      (reader path))))

(define ((make-reader reader #!key (path-prefix "")) path)
  (let ((full-path (string-append path-prefix path)))
    (unless (file-exists? full-path)
      (signal (make-procter-error-condition
               'path-not-found
               'proc-reader
               "The given path does not exist" full-path)))
    (call-with-input-file full-path reader)))

(define (port-map* io proc)
  (with-input-from-port io (cut port-map proc read-line)))

(define ((make-table-reader #!key (horizontal-headers #f) (use-this-headers #f) (vertical-headers #f) (separator '(+ whitespace))) io)
  (let* ((line-reader (make-table-line-reader separator)))
    (remove
     null?
     (port-map* io (cond
                    (horizontal-headers
                     (with-horizontal-headers line-reader (or use-this-headers (columnize (read-line io) separator))))
                    (vertical-headers
                     (with-vertical-headers line-reader))
                    (else line-reader))))))

(define (make-property-table-reader #!key (separator '(seq (+ whitespace) ":" (* whitespace))))
  (let ((line-reader (make-table-line-reader separator)))
    (lambda (io)
      (remove
       null?
       (port-map* io (with-property-headers line-reader))))))

(define ((make-table-line-reader separator) line)
  (columnize line separator))

(define (columnize line separator)
  (irregex-split separator line))

(define ((with-horizontal-headers reader headers) line)
  (zip headers (reader line)))

(define ((with-vertical-headers reader) line)
  (let ((line (reader line)))
    (if (null? line)
        line
        (cons (car line) (list (cdr line))))))

(define ((with-property-headers reader) line)
  (let ((line (reader line)))
    (if (null? line)
        line
        (cons (car line) (if (null? (cdr line)) #f (cdr line))))))


)

