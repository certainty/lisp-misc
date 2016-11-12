(foreign-declare #<<EOF
 #include<unistd.h>
 #include <sys/utsname.h>
 #include <sys/types.h>
 #include <sys/socket.h>
 #include <netdb.h>
EOF
)

(define proc* (make-reader (make-table-reader)))

(define (proc-h* path #!key (headers #f))
  ((make-reader (make-table-reader horizontal-headers: #t use-this-headers: headers)) path))

(define (proc-v* path)
  ((make-reader (make-table-reader vertical-headers: #t)) path))

;; partitions a list of the type
(define (partition-at key collection)
  (let loop ((coll collection) (current-part (list)) (all-parts (list)))
    (cond
     ((null? coll)
      (reverse
       (if (null? current-part) all-parts (cons current-part all-parts))))
     ((string-ci=? (caar coll) key)
      (if (null? current-part)
          (loop (cdr coll) current-part all-parts)
          (loop (cdr coll) (list) (cons current-part all-parts))))
     (else
      (loop (cdr coll) (cons (car coll) current-part) all-parts)))))

(define (column key #!optional (matches? string-ci=?))
  (lambda (ls)
    (matches? key (car ls))))

;; uptime
(define-fact-retriever (uptime)
  (proc-h* "/proc/uptime" headers: '("total" "idle")))

;; cpu
(define cpu-info
  (make-fully-qualified-reader
   "/proc/cpuinfo"
   (make-property-table-reader)))

(define all-cpu-info (delay (cpu-info)))
(define per-cpu-info (delay (partition-at "processor" (force all-cpu-info))))

(define-fact-retriever/m (physical-cpu-amount)
  (let ((physical-ids (map cdr (filter (column "physical id") (force all-cpu-info)))))
    (length (delete-duplicates physical-ids))))

(define-fact-retriever/m (cpus)
  (let ((per-cpu (force per-cpu-info)))
    (zip (iota (length per-cpu)) per-cpu)))

;; memory

;; hostname
(define hostname
  (foreign-lambda* nonnull-c-string ()
    "char name[1024];"
    "gethostname(name,1023);"
    "C_return(name);"))

(define fqdn
  (foreign-lambda* c-string-list* ((c-string hostname))
    "struct addrinfo hints,*info,*p;"
    "int result,num = 0,recordamount = 0;"
    "char **fqdns;"
    "memset(&hints,0,sizeof(hints));"
    "hints.ai_family = AF_UNSPEC;"
    "hints.ai_socktype = SOCK_STREAM;"
    "hints.ai_flags = AI_CANONNAME;"
    "if((result = getaddrinfo(hostname,\"http\",&hints,&info))){"
    "  C_return(\"\"); "
    "}else{"
    "  for( p = info, recordamount = 0; p!=NULL; p = p->ai_next, recordamount++);"
    "  fqdns = malloc((recordamount+1) * sizeof(char*));"
    "  for( p = info; p!= NULL; p = p->ai_next ){"
    "    fqdns[num++] = strdup(p->ai_canonname);"
    "  }"
    "  fqdns[num++] = NULL;"
    "}"
    "freeaddrinfo(info);"
    "C_return(fqdns);"
    ))


(define-fact-retriever (hostname) (hostname))

(define-fact-retriever (fqdn)
  (fqdn (hostname)))

;; uname
(define-foreign-record-type (utsname "struct utsname")
  (constructor: make-utsname)
  (destructor: free-utsname)
  (c-string sysname  utsname-sysname)
  (c-string nodename utsname-nodename)
  (c-string release  utsname-release)
  (c-string version  utsname-version)
  (c-string machine  utsname-machine))

(define uname (foreign-lambda* utsname ()
                "struct utsname name;"
                "int ignored = uname(&name);"
                "C_return(&name);"))

(define-fact-retriever (kernel)
  (let ((utsname (uname)))
    (string-downcase (utsname-sysname utsname))))

(define-fact-retriever (kernel-release)
  (let ((utsname (uname)))
    (utsname-release utsname)))

;; load
(define-fact-retriever (load-average)
  (let ((load-total (proc* "/proc/loadavg")))
    load-total))




