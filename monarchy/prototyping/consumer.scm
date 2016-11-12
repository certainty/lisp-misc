;;  =license=
;;  =/license=

(use zmq posix)


(let ((socket (make-socket 'sub)))
  (connect-socket socket "tcp://127.0.0.1:5556")
  (connect-socket socket "tcp://127.0.0.1:5558")
  (socket-option-set! socket 'subscribe "")
  (let loop ((message (receive-message* socket)))
    (print "[C] message arrived: " message)
    (flush-output)
    (loop (receive-message* socket as: 'string))))
