;;  =license=
;;  =/license=

(use zmq posix)

(let ((socket (make-socket 'push)))
  (connect-socket socket "tcp://127.0.0.1:5555")
  (connect-socket socket "tcp://127.0.0.1:5557")
  
  (send-message socket (sprintf  "Agent-Message"))
  ;(receive-message* socket)
  
  (close-socket socket))
