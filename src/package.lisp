(defpackage #:telnet-stream
  (:use #:common-lisp
        #:sb-bsd-sockets)
  (:import-from #:sb-unix
                #:unix-simple-poll)
  (:export #:telnet-stream
           #:telnet-stream-socket
           #:process-iac
           #:send-iac))
