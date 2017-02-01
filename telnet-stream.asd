(asdf:defsystem #:telnet-stream
  :description "Telnet stream"
  :author "Janis Dzerins <janis.dzerins@cert.lv>"
  :version "0.1"
  :licence "MIT"
  :long-description "Extended stream class that handles Telnet
  commands.  For now SBCL specific."
  :depends-on ((:require "sb-bsd-sockets"))
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "vars")
                             (:file "telnet-stream")))))
