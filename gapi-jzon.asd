(defsystem gapi-jzon
  :version "0.2.0"
  :author "C. Daniel Mojoli, based on Dmitrii Kosenkov's original work"
  :license "MIT"
  :depends-on ("alexandria"
               "asn1"
               "cl-base64"
               "com.inuoe.jzon"
               "dexador"
               "ironclad"
               "jose"
               "log4cl"
               "pem")
  :description "Google Cloud API Client (JZON version)"
  :source-control (:git "https://github.com/cdmojoli/gapi-jzon.git")
  :components ((:file "package")
               (:file "time")
               (:file "trivia-jzon")
               (:file "gapi")
               (:file "defun-gapi")))
