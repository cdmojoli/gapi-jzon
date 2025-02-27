(defsystem gapi-jzon
  :version "0.1.0"
  :author "Dmitrii Kosenkov (JZON variant by cdmojoli)"
  :license "MIT"
  :depends-on ("alexandria"
               "asn1"
               "cl-base64"
               "com.inuoe.jzon"
               "dexador"
               "ironclad"
               "jose"
               "pem")
  :description "Google Cloud API Client (JZON version)"
  :homepage "https://github.com/Junker/gapi"
  :source-control (:git "https://github.com/Junker/gapi.git")
  :components ((:file "package")
               (:file "time")
               (:file "trivia-jzon")
               (:file "gapi")))
