(defpackage gapi-jzon
  (:use #:cl #:alexandria)
  (:import-from #:local-time
                #:timestamp-to-unix
                #:now)
  (:import-from #:trivia
                #:access
                #:guard
                #:match)
  (:export #:auth
           #:client
           #:client-access-token
           #:client-access-token-expires-at
           #:client-access-token-expired-p
           #:client-authorized-p
           #:client-client-email
           #:client-project-id
           #:client-scopes
           #:client-token-uri
           #:defun-gapi
           #:gapi-error
           #:gapi-error-code
           #:gapi-error-details
           #:gapi-error-message
           #:gapi-error-status
           #:generate-jwt
           #:make-client-with-service-account
           #:request
           #:response
           #:&client
           #:&payload))
