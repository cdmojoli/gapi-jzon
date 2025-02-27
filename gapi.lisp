(in-package #:gapi-jzon)

(defparameter *jwt-token-expiry-length* 3600)

(define-condition gapi-error (error)
  ((code :initarg :code
         :reader gapi-error-code
         :type integer)
   (message :initarg :message
            :reader gapi-error-message
            :type string)
   (status :initarg :status
           :reader gapi-error-status
           :type string)
   (details :initarg :details
            :reader gapi-error-details))
  (:report
   (lambda (condition stream)
     (with-slots (code message status details) condition
       (format
        stream
        "Google API request failed with code ~D and status ~S: ~A~% Details:~%~S"
        code status message details)))))

(defclass client ()
  ((project-id :initarg :project-id :accessor client-project-id :type string)
   (private-key :initarg :private-key :accessor client-private-key
                :type ironclad:rsa-private-key)
   (client-email :initarg :client-email :accessor client-client-email
                 :type string)
   (token-uri :initarg :token-uri :accessor client-token-uri :type string)
   (scopes :initarg :scopes :accessor client-scopes :type list)
   (access-token :initarg nil :accessor client-access-token :initform nil)
   (access-token-expires-at :initarg :access-token-expires-at
                            :accessor client-access-token-expires-at
                            :type integer))
  (:default-initargs
   :project-id (error "PROJECT-ID required.")
   :private-key (error "PRIVATE-KEY required.")
   :client-email (error "CLIENT-EMAIL required.")
   :access-token-expires-at 0))

;; PRIV
(defun parse-service-account-file (path)
  (com.inuoe.jzon:parse (uiop:read-file-string path)))

(defun %generate-jwt (private-key client-email token-uri scopes expiry-length)
  (declare (ironclad:rsa-private-key private-key)
           (string client-email token-uri)
           (list scopes)
           (integer expiry-length)
           (values string))

  (jose:encode :rs256
               private-key
               `(("iss" . ,client-email)
                 ("iat" . ,(get-unix-time))
                 ("exp" . ,(+ (get-unix-time) expiry-length))
                 ("scope" . ,(format nil "~{~A~^ ~}" scopes))
                 ("aud" . ,token-uri))))

(defun %auth (token-url jwt)
  (declare (string token-url jwt)
           (values hash-table))

  (com.inuoe.jzon:parse
   (dex:post
    token-url
    :content
    (format
     nil
     "grant_type=urn:ietf:params:oauth:grant-type:jwt-bearer&assertion=~A"
     jwt)
    :headers '(("Content-Type" . "application/x-www-form-urlencoded")))))

;; PUBLIC

(defun make-client-with-service-account (path &key scopes)
  (declare (pathname path)
           (list scopes)
           (values client))

  (trivia:match (parse-service-account-file path)
    ((jzon project_id private_key client_email token_uri)
     (make-instance 'client
                    :project-id project_id
                    :private-key (pem:read-pem private_key)
                    :client-email client_email
                    :token-uri token_uri
                    :scopes scopes))))

(defmethod generate-jwt ((client client)
                         &key (expiry-length *jwt-token-expiry-length*))
  (declare (integer expiry-length)
           (values string))

  (with-slots (private-key client-email token-uri scopes) client
    (%generate-jwt private-key client-email token-uri scopes expiry-length)))

(defmethod auth ((client client))
  (trivia:match (%auth (client-token-uri client)
                       (generate-jwt client))
    ((jzon expires_in access_token)
     (setf (client-access-token-expires-at client) (+ (get-universal-time)
                                                      expires_in)
           (client-access-token client) access_token))))

(defmethod client-access-token-expired-p ((client client))
  (declare (values boolean))

  (> (get-universal-time)
     (client-access-token-expires-at client)))

(defmethod client-authorized-p ((client client))
  (declare (values boolean))

  (not (null (client-access-token client))))

(defmethod request ((client client) url &key (method :GET) jzon-payload)
  (declare (string url))

  (assert (client-access-token client)
          nil "Client is not authorized, use (gapi:auth client)")
  (handler-case
      (com.inuoe.jzon:parse
       (dex:request url
                    :method method
                    :headers `(("Content-Type" . "application/json")
                               ("Authorization" . ,(format nil "Bearer ~A"
                                                           (client-access-token client))))
                    :content (etypecase jzon-payload
                               ((or string vector null) jzon-payload)
                               ((or hash-table list) (com.inuoe.jzon:stringify jzon-payload)))))
    (dex:http-request-failed (err)
      (if (not (equal (gethash "content-type" (dex:response-headers err))
                      "application/json; charset=UTF-8"))
          (error err)
          (let* ((data (com.inuoe.jzon:parse (dex:response-body err)))
                 (error-data (gethash "error" data)))
            (if (not error-data)
                (error err)
                (trivia:match error-data
                  ((jzon code status message details)
                   (error (make-condition 'gapi-error
                                          :code code
                                          :status status
                                          :message message
                                          :details details))))))))))
