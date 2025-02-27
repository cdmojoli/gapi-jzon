# GAPI-JZON

**This is (probably) NOT THE PROJECT YOU WANT.**

Please go to https://github.com/Junker/gapi . This is a modified fork
of Dmitry Kosenkov's Google API client for Common Lisp.

This fork exists to:

1. use COM.INUOE.JZON instead of Jonathan, which complicates compatibility, and to
2. freely explore changes I might need.
  
## Warning

This software is still ALPHA quality. The APIs will be likely to change.

## Usage

```common-lisp
(defparameter *client*
  (gapi-jzon:make-client-with-service-account
   *service-file* :scopes '("https://www.googleapis.com/auth/cloud-translation")))
(defparameter *project-id* (gapi-jzon:client-project-id *client*))
(gapi-jzon:auth *client*)
(gapi-jzon:request *client* (format nil
                                    "https://translate.googleapis.com/v3beta1/projects/~A:detectLanguage"
                                    *project-id*)
                   :method :POST :payload '(:|content| "Hello"))
;; => (:|languages| ((:|confidence| 1 :|languageCode| "en")))


;; FCM
(defparameter *client*
  (gapi-jzon:make-client-with-service-account
   *service-file* :scopes '("https://www.googleapis.com/auth/firebase.messaging")))
(defparameter *project-id* (gapi-jzon:client-project-id *client*))
(gapi-jzon:auth *client*)
(defvar *message*
  (list :|token| *token*
        :|notification| (list :|title| "Message Title"
                              :|body| "Message body")))
(gapi-jzon:request *client* (format nil "https://fcm.googleapis.com/v1/projects/~A/messages:send"
                                    *project-id*)
                   :method :POST :data (list :|message| *message*))

;;etc
```

## Documentation

- [Google APIs Explorer](https://developers.google.com/apis-explorer)
