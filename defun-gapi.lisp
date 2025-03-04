(in-package #:gapi-jzon)

(defvar *defun-gapi-defun* 'cl:defun
  "Experimental extension point for DEFUN interchangeable forms.")

(defmacro defun-gapi (name
                      (method url-pattern (&rest args))
                      &body body)
  "Define a function around a Google API call.

Returns the evaluation of the body; if no body is provided, returns
the Google API response.

NAME is the function name.

METHOD is the DEXADOR:REQUEST method keyword (e.g. :GET, :POST).

URL-PATTERN is a FORMAT control string.

ARGS specify the resulting function's lambda list.

&CLIENT lambda keyword marks the position of the GAPI-JZON:CLIENT
instance and must be specified anywhere within ARGS.

&PAYLOAD lambda keyword marks the position of the JZON payload and may
or may not be specified within ARGS.

All non-lambda-keyword args are in-order format arguments to
URL-PATTERN.

The API response is lexically bound to GAPI-JZON:RESPONSE so the body
can access it."
  (declare (symbol name)
           (keyword method)
           (string url-pattern))

  (unless (find '&client args)
    (error "&client must be specified in lambda list."))

  (multiple-value-bind (remaining-forms declarations doc-string)
      (alexandria:parse-body body :documentation t)

           (let* (client
                  payload
                  url-params
                  (resolved-params
                    (mapcar
                     (lambda (param)
                       (case param
                         (&client (setf client (gensym)))
                         (&payload (setf payload (gensym)))
                         (t (push param url-params) param)))
                     args))
                  (url-params (reverse url-params)))

             `(,*defun-gapi-defun* ,name ,resolved-params
                ,doc-string
                ,@declarations

                (when (or (not (client-authorized-p ,client))
                          (client-access-token-expired-p ,client))
                  (auth ,client))

                            (let ((response
                (request ,client
                         (format nil ,url-pattern ,@url-params)
                         :method ,method
                         ,@(when payload
                                                 `(:jzon-payload ,payload)))))
                              ,@(or remaining-forms '(response)))))))
