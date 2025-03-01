(in-package #:gapi-jzon)

(defvar *defun-gapi-defun* 'cl:defun
  "Experimental extension point for DEFUN interchangeable forms.")

(defmacro defun-gapi (name (&rest args) &body body)
  (declare (symbol name))

  (unless (find '&client args)
    (error "&client must be specified in lambda list."))

  (multiple-value-bind (remaining-forms declarations doc-string)
      (alexandria:parse-body body :documentation t)

    (or (trivia:match remaining-forms
          ((list (guard method
                        (member method '(:GET :HEAD :OPTIONS :PATCH :POST :PUT :DELETE)))
                 (guard url-pattern
                        (typep url-pattern 'string)))

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

                (request ,client
                         (format nil ,url-pattern ,@url-params)
                         :method ,method
                         ,@(when payload
                             `(:jzon-payload ,payload)))))))
        (error "DEFUN-GAPI body requires 1) a method keyword and 2) an URL pattern."))))
