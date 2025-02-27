(in-package #:gapi-jzon)

(trivia:defpattern jzon (&rest args)
  "Convenient Trivia pattern for COM.INUOE.JZON parsed structures,
which represents objects as hashtables.

EXAMPLES:

The following examples help illustrate:

(DEFPARAMETER
  JZON-HASHTABLE
  (COM.INUOE.JZON:PARSE \"{\\\"name\\\": \\\"Andrea\\\", \\\"age\\\": \\\"10\\\"}\"))

(MATCH JZON-HASHTABLE
  ((JZON NAME                   ; matches (lowercase) \"name\" property, binds to NAME
         AGE)                   ; matches (lowercase) \"age\" property, binds to AGE
   (FORMAT T \"~A is ~D years old.~%\" NAME AGE)))

You can have property names distinct of the symbols bound. You can use
strings or keywords. If you need to refer to UPPER OR MIXED case JSON property
names, use strings.

(MATCH JZON-HASHTABLE
  ((JZON (\"name\" . SOME-NAME) ; matches \"name\", binds to SOME-NAME
         (:AgE . SOME-AGE))     ; matches (lowercase) \"age\", binds to SOME-AGE
   (FORMAT T \"~A is ~D years old.~%\" SOME-NAME SOME-AGE)))

NESTING:

Nesting is trivial:

(MATCH ACCOUNT
  ((JZON SERVICE                ; matches (lowercase) \"service\", binds to SERVICE
         (\"ClientInfo\"        ; matches \"ClientInfo\"
          . (JZON NAME          ; object subpattern; also matches \"name\" and binds NAME
                  (\"address\"  ; matches \"address\"
                    . (JZON STREET ; object subpattern; matches \"street\", binds STREET
                            STNUMBER))))) ; matches \"stnumber\", binds to STNUMBER

   (FORMAT T \"~A is enrolled in service ~A and lives at ~A # ~A~%\"
     NAME SERVICE STREET STNUMBER)))
"

  `(and ,@(mapcar (lambda (arg)
                    (flet ((syntax-error (arg)
                               (error
                                "Non-KEYWORD SYMBOL or CONS expected, got ~S." arg)))
                      (typecase arg
                        (keyword (syntax-error arg))
                        (symbol (setf arg (cons arg arg)))
                        (cons)
                        (t (syntax-error arg))))

                    (funcall (trivia.level0:lambda-match0
                               ((cons key pattern)
                                (etypecase key
                                  (symbol (setf key (string-downcase (symbol-name key))))
                                  (string))
                                (with-gensyms (ht)
                                  `(trivia:<> ,pattern
                                              (gethash ,key ,ht)
                                              ,ht))))
                             arg))
                  args)))
