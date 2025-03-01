# GAPI-JZON

This is an **API-breaking** fork of Dmitry Kosenkov's Google API
client for Common Lisp. You can find the original at
https://github.com/Junker/gapi .

This fork exists to:

1. use COM.INUOE.JZON instead of Jonathan,
2. to provide DEFUN-GAPI,
3. for me to experiment.
  
## Warning

This software is (the fork of a) still ALPHA quality codebase. The
APIs will be likely to change.

## Usage

GAPI-JZON allows you to:

```common-lisp
CL-USER> (use-package :gapi-jzon)
;;=> T

CL-USER> (defun-gapi get-calendar-events (&client calendar-id start end)
           (declare (string calendar-id start end)) ; regular declaration, optional.
           "Return JZON of calendar events"         ; doc-string, optional

           :GET                                     ; http method, required
           "https://www.googleapis.com/calendar/v3/calendars/~A/events?timeMin=~A&timeMax=~A&orderBy=startTime&singleEvents=true")
;;=> GET-CALENDAR-EVENTS

CL-USER> (defparameter *client* (gapi-jzon:make-client-with-service-account
                                 #P"/home/myuser/service-account-key-file"
                                 :scopes '("https://www.googleapis.com/auth/calendar")))
;;=> *CLIENT*

CL-USER> (defparameter *calendar-id* "c_eda0999*********************************81ff@group.calendar.google.com")
;;=> *CALENDAR-ID*

CL-USER> (get-calendar-events *client* *calendar-id* 
                              "2025-01-01T00:00:00-04:00" "2025-02-01T00:00:00-04:00")
;;=> #<HASH-TABLE :TEST EQUAL :COUNT 9 {1003EF87E3}>

CL-USER> (match *
           ((jzon items
                  updated)
            (format t "~D items retrieved, last updated ~S~%" (length items) updated)))
;;-> 230 items retrieved, last updated "2025-02-27T07:24:56.346Z"
;;=> NIL

```

DEFUN-GAPI introduces two lambda list keywords: &CLIENT and &PAYLOAD.

`&CLIENT` must be included in the lambda list, and marks where in the
lambda list the `GAPI-JZON:CLIENT` must be passed as an argument.

`&PAYLOAD` is optional, and marks where in the lambda list the payload
must be passed as an argument.

Dmitry Kosenkov's original API is also available, except it everything
JSON is parsed and serialized by COM.INUOE.JZON and not by Jonathan.

## Documentation

- [Google APIs Explorer](https://developers.google.com/apis-explorer)
