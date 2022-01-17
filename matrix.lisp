(defpackage :fwoar.lisp-sandbox.matrix
  (:use :cl )
  (:export ))
(in-package :fwoar.lisp-sandbox.matrix)

(defvar *matrix-host*)

(defun matrix-sync (homeserver-prefix access-token &optional (since nil since-p))
  (yason:parse
   (let ((drakma:*text-content-types* (acons "application" "json"
                                             drakma:*text-content-types*)))
     (drakma:http-request
      (if since-p
          (format nil "~a/sync?since=~a" homeserver-prefix since)
          (format nil "~a/sync" homeserver-prefix))
      :additional-headers
      `(("Authorization" . ,(format nil "Bearer ~a" access-token)))))))

(fw.lu:defclass+ plaintext-message ()
  ((body :initarg :body :reader body)))
(defmethod yason:encode-object ((object plaintext-message))
  (yason:with-object ()
    (yason:encode-object-element "msgtype" "m.text")
    (yason:encode-slots object))
  object)
(defmethod yason:encode-slots progn ((object plaintext-message))
  (yason:encode-object-element "body" (body object)))

(fw.lu:defclass+ html-message ((plaintext-message (body)))
  ())
(defmethod yason:encode-object ((object html-message))
  (yason:with-object ()
    (yason:encode-object-element "msgtype" "m.text")
    (yason:encode-object-element "format" "org.matrix.custom.html")
    (yason:encode-slots object))
  object)
(defmethod yason:encode-slots progn ((object html-message))
  (let ((body (body object)))
    (yason:encode-object-element "body" (plump:render-text (plump:parse body)))
    (yason:encode-object-element "formatted_body" (body object))))


(defun construct-message (body)
  (yason:encode-alist
   `(("msgtype" . "m.text")
     ("body" . ,body))))


(defun get-room-send-url (room-id event-type txid)
  (let ((uri (make-instance 'puri:uri
                            :scheme :https
                            :host *matrix-host*)))
    (prog1 uri
      (setf (puri:uri-parsed-path uri)
            (list :absolute
                  "_matrix" "client" "r0" "rooms"
                  room-id
                  "send"
                  event-type
                  (princ-to-string txid))))))

(defun get-room-state-url (room-id event-type txid)
  (let ((uri (make-instance 'puri:uri
                            :scheme :https
                            :host *matrix-host*)))
    (prog1 uri
      (setf (puri:uri-parsed-path uri)
            (list :absolute
                  "_matrix" "client" "r0" "rooms"
                  room-id
                  "state"
                  event-type
                  (princ-to-string txid))))))

(let ((v (load-time-value 1)))
  (defun monotonic-txid ()
    (incf v)))

(defun send-room-event (access-token room-id event-type txid event &optional (method :put))
  (drakma:http-request (get-room-state-url room-id event-type txid)
                       :additional-headers `(("Authorization" . ,(format nil "Bearer ~a" access-token)))
                       :method method
                       :content-type "application/json"
                       :content event))

;; https://<matrix.host>/_matrix/client/r0/rooms/<room.id>/send/m.room.message/:it
(defun send-message (access-token room-id event-type txid message)
  (drakma:http-request (get-room-send-url room-id event-type txid)
                       :additional-headers `(("Authorization" . ,(format nil "Bearer ~a" access-token)))
                       :method :put
                       :content-type "application/json"
                       :content (with-output-to-string (s)
                                  (yason:with-output (s)
                                    (yason:encode-object message)))))

(defun evaluable-code-block (str)
  (when str
    (destructuring-bind (language code)
        (fwoar.string-utils:partition #\newline str)
      (and (or (equal (serapeum:trim-whitespace language) "lisp")
               (equal (serapeum:trim-whitespace language) "javascript")
               (equal (serapeum:trim-whitespace language) "js"))
           (or (alexandria:starts-with-subseq ";;>" code)
               (alexandria:starts-with-subseq "//>" code)
               (alexandria:starts-with-subseq "#>" code))
           code))))

(defun find-code-block (str)
  (alexandria:when-let* ((start (fw.lu:may (+ (search "```" str) 3)))
                         (end (search "```" str :start2 start)))
    (subseq str start end)))

#+(or) #+(or)
(send-message +access-token+
              "<room_id>"
              "m.room.message"
              (monotonic-txid)
              (plaintext-message (prin1-to-string *)))

(fw.lu:dive '("rooms" "join" "<room_id>" "timeline" "events") *)

(defvar +timeline-lens+
  (data-lens.lenses:make-hash-table-lens "timeline"))
(defvar +timeline-events-lens+
  (data-lens:• +timeline-lens+
               (data-lens.lenses:make-hash-table-lens "events")))

(defvar +message-content-lens+
  (data-lens:• (data-lens.lenses:make-hash-table-lens "content")
               (data-lens.lenses:make-hash-table-lens "body")))

(defun room-joins (message)
  (fw.lu:dive '("rooms" "join")
              message))

(defun room-list (room-join-ht)
  (fw.lu:may (alexandria:hash-table-keys room-join-ht)))

(defun call-with-rooms (message cb)
  (let* ((room-joins (room-joins message))
         (room-list (room-list room-joins)))
    (loop for room-id in room-list
          collect (funcall cb
                           (gethash room-id room-joins)
                           room-id))))

(defmacro with-rooms ((room-sym room-id-sym) message &body body)
  `(call-with-rooms ,message
                    (lambda (,room-sym ,room-id-sym)
                      ,@body)))

(defun eval-js (message)
  (cl-js:run-js (evaluable-code-block (find-code-block *))))

#+(or)
(loop (with-rooms (room room-id) (funcall *syncer*)
        (funcall (data-lens:• (data-lens:over
                               (lambda (it)
                                 (let ((maybe-js-result (fw.lu:may
                                                            (cl-js:run-js
                                                             (evaluable-code-block
                                                              (find-code-block
                                                               (data-lens.lenses:view +message-content-lens+ it)))))))
                                   (when maybe-js-result
                                     (send-message +access-token+
                                                   room-id
                                                   "m.room.message"
                                                   (monotonic-txid)
                                                   (plaintext-message (prin1-to-string maybe-js-result)))))))
                              (data-lens:include (data-lens:on (data-lens:• 'evaluable-code-block
                                                                            'find-code-block)
                                                               (lambda (it)
                                                                 (data-lens.lenses:view +message-content-lens+
                                                                                        it))))
                              (data-lens:include (data-lens:on (data-lens:== "m.room.message" :test 'equal)
                                                               (data-lens:key "type"))))
                 (data-lens.lenses:view +timeline-events-lens+ room)))
      (sleep 1))

(defclass room-sync (fw.lu:hashtable-slot-mixin)
  ((state :reader state)
   (summary :reader summary)
   (timeline :reader timeline)
   (ephemeral :reader ephemeral)
   (account-data :reader account-data)
   (unread-notifications :reader unread-notifications)))

(defclass event (fw.lu:hashtable-slot-mixin)
  ((type :reader event-type)))

(defclass timeline-event (event)
  ((sender :reader event-sender)
   (content :reader event-content)
   (event-id :reader event-event-id)
   (unsigned :reader event-unsigned)
   (origin-server-ts :reader event-origin-server-ts)))

(defvar +events-lens+ (data-lens.lenses:make-hash-table-lens "events"))
(defmethod timeline :around ((object room-sync))
  (data-lens.lenses:over +events-lens+
                         (data-lens:over
                          (lambda (it)
                            (make-instance 'timeline-event :doc it)))
                         (call-next-method)))

#+(or)
#(hash-table "state": #(hash-table "events": (#(hash-table "type": "co.fwoar.testing",
                                                "sender": "@xxx:xx.xx",
                                                "content": #(hash-table "bar": 23,
                                                             "baz": 404,
                                                             "foo": 2),
                                                "event_id": "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
                                                "unsigned": #(hash-table "age": 5984230),
                                                "state_key": "the_state_key4",
                                                "origin_server_ts": 1642210653378))),
  "summary": #(hash-table),
  "timeline": #(hash-table "events": (#(hash-table "type": "m.room.message",
                                        "sender": "@xxx:xx.xx",
                                        "content": #(hash-table "body": "xxxxxxx xxxx xxx xxxxxxx",
                                                     "msgtype": "m.text"),
                                        "event_id": "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
                                        "unsigned": #(hash-table "age": 5120086),
                                        "origin_server_ts": 1642211517522)),
                "limited": T,
                "prev_batch": "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
  "ephemeral": #(hash-table "events": (#(hash-table "type": "m.typing",
                                         "content": #(hash-table "user_ids": NIL))
                                       #(hash-table "type": "m.receipt",
                                         "content": #(hash-table "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx": #(hash-table "m.read": #(hash-table "@xxx:xx.xx": #(hash-table "ts": 1642211577072,
                                                                                                                                                                     "hidden": NIL))),
                                                      "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx": #(hash-table "m.read": #(hash-table "@xxx:xx.xx": #(hash-table "ts": 1642211586525,
                                                                                                                                                          "hidden": NIL),
                                                                                                                               "@xxx:xx.xx": #(hash-table "ts": 1642214183951,
                                                                                                                                               "hidden": NIL),
                                                                                                                               "@xxx:xx.xx": #(hash-table "ts": 1642211584953,
                                                                                                                                               "hidden": NIL))))))),
  "account_data": #(hash-table "events": NIL),
  "unread_notifications": #(hash-table "highlight_count": 0,
                            "notification_count": 118),
  "org.matrix.msc2654.unread_count": 113)
