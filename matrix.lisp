(in-package :automatricl)

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
