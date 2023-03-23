(in-package :automatricl)

(defvar *matrix-host*)
(defvar *matrix-password*)

#+(or)
(defun fwoar/tmp/access-token ()
  (let ((url-request-method "POST")
        (url-request-data (json-encode-alist
                           `((type . "m.login.password")
                             (identifier . ((type . "m.id.user")
                                            (user . "el-bot")))
                             (password . ,(fwoar/tmp/get-password)))))
        (url-request-extra-headers '(("Content-Type" . "application/json"))))
    (with-current-buffer (url-retrieve-synchronously "https://m.edw.ai/_matrix/client/r0/login")
      (goto-char (point-min))
      (forward-paragraph)
      (gethash "access_token" (json-parse-string (buffer-substring (point) (point-max)))))))

(defun uid (username)
  (alexandria:alist-hash-table
   `(("type" . "m.id.user")
     ("user" . ,username))))

(defun login-body (username password)
  (alexandria:alist-hash-table
   `(("type" . "m.login.password")
     ("identifier" . ,(uid username))
     ("password" . ,password))))

(defmacro with-json-output ((s &key indent) &body body)
  `(let ((,s (yason:make-json-output-stream s :indent ,indent)))
     ,@body))

(defun json-string (v)
  (with-output-to-string (s)
    (with-json-output (s)
      (yason:encode v s))))

(defun merge-matrix-api (host path)
  (let ((base (make-instance 'puri:uri :scheme :https
                                       :host host
                                       :path "/_matrix/client/r0/")))
    (puri:merge-uris (if (alexandria:starts-with #\/ path)
                         (subseq path 1)
                         path)
                     base)))

(defun auth-header (creds)
  `("Authorization" . ,(format nil
                               "Bearer ~a"
                               (access-token creds))))

(defun matrix-request (uri creds
                       &key
                         (content nil content-p)
                         (method (if content-p :post :get)))
  (let ((drakma:*text-content-types* (acons "application" "json"
                                            drakma:*text-content-types*))
        (auth-header (list (auth-header creds))))
    (yason:parse
     (if content-p
         (drakma:http-request uri
                              :method method
                              :additional-headers auth-header
                              :content-type "application/json"
                              :content content)
         (drakma:http-request uri
                              :method method
                              :additional-headers auth-header)))))

(fw.lu:defclass+ creds ((fw.lu:hashtable-slot-mixin (doc)))
  ((user-id :reader user-id)
   (device-id :reader device-id)
   (home-server :reader home-server)
   (access-token :reader access-token)))

(defun login (host username password)
  (let ((drakma:*text-content-types* (acons "application" "json"
                                            drakma:*text-content-types*)))
    (creds
     (yason:parse
      (drakma:http-request (merge-matrix-api host "/login")
                           :method :post
                           :content-type "application/json"
                           :content (json-string
                                     (login-body username password)))))))

(defun matrix-sync (host creds &optional (since nil since-p))
  (matrix-request (merge-matrix-api host
                                    (if since-p
                                        (format nil "/sync?since=~a" since)
                                        (format nil "/sync")))
                  creds))

(fw.lu:defclass+ syncer ()
  ((%since :accessor since :initform nil)
   (%sync-cont :reader sync-cont :initform nil)))
(defgeneric start-sync (syncer host creds)
  (:method ((syncer syncer) host creds)
    (unless (sync-cont syncer)
      (funcall
       (setf (slot-value syncer '%sync-cont)
             (lambda ()
               (fw.lu:prog1-bind (r (if (since syncer)
                                        (matrix-sync host creds (since syncer))
                                        (matrix-sync host creds)))
                 (setf (since syncer) (gethash "next_batch" r)))))))))

(defgeneric tick (syncer)
  (:method ((syncer syncer))
    (funcall (sync-cont syncer))))

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

#+(or)
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
