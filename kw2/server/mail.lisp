(in-package :kw2)

(defparameter +header-separator+ (format nil "~d~d~d~d" #\Return #\Newline #\Return #\Newline))
(defparameter +header-newline+ (format nil "~d~d" #\Return #\Newline))
(defparameter +header-separator-length+ (length +header-separator+))
(defparameter +header-message-id+ "Message-ID: ")
(defparameter +header-references+ "References: ")
(defparameter +header-subject+ "Subject: ")

;;;; REMOVE THESE
(defparameter *last-posted-message* '())
(defparameter *last-posted-from* '())
(defparameter *last-posted-to* '())
;;;; END

(defprepared-with-names mail-get-group-id-query (alias)
 ("SELECT pk_id FROM groups WHERE alias = $1" alias))

(defprepared-with-names mail-get-from-user-id-query (from)
 ("SELECT pk_id FROM users WHERE email = $1" from))

(defprepared-with-names mail-get-group-flags-query (group-id)
 ("SELECT flags FROM groups WHERE pk_id = $1" group-id))

(defprepared-with-names mail-get-group-membership-query (group-id user-id)
 ("SELECT fk_user_id, fk_group_id, flags FROM acl WHERE fk_user_id = $1 AND fk_group_id = $2" group-id user-id))

(defprepared-with-names post-insert-new-post (parent group-id user-id message-id subject headers post-date body)
 ("INSERT INTO posts (fk_parent_post_id, fk_group_id, fk_user_id, message_id, subject, headers, post_date, body)
  VALUES ($1, $2, $3, $4, $5, $6, $7, $8)" parent group-id user-id message-id subject headers post-date body))

(defprepared-with-names mail-get-post-id-query (post-id)
 ("SELECT pk_id FROM posts WHERE message_id = $1" post-id))

(defun mail-get-group-id (group)
 (let* ((group-name-end (position #\@ group))
        (group-name (subseq group 0 group-name-end))
        (group-id (caar (mail-get-group-id-query group-name))))
  group-id))

(defun mail-get-from-user-id (from)
 (let ((user-id (caar (mail-get-from-user-id-query from))))
  user-id))

(defun mail-get-group-flags (group-id)
 (let ((flags (caar (mail-get-group-flags-query group-id))))
  flags))

(defun mail-member-of-group-p (group-id user-id)
 (let* ((query-results (mail-get-group-membership-query group-id user-id)))))

(defun mail-get-post-id (post-id)
 (if post-id
  (let ((query-results (mail-get-post-id-query post-id)))
   (caar query-results))
  nil))

(defun whitespace-p (c)
  (case c ((#\Space #\Tab) t)
    (otherwise nil)))

(defun make-keyword (name)
  (intern (string-upcase name) "KEYWORD"))

(defun references-parser (contents)
  (let* ((reference-list '())
         (start-idx 0)
         (end-idx (1- (length contents))))
    (block outer
           (loop while (<= start-idx end-idx) do
                 (let* ((next-reference-start (search "<" contents :start2 start-idx))
                        (next-reference-end (if next-reference-start 
                                              (search ">" contents :start2 next-reference-start) 
                                              nil)))
                   (when (and next-reference-start next-reference-end)
                     (let ((reference (subseq contents next-reference-start (1+ next-reference-end))))
                       (setf reference-list (append reference-list (list reference)))))

                   (if next-reference-end
                     (setf start-idx (1+ next-reference-end))
                     (return-from outer)))))
    reference-list))

(defparameter header-field-parsers `((:references . ,(function references-parser))))

(defun parse-header-field (field contents)
  (let ((parser (cdr (assoc field header-field-parsers))))
    (if parser
      (funcall parser contents)
      contents)))

(defun parse-header (header)
  (let ((start-idx 0)
        (end-idx (1- (length header)))
        (header-fields '())
        (header-copy (subseq header 0)))
    (loop while (<= start-idx end-idx) do
          (let* ((next-newline-idx (search +header-newline+ header-copy :start2 start-idx))
                 (char-after (if (and next-newline-idx (< next-newline-idx (1- end-idx)))
                               (char header-copy (+ next-newline-idx 2)) 
                               nil)))
            (if (and next-newline-idx char-after)
              (progn
                (if (whitespace-p char-after)
                  (progn
                    (setf (aref header-copy next-newline-idx) #\space)
                    (setf (aref header-copy (1+ next-newline-idx)) #\space))
                  (let* ((header-name-end (search ":" header-copy :start2 start-idx))
                         (header-name (subseq header start-idx header-name-end))
                         (header-keyword (make-keyword header-name))
                         (header-raw-contents (subseq header (+ header-name-end 2) next-newline-idx))
                         (header-contents (parse-header-field header-keyword header-raw-contents)))
                    (setf start-idx (+ next-newline-idx 2))
                    (setf header-fields (acons header-keyword header-contents header-fields)))))
              (incf start-idx))))
    header-fields))

(defun mail-handle-post ()
 (let* ((content (post-parameter "data"))
        (to (post-parameter "to"))
        (from (post-parameter "from"))
        (group-id (mail-get-group-id to))
        (from-user-id (mail-get-from-user-id from)))
  (unless group-id
   (return-from mail-handle-post nil))

  (let* ((group-flags (mail-get-group-flags group-id))
         (member-of-group (mail-member-of-group-p group-id from-user-id)))
   (when (or member-of-group (group-allows-public-posting group-flags))
    (let* ((header-length (search +header-separator+ content))
           (header (subseq content 0 header-length))
           (body (subseq content (+ header-length +header-separator-length+)))
           (parsed-header (parse-header header))
           (message-id (cdr (assoc :message-id parsed-header)))
           (references (cdr (assoc :references parsed-header)))
           (subject (cdr (assoc :subject parsed-header)))
           (parent (car (last references)))
           (parent-id (mail-get-post-id parent))
           (date (simple-date:universal-time-to-timestamp (get-universal-time))))
     (post-insert-new-post parent-id group-id from-user-id message-id subject header date body))))))

(defun mail-handler ()
 (let ((req (request-method* *request*)))
  (if (eq req :post)
   (mail-handle-post)
   (progn
    (setf (return-code*) +http-method-not-allowed+)
    "Method not allowed!"))))


