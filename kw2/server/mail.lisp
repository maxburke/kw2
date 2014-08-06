(in-package :kw2)

(defparameter +header-separator+ (format nil "~d~d~d~d" #\Return #\Newline #\Return #\Newline))
(defparameter +header-separator-length+ (length +header-separator+))

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
   ; TODO: finish this
   ; Things to do: After validating the user can post to this group 
   ; (either because the user is a member or the group allows public
   ; posts) add the post to the database
   )))

;        (header-length (search +header-separator+ content))
;        (header (subseq content 0 header-length))
;        (body (subseq content (+ header-length +header-separator-length+))))
;  (setf *last-posted-message* content)
;  (setf *last-posted-to* to)
;  (setf *last-posted-from* from)
; )
;)



(defun mail-handler ()
 (let ((req (request-method* *request*)))
  (if (eq req :post)
   (mail-handle-post)
   (progn
    (setf (return-code*) +http-method-not-allowed+)
    "Method not allowed!"))))
