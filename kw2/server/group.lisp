(in-package :kw2)

(defun api-group-new-handler ()
 (if *session*
  (handle-static-file #p"static/newgroup.html")
  (redirect "/")))

(defprepared-with-names group-fetch-summaries-query (user-id group-name start count)
 ("SELECT posts.pk_id, posts.fk_parent_post_id, posts.message_id, posts.subject, posts.post_date
   FROM posts
       INNER JOIN groups ON posts.fk_group_id = groups.pk_id
       INNER JOIN acl ON (acl.fk_user_id = $1 AND acl.fk_group_id = groups.pk_id)
       WHERE groups.alias = $2
       LIMIT $3
       OFFSET $4" user-id group-name count start))

(defun group-fetch-summaries (user-id group-name start count)
 (with-connection *db-connection-parameters*
  (let ((results (group-fetch-summaries-query user-id group-name start count)))
   (mapcar (lambda (row)
            (setf (nth 4 row) (simple-date:timestamp-to-universal-time (nth 4 row))))
    results)
   results)
 )
)

(defun api-group-summary-handler (params)
 (unless *session*
  (redirect "/"))

 (let* ((user-id (session-value 'id *session*))
        (group (fourth params))
        (start (if (null (fifth params)) 0 (fifth params)))
        (count (if (null (sixth params)) 50 (sixth params)))
        (summaries (group-fetch-summaries user-id group start count)))
  (st-json:write-json-to-string summaries))
)

(defun api-group-validate-handler (uri)
 (unless *session*
  (redirect "/"))

 (with-connection *db-connection-parameters*
  (let* ((alias (cadddr uri)))
   (if (and alias (not (alias-exists-p alias)))
    *successful-post-response*
    *unsuccessful-post-response*))))

(defprepared-with-names access-permitted-p (user-id group-name)
 ("SELECT acl.*
   FROM acl
       INNER JOIN groups ON acl.fk_group_id = groups.pk_id
       WHERE acl.fk_user_id = $1 AND groups.alias = $2" user-id group-name))

(defun access-denied-p (user-id group-name)
 (with-connection *db-connection-parameters*
  (not (access-permitted-p user-id group-name)))
)

(defun group-handler (params)
 (unless *session*
  (redirect "/"))

 (let* ((user-id (session-value 'id *session*))
        (group-name (second params)))
  (if (access-denied-p user-id group-name)
   (progn
    (setf (return-code*) +http-forbidden+)
    (return-from group-handler)))

  (handle-static-file #p"static/group.html"))
)

