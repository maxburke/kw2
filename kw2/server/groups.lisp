(in-package :kw2)

;;; All groups with most recent post information:
;;; SELECT g.group_id, g.name, p.subject, p.post_date, u.display_name
;;; FROM groups AS g
;;;     INNER JOIN acl ON g.group_id = acl.group_id
;;;     LEFT JOIN (
;;;         SELECT group_id, MAX(post_id) AS post_id
;;;         FROM posts
;;;         GROUP BY posts.group_id
;;;     ) AS recent ON recent.group_id = g.group_id
;;;     LEFT JOIN posts AS p ON (recent.post_id = p.post_id)
;;;     LEFT JOIN users AS u ON p.user_id = u.user_id
;;; WHERE acl.user_id = '1';

(defparameter +group-flag-allow-public-posting+ 1)

(defun group-allows-public-posting (flags)
 (eq (logand flags +group-flag-allow-public-posting+) +group-flag-allow-public-posting+))

(defprepared-with-names groups-fetch-with-summary-query (user-id)
 ("SELECT g.pk_id AS group_id, g.name, g.alias, p.pk_id AS post_id, p.subject, p.post_date, CASE WHEN u.display_name IS NULL THEN u.email ELSE u.display_name END AS from
   FROM groups AS g
       INNER JOIN acl ON g.pk_id = acl.fk_group_id
       LEFT JOIN (
           SELECT fk_group_id, MAX(pk_id) AS post_id
           FROM posts
           GROUP BY posts.fk_group_id
       ) AS recent ON recent.fk_group_id = g.pk_id
       LEFT JOIN posts AS p ON (recent.post_id = p.pk_id)
       LEFT JOIN users AS u ON p.fk_user_id = u.pk_id
   WHERE acl.fk_user_id = $1" user-id))

(defun groups-fetch-with-summary (user-id)
 (with-connection *db-connection-parameters*
  (let ((results (groups-fetch-with-summary-query user-id)))
   (mapcar (lambda (row)
            (setf (nth 5 row) (simple-date:timestamp-to-universal-time (nth 5 row))))
    results)
   results)))

(defprepared-with-names alias-exists-p (alias)
 ("SELECT pk_id FROM groups WHERE alias = $1" alias))

(defun groups-fetch (user-id)
  (let* ((groups-with-summary (groups-fetch-with-summary user-id)))
   (st-json:write-json-to-string groups-with-summary)))

(defun api-groups-handler ()
 (if *session*
  (groups-fetch (session-value 'id *session*))
  (redirect "/")))


