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

(defprepared-with-names groups-fetch-with-summary (user-id)
 ("SELECT g.pk_id AS group_id, g.name, g.alias, p.pk_id AS post_id, p.subject, p.post_date, u.display_name
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

(defprepared-with-names alias-exists-p (alias)
 ("SELECT pk_id FROM groups WHERE alias = $1" alias))

(defun groups-fetch (user-id)
 (with-connection *db-connection-parameters*
  (let* ((groups-with-summary (groups-fetch-with-summary user-id)))
   (st-json:write-json-to-string groups-with-summary))))

(defun groups-handler ()
 (if *session*
  (groups-fetch (session-value 'id *session*))
  (redirect "/")))

(defun new-group-validation-handler (uri)
 (unless *session*
  (redirect "/"))

 (with-connection *db-connection-parameters*
  (let* ((alias (cadddr uri)))
   (if (and alias (not (alias-exists-p alias)))
    *successful-post-response*
    *unsuccessful-post-response*))))

(defun new-group-handler ()
 (if *session*
  (handle-static-file #p"static/newgroup.html")
  (redirect "/")))


