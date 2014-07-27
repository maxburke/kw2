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
 ("SELECT g.group_id, g.name, p.subject, p.post_date, u.display_name
   FROM groups AS g
       INNER JOIN acl ON g.group_id = acl.group_id
       LEFT JOIN (
           SELECT group_id, MAX(post_id) AS post_id 
           FROM posts 
           GROUP BY posts.group_id
       ) AS recent ON recent.group_id = g.group_id
       LEFT JOIN posts AS p ON (recent.post_id = p.post_id)
       LEFT JOIN users AS u ON p.user_id = u.user_id
   WHERE acl.user_id = $1" user-id))
  
(defun groups-fetch (user-id)
 (let* ((groups-with-summary (groups-fetch-with-summary user-id)))
  (st-json:write-json-to-string groups-with-summary))
)

(defun groups-handler (uri)
 (if *session*
  (groups-fetch (session-value 'id *session*))
  (redirect "/")))

