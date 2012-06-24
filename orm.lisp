(in-package :re)

(ql:quickload :postmodern)

(defparameter *db-parameters* 
  (list "re_db" "re_user" "re_pass" "localhost" :pooled-p t))

(defmacro with-re-db (&body body)
  `(with-connection (list ,@*db-parameters*) ,@body))

(defun menu-items (&key lang)
  "TODO: returns menu items for specified language. "
  '(("Home" "./")
    ("Real Estate" "./real-estate")
    ("Gallery" "./gallery")
    ("About the project" "./about")))

(defun tr (keyword lang)
  (with-re-db
   (or (query (:select :value :from :tr :where (:and (:= :lang (string-downcase (smake lang))) (:= :keyword (string-downcase (smake keyword))))) :single))))