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
   (or (query (:select
	       :value :from :tr
	       :where (:and (:= :lang (string-downcase (smake lang)))
			    (:= :keyword (string-downcase (smake keyword)))))
	      :single))))

(defun obfuscate-password (passwd)
  (hash-password (+s "sloboda" passwd)))

(defclass user ()
  ((ix-user :col-type serial :initarg :ix-user :reader ix-user)
   (username :col-type string :initarg :username :reader username :initform "")
   (email :col-type string :initarg :email :reader email :initform "")
   (passwd :col-type string :initarg :passwd :initform "")
   (acc-type :col-type string :initarg :acc-type :reader acc-type :initform "")
   (role :col-type integer :initarg :role :reader role :initform 0)
   (status :col-type integer :initarg :status :reader status :initform 0))
  (:metaclass dao-class)
  (:keys ix-user))

(defun user-if-valid (username pass)
  (with-re-db
    (let ((matched-users
	   (select-dao 'user
		       (:and (:= :username username)
			     (:= :passwd (obfuscate-password pass))))))
      (if (< 0 (length matched-users))
	  (car matched-users)))))

;;;;;;;; project-specific code

(defclass estate ()
  ((ix-estate :col-type serial :initarg :ix-estate :accessor ix-estate)
   (ix-user :col-type integer :initarg :ix-user :accessor ix-user :initform 0)
   (address :col-type string :initarg :address :accessor address :initform "")
   (telnum :col-type string :initarg :telnum :accessor telnum :initform "")
   (ix-main-pic :col-type integer :initarg :ix-main-pic :accessor ix-main-pic
		:initform "")
   (visible :col-type integer :initarg :visible :accessor visible :initform 0))
  (:metaclass dao-class)
  (:keys ix-estate))

(defclass pic ()
  ((ix-pic :col-type serial :initarg :ix-pic :accessor ix-pic)
   (ix-estate :col-type integer :initarg :ix-estate :accessor ix-estate
	      :initform 0)
   (path :col-type string :initarg :path :accessor path :initform ""))
  (:metaclass dao-class)
  (:keys ix-pic))