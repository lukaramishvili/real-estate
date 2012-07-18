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
   (loc-lat :col-type string :initarg :loc-lat :accessor loc-lat :initform "0")
   (loc-lng :col-type string :initarg :loc-lng :accessor loc-lng :initform "0")
   (visible :col-type integer :initarg :visible :accessor visible :initform 0)

   (apt-type :col-type string :initarg :apt-type 
	     :accessor apt-type :initform "")
   (status :col-type string :initarg :status :accessor status :initform "")
   (pst-code :col-type string :initarg :pst-code :accessor pst-code 
	     :initform "")
   (munic :col-type string :initarg :munic :accessor munic :initform "")
   (ix-country :col-type integer :initarg :ix-country :accessor ix-country 
	       :iniform 0)
   (constr :col-type string :initarg :constr :accessor constr :initform "")
   (total :col-type integer :initarg :total :accessor total :initform 0)
   (land :col-type integer :initarg :land :accessor land :initform 0)
   (desc :col-type string :initarg :desc :accessor desc :initform "")
   (zmh :col-type string :initarg :zmh :accessor zmh :initform "")
   (price :col-type decimal :initarg :price :accessor price :initform 0)
   (since :col-type integer :initarg :since :accessor since :initform 0)
   (bedrooms :col-type integer :initarg :bedrooms 
	     :accessor bedrooms :initform 0)
   (bathrooms :col-type integer :initarg :bathrooms 
	      :accessor bathrooms :initform 0)
   (terrace-p :col-type integer :initarg :terrace-p 
	      :accessor terrace-p :initform 0)
   (garden-p :col-type integer :initarg :garden-p 
	     :accessor garden-p :initform 0)
   (parking-lots :col-type integer :initarg :pstking-lots 
		 :accessor parking-slots :initform 0)
   (building-permit :col-type integer :initarg :building-permit 
		    :accessor building-permit :initform 0)
   (destination :col-type string :initarg :destination 
		:accessor destination :initform "")
   (summons :col-type string :initarg :summons 
	    :accessor summons :initform "")
   (preemption :col-type string :initarg :preemption 
	       :accessor preemption :initform "")
   (subdiv-permit :col-type string :initarg :subdiv-permi 
		  :accessor subdiv-permit :initform "")
   (epc :col-type decimal :initarg :epc :accessor epc :initform 0)
   (kad-ink :col-type decimal :initarg :kad-ink :initform 0)
   )
  (:metaclass dao-class)
  (:keys ix-estate))

(defclass pic ()
  ((ix-pic :col-type serial :initarg :ix-pic :accessor ix-pic)
   (ix-estate :col-type integer :initarg :ix-estate 
	      :accessor ix-estate :initform 0)
   (order :col-type integer :initarg :order :accessor order :initform 0)
   (path :col-type string :initarg :path :accessor path :initform ""))
  (:metaclass dao-class)
  (:keys ix-pic))
