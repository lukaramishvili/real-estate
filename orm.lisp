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
	      :single)
       (string-capitalize 
	(string-downcase (substitute #\Space #\- (+s keyword)))))))

(defun obfuscate-password (passwd)
  (hash-password (+s "sloboda" passwd)))

(defun valid-acc-types ()
  (list "simple" "broker"))

(defun valid-acc-type-p (acc-type)
  (if (member acc-type (valid-acc-types) 
	      :test #'string=)
      t))

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

(defun save-user (user)
  (with-re-db 
    (insert-dao user)
    (ix-user user)))

;;;;;;;; project-specific code

(defun spec-f-p (filter)
  "specific-filter-p: returns true if the filter is specific 
   (not including everything)"
  (and filter 
       (not (string= filter "*"))))

(defun apt-type-options (&key not-sel)
  (concatenate 'list 
	       (if not-sel (list "*"))
	       (list "apartment" "house" "land" "office" 
		     "commercial" "garage" "new")))

(defun status-options (&key not-sel)
  (concatenate 'list 
	       (if not-sel (list "*"))
  (list "sale" "rent")))

(defun all-countries (&key not-sel)
  (concatenate 'list 
	       (if not-sel (list "*"))
  (list (list 1 "Belgium") (list 2 "Niederlands"))))

(defun constr-options (&key not-sel)
  (concatenate 'list 
	       (if not-sel (list "*"))
  (list "detached" "terraced" "semi-detached")))

(defun summons-options (&key not-sel)
  (concatenate 'list 
	       (if not-sel (list "*"))
  (list (list "nvt" "NVT") (list "vt" "VT"))))

(defun preemption-options (&key not-sel)
  (concatenate 'list 
	       (if not-sel (list "*"))
  (list (list "nvt" "NVT") (list "vt" "VT"))))

(defun subdiv-permit-options (&key not-sel)
  (concatenate 'list 
	       (if not-sel (list "*"))
  (list (list "nvt" "NVT") (list "vt" "VT"))))

(defclass estate ()
  ((ix-estate :col-type serial :initarg :ix-estate :accessor ix-estate)
   (ix-user :col-type integer :initarg :ix-user :accessor ix-user :initform 0)
   (address :col-type string :initarg :address :accessor address :initform "")
   (telnum :col-type string :initarg :telnum :accessor telnum :initform "")
   (ix-main-pic :col-type integer :initarg :ix-main-pic :accessor ix-main-pic
		:initform 0)
   (loc-lat :col-type string :initarg :loc-lat :accessor loc-lat :initform "0")
   (loc-lng :col-type string :initarg :loc-lng :accessor loc-lng :initform "0")
   (visible :col-type boolean :initarg :visible :accessor visible :initform 0)

   (apt-type :col-type string :initarg :apt-type 
	     :accessor apt-type :initform "")
   (status :col-type string :initarg :status :accessor status :initform "")
   (pst-code :col-type string :initarg :pst-code :accessor pst-code 
	     :initform "")
   (munic :col-type string :initarg :munic :accessor munic :initform "")
   (ix-country :col-type integer :initarg :ix-country :accessor ix-country 
	       :initform 0)
   (constr :col-type string :initarg :constr :accessor constr :initform "")
   (total :col-type integer :initarg :total :accessor total :initform 0)
   (land :col-type integer :initarg :land :accessor land :initform 0)
   (desc :col-type string :initarg :desc :accessor desc :initform "")
   (zmh :col-type string :initarg :zmh :accessor zmh :initform "")
   (price :col-type float :initarg :price :accessor price :initform 0)
   (since :col-type bigint :initarg :since :accessor since :initform 0)
   (bedrooms :col-type integer :initarg :bedrooms 
	     :accessor bedrooms :initform 0)
   (bathrooms :col-type integer :initarg :bathrooms 
	      :accessor bathrooms :initform 0)
   (terrace-p :col-type boolean :initarg :terrace-p 
	      :accessor terrace-p :initform 0)
   (garden-p :col-type boolean :initarg :garden-p 
	     :accessor garden-p :initform 0)
   (parking-lots :col-type integer :initarg :parking-lots 
		 :accessor parking-lots :initform 0)
   (building-permit-p :col-type boolean :initarg :building-permit-p 
		    :accessor building-permit-p :initform 0)
   (destination :col-type string :initarg :destination 
		:accessor destination :initform "")
   (summons :col-type string :initarg :summons 
	    :accessor summons :initform "")
   (preemption :col-type string :initarg :preemption 
	       :accessor preemption :initform "")
   (subdiv-permit :col-type string :initarg :subdiv-permit 
		  :accessor subdiv-permit :initform "")
   (epc :col-type float :initarg :epc :accessor epc :initform 0)
   (kad-ink :col-type float :initarg :kad-ink :accessor kad-ink :initform 0)
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

(defun pics-for-firstpage ()
  (with-re-db
    (remove-if-not 
     (lambda (x) x)
     (mapcar (lambda (ix-pic) 
	       (car (select-dao 'pic (:= :ix-pic (car ix-pic)))))
	     (query (:select :ix-main-pic :from :estate
			     :where (:> :ix-main-pic 0))) 
	     ))));here was :list but didn't work for some reason

(defun pic-to-hashtable (pic &key (make-path-linkable nil))
  "make hashtable from an integer or 'pic class instance "
  (let ((p (cond ((integerp pic) (with-re-db 
				   (car (select-dao 'pic (:= :ix-pic pic)))))
		 ((equalp (find-class 'pic) (class-of pic)) pic))))
    (if p
	(alexandria:plist-hash-table 
	 (list :ix-pic (ix-pic p)
	       :ix-estate (ix-estate p)
	       :order (order p)
	       :path (if make-path-linkable
			 (linkable-pic-path p)
			 (path p)))
	 :test #'equal))))

(defun estate-nonmain-pics (estate)
  (estate-pics estate :include-main-pic nil))

(defun estate-pics (estate &key (include-main-pic t))
  (let ((e (cond ((integerp estate) 
		  (with-re-db 
		    (car (select-dao 'estate (:= :ix-estate estate)))))
		 ((equalp (find-class 'estate) (class-of estate)) 
		  estate))))
    (if e
	(with-re-db 
	  (eval 
	   `(select-dao 
	     'pic 
	     (:and (:= :ix-estate ,(ix-estate e)) 
		   ,(if include-main-pic 
			`(:!= :ix-pic ,(ix-main-pic e)) 
			t))))))))

(defun get-country (ix-country)
  (loop for i in (all-countries) 
     do (if (= (car i) ix-country) 
	    (return (cadr i)))))

(defun estate-to-hash-table (e)
  (alexandria:plist-hash-table 
   (list :address (address e)
	 :telnum (telnum e)
	 :apt-type (apt-type e)
	 :status (status e)
	 :pst-code (pst-code e)
	 :munic (munic e)
	 :country (get-country (ix-country e))
	 :constr (constr e)
	 :total (total e)
	 :land (land e)
	 :desc (desc e)
	 :zmh (zmh e)
	 :price (price e)
	 :since (since e)
	 :bedrooms (bedrooms e)
	 :bathrooms (bathrooms e)
	 :terrace-p (terrace-p e)
	 :garden-p (garden-p e)
	 :parking-lots (parking-lots e)
	 :building-permit-p (building-permit-p e)
	 :destination (destination e)
	 :summons (summons e)
	 :preemption (preemption e)
	 :subdiv-permit (subdiv-permit e)
	 :epc (epc e)
	 :kad-ink (kad-ink e)
	 )))

#+nil 
(defmacro filter-estates (filters-alist)
  `(let ((f-a (remove-if-not 
	       (lambda(aitem)
		 (valid-slot-p 'estate (car aitem)))
	       ,filters-alist))
	 (apt-type (if assoc)
	 )
     (with-re-db 
       (select-dao 
	'estate
	(:and 
	 t
	 ,@(if (assoc :apt-type f-a)
	       '(:= :apt-type (car (assoc :apt-type f-a))))))
	))))

(defun filter-estates (filters-alist &key (count 10000) (offset 0))
  (let* ((fa filters-alist)
	 (apt-type (cdr (assoc :apt-type fa)))
	 (status (cdr (assoc :status fa)))
	 (ix-country (cdr (assoc :ix-country fa)))
	 (constr (cdr (assoc :constr fa)))
	 (total-min (cdr (assoc :total-min fa)))
	 (total-max (cdr (assoc :total-max fa)))
	 (price-min (cdr (assoc :price-min fa)))
	 (price-max (cdr (assoc :price-max fa)))
	 (bedrooms-min (cdr (assoc :bedrooms-min fa)))
	 (bathrooms-min (cdr (assoc :bathrooms-min fa)))
	 (terrace (cdr (assoc :terrace fa)))
	 (garden (cdr (assoc :garden fa)))
	 (building-permit (cdr (assoc :building-permit fa)))
	 (summons (cdr (assoc :summons fa)))
	 (preemption (cdr (assoc :preemption fa)))
	 (subdiv-permit (cdr (assoc :subdiv-permit fa))))
    (with-re-db
      (eval 
       `(query-dao 
	 'estate
	 (:limit 
	  (:select 
	   :* :from :estate :where
	   (:and 
	    t
	    ,(if (spec-f-p apt-type) `(:= :apt-type ,apt-type) t)
	    ,(if (spec-f-p status) `(:= :status ,status) t)
	    ,(if (spec-f-p ix-country) `(:= :ix-country ,ix-country) t)
	    ,(if (spec-f-p constr) `(:= :constr ,constr) t)
	    ,(if total-min `(:>= :total ,total-min) t)
	    ,(if total-max `(:<= :total ,total-max) t)
	    ,(if price-min `(:>= :price ,price-min) t)
	    ,(if price-max `(:<= :price ,price-max) t)
	    ,(if bedrooms-min `(:<= :bedrooms ,bedrooms-min) t)
	    ,(if bathrooms-min `(:<= :bathrooms ,bathrooms-min) t)
	    ,(if terrace `(:= :terrace-p ,terrace) t)
	    ,(if garden `(:= :garden-p ,garden) t)
	    ,(if building-permit `(:= :building-permit-p ,building-permit) t)
	    ,(if (spec-f-p summons) `(:= :summons ,summons) t)
	    ,(if (spec-f-p preemption) `(:= :preemption ,preemption) t)
	    ,(if (spec-f-p subdiv-permit) 
		 `(:= :subdiv-permit ,subdiv-permit) t))
	   )
	  ,count ,offset))))))