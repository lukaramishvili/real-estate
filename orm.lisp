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

(defclass tr ()
  ((ix-tr :col-type serial :initarg :ix-tr :accessor ix-tr)
   (keyword :col-type string :initarg :keyword :accessor keyword :initform "")
   (lang :col-type string :initarg :lang :accessor lang :initform "")
   (value :col-type string :initarg :value :accessor value :initform ""))
  (:metaclass dao-class)
  (:keys ix-tr))

(defun tr (keyword lang)
  (with-re-db
   (or (query (:select
	       :value :from :tr
	       :where (:and (:= :lang (string-downcase (smake lang)))
			    (:= :keyword (string-downcase (smake keyword)))))
	      :single)
       (string-capitalize 
	(string-downcase (substitute #\Space #\- (+s keyword)))))))

(defun tr-if-exists (keyword lang)
  (with-re-db
    (car 
     (query-dao 
      'tr 
      (:select
       :* :from :tr
       :where (:and (:= :lang (string-downcase (smake lang)))
		    (:= :keyword (string-downcase (smake keyword)))))))))

(defun add-tr (keyword lang value)
  (with-re-db 
    (let ((tr-to-save 
	   (or (tr-if-exists keyword lang) 
	       (make-instance 'tr :keyword (string-downcase (smake keyword))
			      :lang (string-downcase (smake lang))
			      :value (string-downcase (smake value))))))
      (setf (value tr-to-save) value)
      (save-dao tr-to-save)
      tr-to-save)))

(defun all-tr (lang)
  (with-re-db (select-dao 'tr (:= :lang (string-downcase (smake lang))))))

(defun wrap-like (val)
  (+s "%" val "%"))
    
    

(defun obfuscate-password (passwd)
  (hash-password (+s "sloboda" passwd)))

(defun valid-acc-types ()
  (list "simple" "broker"))

(defun valid-acc-type-p (acc-type)
  (if (member acc-type (valid-acc-types) 
	      :test #'string=)
      t))

(defun status-list ()
  (list :activated 1
	:verified 2))

(defun status->int (status)
  (getf (status-list) status))

(defun role-list ()
  (list :admin 1
	:administrator 1
	:broker 2
	:simple 3))

(defun role->int (role)
  (getf (role-list) role))

(defclass user ()
  ((ix-user :col-type serial :initarg :ix-user :accessor ix-user)
   (email :col-type string :initarg :email :accessor email :initform "")
   (fname :col-type string :initarg :fname :accessor fname :initform "")
   (lname :col-type string :initarg :lname :accessor lname :initform "")
   (url :col-type string :initarg :url :accessor url :initform "")
   (telnum :col-type string :initarg :telnum :accessor telnum :initform "")
   (username :col-type string :initarg :username :accessor username :initform "")
   (passwd :col-type string :initarg :passwd :initform "")
   (acc-type :col-type string :initarg :acc-type :accessor acc-type :initform "")
   (role :col-type integer :initarg :role :accessor role :initform 0)
   (status :col-type integer :initarg :status :accessor status :initform 0))
  (:metaclass dao-class)
  (:keys ix-user))

(defun all-users ()
  (with-re-db (select-dao 'user)))

(defun single-user (ix-user)
  (with-re-db
    (get-dao 'user ix-user)))

(defun user-if-valid (username pass)
  (with-re-db
    (let ((matched-users
	   (select-dao 'user
		       (:and (:= :username username)
			     (:= :passwd (obfuscate-password pass))))))
      (if (< 0 (length matched-users))
	  (car matched-users)))))

(defun user-with-username (username)
  (with-re-db (car (select-dao 'user (:= :username username)))))

(defun user-with-email (email)
  (with-re-db (car (select-dao 'user (:= :email email)))))

(defun save-user (user)
  (with-re-db 
    (insert-dao user)
    (ix-user user)))

(defun update-user (user)
  (with-re-db
   (update-dao user)))

;;;;;;;; project-specific code

(defun spec-f-p (filter)
  "specific-filter-p: returns true if the filter is specific 
   (not including everything)"
  (and filter 
       (not (string= filter "*"))))

(defun apt-type-options (&key not-sel)
  (concatenate 'list 
	       (if not-sel (list "*"))
	       (list (list "apartment" (tr :apartment :nl))
		     (list "house" (tr :house :nl))
		     (list "land" (tr :land :nl))
		     (list "office" (tr :office :nl))
		     (list "commercial" (tr :commercial :nl))
		     (list "garage" (tr :garage :nl))
		     (list "new" (tr :new :nl)))))

(defun status-options (&key not-sel)
  (concatenate 'list 
	       (if not-sel (list "*"))
  (list (list "sale" (tr :sale :nl)) 
	(list "rent" (tr :rent :nl)))))

(defun all-countries (&key not-sel)
  (concatenate 'list 
	       (if not-sel (list "*"))
	       (list (list 1 (tr :belgium :nl))
		     (list 2 (tr :niederlands :nl))
		     (list 3 (tr :france :nl)) 
		     (list 4 (tr :spain :nl)))))

(defun constr-options (&key not-sel)
  (concatenate 'list 
	       (if not-sel (list "*" "*"))
  (list "detached" "terraced" "semi-detached")))

(defun summons-options (&key not-sel)
  (concatenate 'list 
	       (if not-sel (list "*" "*"))
  (list (list "nvt" "NVT") (list "vt" "VT"))))

(defun preemption-options (&key not-sel)
  (concatenate 'list 
	       (if not-sel (list "*" "*"))
  (list (list "nvt" "NVT") (list "vt" "VT"))))

(defun subdiv-permit-options (&key not-sel)
  (concatenate 'list 
	       (if not-sel (list "*" "*"))
  (list (list "nvt" "NVT") (list "vt" "VT"))))

(defun destination-options (&key not-sel)
  (concatenate 'list 
	       (if not-sel (list "*" "*"))
  `((1 "Agrarisch gebied")
    (5 "Bosgebied")
    (2 "Gebied met economische activiteiten")
    (16 "Gebied voor dagrecreatie")
    (17 "Gebied voor verblijfsrecreatie")
    (8 "Gemengd woongebied")
    (12 "Groengebied")
    (4 "Grondreservegebied (woonuitbreidingsgebied)")
    (6 "Industriegebied")
    (15 "Industriegebied voor ambachtelijke bedrijven of gebieden voor kleine en middelgrote ondernemingen")
    (18 "Landschappelijk waardevol agrarisch gebied")
    (9 "Natuurgebied")
    (19 "Natuurreservaat")
    (20 "Niet ingegeven")
    (10 "Parkgebied")
    (7 "Recreatiegebied")
    (3 "Winningsgebied")
    (11 "Woongebied")
    (13 "Woongebied met een culturele historische en/of esthetische waarde")
    (14 "Woonpark"))))

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

(defun all-users-paged (page &key (per-page 10))
  (with-re-db 
  (query-dao 'user 
	     (:limit (:order-by (:select :* :from :user) :ix-user)
		     per-page (* (- page 1) per-page)))))

(defun all-estates-paged (page &key (per-page 10))
  (with-re-db 
  (query-dao 'estate 
	     (:limit (:order-by (:select :* :from :estate) :ix-estate)
		     per-page (* (- page 1) per-page)))))

(defclass pic ()
  ((ix-pic :col-type serial :initarg :ix-pic :accessor ix-pic)
   (ix-estate :col-type integer :initarg :ix-estate 
	      :accessor ix-estate :initform 0)
   (order :col-type integer :initarg :order :accessor order :initform 0)
   (path :col-type string :initarg :path :accessor path :initform ""))
  (:metaclass dao-class)
  (:keys ix-pic))

(defclass fav ()
  ((ix-fav :col-type serial :initarg :ix-fav :accessor ix-fav)
   (ix-user :col-type integer :initarg :ix-user 
	      :accessor ix-user :initform 0)
   (ix-estate :col-type integer :initarg :ix-estate 
	      :accessor ix-estate :initform 0))
  (:metaclass dao-class)
  (:keys ix-fav))

(defun zml-app-type-options (&key not-sel)
  (concatenate 'list 
	       (if not-sel (list "*" "*"))
  (list (list "express" "Express") (list "advanced" "Advanced"))))

(defclass zml-app ()
  ((ix-zml-app :col-type serial :initarg :ix-zml-app :accessor ix-zml-app)
   (date :col-type bigint :initarg :date :accessor date :initform 0)
   (filled-form :col-type string :initarg :filled-form :accessor filled-form
		:initform 0))
  (:metaclass dao-class)
  (:keys ix-zml-app))

(defclass zml-app-with-fields ()
  ((ix-zml-app :col-type serial :initarg :ix-zml-app :accessor ix-zml-app)
   (app-type :col-type string :initarg :app-type :accessor app-type 
	      :initform "")
   ;; common fields - used both in express and advanced applications
   (first-name :col-type string :initarg :first-name :accessor first-name 
		:initform "")
   (last-name :col-type string :initarg :last-name :accessor last-name 
	       :initform "")
   (email :col-type string :initarg :email :accessor email :initform "")
   (phone :col-type string :initarg :phone :accessor phone :initform "")
   ;; express fields
   (amount :col-type string :initarg :amount :accessor amount :initform "")
   (start :col-type bigint :initarg :start :accessor start :initform "")
   (end :col-type bigint :initarg :end :accessor end :initform "")
   (formula :col-type string :initarg :formula :accessor formula :initform "")
   ;; advanced fields - calculator
   (b9 :col-type string :initarg :b9 :accessor b9 :initform "")
   (b10 :col-type string :initarg :b10 :accessor b10 :initform "")
   (b11 :col-type string :initarg :b11 :accessor b11 :initform "")
   (b12 :col-type string :initarg :b12 :accessor b12 :initform "")
   (b13 :col-type string :initarg :b13 :accessor b13 :initform "")
   (b15 :col-type string :initarg :b15 :accessor b15 :initform "")
   (b16 :col-type string :initarg :b16 :accessor b16 :initform "")
   (b18 :col-type string :initarg :b18 :accessor b18 :initform "")
   (b19 :col-type string :initarg :b19 :accessor b19 :initform "")
   (b21 :col-type string :initarg :b21 :accessor b21 :initform "")
   (b22 :col-type string :initarg :b22 :accessor b22 :initform "")
   (b25 :col-type string :initarg :b25 :accessor b25 :initform "")
   (b28 :col-type string :initarg :b28 :accessor b28 :initform "")
   (b29 :col-type string :initarg :b29 :accessor b29 :initform "")
   (b30 :col-type string :initarg :b30 :accessor b30 :initform "")
   (b31 :col-type string :initarg :b31 :accessor b31 :initform "")
   (b32 :col-type string :initarg :b32 :accessor b32 :initform "")
   (calc-result :col-type string :initarg :calc-result :accessor calc-result 
		 :initform "")
   ;; advanced fields - contact fields
   ;; email and phone are included in common fields
   (gsm-phone :col-type string :initarg :gsm-phone :accessor gsm-phone
	       :initform "")
   (comment :col-type string :initarg :comment :accessor comment :initform "")
   )
  (:metaclass dao-class)
  (:keys ix-zml-app))

(defclass zml-creditor ()
  ((ix-zml-creditor :col-type serial :initarg :ix-zml-creditor
		    :accessor ix-zml-creditor)
   (name :col-type string :initarg :name :accessor name :initform "")
   (birth :col-type bigint :initarg :birth :accessor birth :initform "")
   (regnum :col-type string :initarg :regnum :accessor regnum :initform "")
   (street :col-type string :initarg :street :accessor street :initform "")
   (city :col-type string :initarg :city :accessor city :initform "")
   (postcode :col-type string :initarg :postcode :accessor postcode 
	      :initform "")
   (marital :col-type string :initarg :marital :accessor marital :initform "")
   (worker-type :col-type integer :initarg :worker-type :accessor worker-type
		 :initform "")
   ;; TODO: fields for each worker-type (arbeider bediende etc.)
   )
  (:metaclass dao-class)
  (:keys ix-zml-creditor))

(defclass zml-loan ()
  ((ix-zml-loan :col-type serial :initarg :ix-zml-loan :accessor ix-zml-loan)
   (type :col-type string :initarg :type :accessor type :initform "")
   (bank :col-type string :initarg :bank :accessor bank :initform "")
   (amount :col-type string :initarg :amount :accessor amount :initform "")
   (outstanding :col-type string :initarg :outstanding :accessor outstanding
		 :initform "")
   (start-date :col-type string :initarg :start-date :accessor start-date
		:initform "")
   (maturity :col-type string :initarg :maturity :accessor maturity
	      :initform "")
   (interest-rate :col-type string :initarg :interest-rate
		   :accessor interest-rate :initform "")
   (monthly-payment :col-type string :initarg :monthly-payment 
		     :accessor monthly-payment :initform "")
   (take-over :col-type string :initarg :take-over :accessor take-over
	      :initform ""))
  (:metaclass dao-class)
  (:keys ix-zml-loan))


(defun user-has-fav (ix-user ix-estate)
  (< 0
     (with-re-db 
       (query (:select (:count :*) :from :fav :where 
		       (:and (:= :ix-user ix-user) 
			     (:= :ix-estate ix-estate))) 
	      :single))))

(defun add-fav (ix-user ix-estate)
  (with-re-db 
    (save-dao (make-instance 'fav :ix-user ix-user 
			     :ix-estate ix-estate))))

(defun ensure-fav (ix-user ix-estate)
  (or (user-has-fav ix-user ix-estate)
      (add-fav ix-user ix-estate)))

(defun remove-fav (ix-user ix-estate)
  (with-re-db 
    (query (:delete-from :fav :where (:and (:= :ix-user ix-user) 
					   (:= :ix-estate ix-estate))) 
	   :single))
  t)

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
		   ,(if (not include-main-pic) 
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

(defun max-e-price-for-dmp (dmp) ; dmp is b27
  "Calculates how costly house you can buy by paying no more than ,dmp monthly"
  (let* ((apr 0.0375) ; apr = annual percentage rate, b28
	 (mir (- (expt (1+ apr) 1/12) 1)) ; mir = monthly interest rate, b29
	 (duration 300)) ; duration, in months, b30
    (- (pv mir duration dmp))))

(defun filter-estates (filters-alist &key (count 10000) (offset 0))
  (let* ((fa filters-alist)
	 (ix-user (cdr (assoc :ix-user fa)))
	 (only-favs-of-user (cdr (assoc :only-favs-of-user fa)))
	 (apt-type (cdr (assoc :apt-type fa)))
	 (status (cdr (assoc :status fa)))
	 (ix-country (cdr (assoc :ix-country fa)))
	 (constr (cdr (assoc :constr fa)))
	 (total-min (cdr (assoc :total-min fa)))
	 (total-max (cdr (assoc :total-max fa)))
	 (price-min (cdr (assoc :price-min fa)))
	 (price-max (cdr (assoc :price-max fa)))
	 (desired-monthly-pay (cdr (assoc :desired-monthly-pay fa)))
	 (bedrooms-min (cdr (assoc :bedrooms-min fa)))
	 (bedrooms-max (cdr (assoc :bedrooms-max fa)))
	 (bathrooms-min (cdr (assoc :bathrooms-min fa)))
	 (bathrooms-max (cdr (assoc :bathrooms-max fa)))
	 (terrace (cdr (assoc :terrace fa)))
	 (garden (cdr (assoc :garden fa)))
	 (building-permit (cdr (assoc :building-permit fa)))
	 (summons (cdr (assoc :summons fa)))
	 (preemption (cdr (assoc :preemption fa)))
	 (subdiv-permit (cdr (assoc :subdiv-permit fa)))
	 (epc-min (cdr (assoc :epc-min fa)))
	 (epc-max (cdr (assoc :epc-max fa)))
	 (postcode-1 (cdr (assoc :postcode-1 fa)))
	 (postcode-2 (cdr (assoc :postcode-2 fa)))
	 (postcode-3 (cdr (assoc :postcode-3 fa))))
    (with-re-db
      (eval 
       `(query-dao 'estate
	 (:limit (:select :* :from :estate :where
	   (:and 
	    t
	    ,(if (spec-f-p ix-user) `(:= :ix-user ,ix-user) t)
	    ,(if (spec-f-p only-favs-of-user) 
		 `(:exists (:select :* :from :fav :where 
			       (:and (:= :fav.ix-user ,only-favs-of-user)
				     (:= :fav.ix-estate :estate.ix-estate))))
		 t)
	    ,(if (spec-f-p apt-type) `(:= :apt-type ,apt-type) t)
	    ,(if (spec-f-p status) `(:= :status ,status) t)
	    ,(if (spec-f-p ix-country) `(:= :ix-country ,ix-country) t)
	    ,(if (spec-f-p constr) `(:= :constr ,constr) t)
	    ,(if total-min `(:>= :total ,total-min) t)
	    ,(if total-max `(:<= :total ,total-max) t)
	    ,(if price-min `(:>= :price ,price-min) t)
	    ,(if price-max `(:<= :price ,price-max) t)
	    ;;desired monthly pay calculates maximum estate price (m.e.p) which 
	    ;;can be purchased by paying no more than d.m.p amount monthly.
	    ;;if both d.m.p. and price-max filters exist, then just find estates
	    ;;with both (> price price-max) [above] and (> price m.e.p) [below]
	    ,(if desired-monthly-pay 
		 `(:<= :price ,(max-e-price-for-dmp 
				(read-from-string desired-monthly-pay)))
		 t)
	    ,(if bedrooms-min `(:>= :bedrooms ,bedrooms-min) t)
	    ,(if bedrooms-max `(:<= :bedrooms ,bedrooms-max) t)
	    ,(if bathrooms-min `(:>= :bathrooms ,bathrooms-min) t)
	    ,(if bathrooms-max `(:<= :bathrooms ,bathrooms-max) t)
	    ,(if terrace `(:= :terrace-p ,terrace) t)
	    ,(if garden `(:= :garden-p ,garden) t)
	    ,(if building-permit `(:= :building-permit-p ,building-permit) t)
	    ,(if (spec-f-p summons) `(:= :summons ,summons) t)
	    ,(if (spec-f-p preemption) `(:= :preemption ,preemption) t)
	    ,(if (spec-f-p subdiv-permit) 
		 `(:= :subdiv-permit ,subdiv-permit) t)
	    ,(if (spec-f-p epc-min) `(:>= :epc ,epc-min) t)
	    ,(if (spec-f-p epc-max) `(:<= :epc ,epc-max) t)
	    
	    ,(if (or (spec-f-p postcode-1)
		     (spec-f-p postcode-2)
		     (spec-f-p postcode-3)) 
	       `(:or ,(if (spec-f-p postcode-1) `(:= :pst-code ,postcode-1) nil)
		     ,(if (spec-f-p postcode-2) `(:= :pst-code ,postcode-2) nil)
		     ,(if (spec-f-p postcode-3) `(:= :pst-code ,postcode-3) nil))
	       t)
	    ))
	  ,count ,offset))))))

(defun estates-of-user (ix-user)
  (filter-estates `((:ix-user . ,(+s ix-user)))))

