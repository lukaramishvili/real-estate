(in-package :re)

(defparameter *test-images-path* "/re-test-images/")
#+:WINDOWS-TARGET
(defparameter *test-images-path* "C:/users/Public/Documents/Images/")

(defun random-bool ()
    (= 1 (random 2)))

(defun random-string 
    (length 
     &key (bag "abc def ghi jkl l ak fa sl df ka we a sd ks d"))
  (apply #'+s 
	 (loop for i from 0 to (1- length)
	    collecting 
	      (let ((r (random (length bag))))
		(subseq bag r (1+ r))))))

(defun random-from (list)
  (nth (random (length list)) list))

(defun random-car-from (list)
  (car (nth (random (length list)) list)))

(defun make-random-estate ()
  (make-instance 
   'estate :address (random-string (random 90))
   :telnum (random-string (random 15) :bag "+-() 0123456789")
   :loc-lat (+s (random 60) "." (random 2593453592340))
   :loc-lng (+s (random 60) "." (random 2593453592340))
   :visible t :apt-type (random-from (apt-type-options))
   :status (random-from (status-options))
   :pst-code (random-string (random 6) :bag "0123456789")
   :munic (random-string (random 16))
   :ix-country (random-car-from (all-countries))
   :constr (random-from (constr-options)) :total (random 300) 
   :land (random 2000) :desc (random-string (random 2000))
   :zmh (random-string 6 :bag "0123456789") :price (random 500000.0)
   :since (+ 3554007995 (random 30000)) :bedrooms (random 5)
   :bathrooms (random 6) :terrace-p (random-bool) :garden-p (random-bool)
   :parking-lots (random 6) :building-permit-p (random-bool)
   :destination (random-string (random 30)) 
   :summons (random-car-from (summons-options))
   :preemption (random-car-from (preemption-options))
   :subdiv-permit (random-car-from (subdiv-permit-options))
   :epc (random 145.0) :kad-ink (random 5000.0)))

(defun cook-temp-pic-file ()
  "copies a img file to a temp location, and returns path"
  (let* ((source-path (random-from (cl-fad:list-directory 
				   *test-images-path*)))
	 (tmp-uuid (uuid:make-v4-uuid))
	 (dest-path (+s *project-tmp-dir* tmp-uuid ".jpg")))
    (cl-fad:copy-file source-path dest-path)
    dest-path))

(defun make-random-temp-pic ()
  (make-instance 'pic :path (cook-temp-pic-file)
		 :order 0 :ix-estate 0))

(defun add-random-estate-with-pics (pic-count)
  (let ((e (make-random-estate))
	(pics (loop for i from 1 to pic-count
		   collecting (make-random-temp-pic))))
    (let ((save-results (save-estate-and-pics 0 e pics)))
      (destructuring-bind
	    (success-p ix-saved-e error-message)
	  save-results
	;;set ix-main-pic of the saved estate
	(with-re-db
	  (let ((e-after-save (get-dao 'estate ix-saved-e)))
	    (setf (ix-main-pic e-after-save) 
		  (ix-pic (random-from pics)))
	    (save-dao e-after-save)
	    save-results))))))

(defun update-dao-table-preserve-data (table-class)
  (with-re-db
    (let ((existing-rows (select-dao table-class)))
      (query (:drop-table table-class))
      (execute (dao-table-definition table-class))
      (loop for r in existing-rows
	   do (save-dao r)))))

