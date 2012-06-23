(in-package :re)

(defun +s (&rest args)
  (apply #'concatenate
	 (cons 'string
	       (mapcar
		#'(lambda (s) 
		    (if (stringp s)
			s
			(format nil "~a" s)))
		args))))

(defun smake (&rest args)
  "If the first argument is a list, concatenates first argument items. 
   Otherwise, concatenates argument list."
  (if (listp (car args))
      (format nil "~{~a~}" (car args))
      (format nil "~{~a~}" args)))

(defun hash-password (password)
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence 
    :sha256 
    (ironclad:ascii-string-to-byte-array password))))

(defun unix-time-from-universal (univ-to-convert)
  (let ((unix-difference (encode-universal-time 0 0 0 1 1 1970 0)))
    (- univ-to-convert unix-difference)))

(defun universal-time-from-unix (unix-time)
  (let ((unix-difference (encode-universal-time 0 0 0 1 1 1970 0)))
    (+ unix-time unix-difference)))

(defun rss-skeleton
  (&key (title "") (link-self "") (link-alt "") (subtitle "")
	(class "") (updated "") (id "") entries)
  (cl-who:with-html-output-to-string
   (*standard-output*
    nil :prologue "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    :indent t)
   (:feed :xmlns "http://www.w3.org/2005/Atom"
	 :|xmlns:re| "http://purl.org/atompub/rank/1.0"
	 (:title :type "text" title)
	 (:link :rel "self" :href link-self
	       :type "application/atom+xml")
	 (:link :rel "alternate" :href link-alt :type "text/html")
	 (:subtitle subtitle)
	 (:updated updated)
	 (:id id)
	 (cl-who:str entries))))

(defun rss-entry
  (&key (id "") (title "") (category-scheme "") (categories nil)
	(author-name "") (author-uri "") (link-alt "")
	(published "") (updated "") (summary ""))
  (cl-who:with-html-output-to-string
   (*standard-output* nil :prologue nil :indent t)
   (:entry
    (:id (cl-who:str id))
    (:title :type "text" (cl-who:str title))
    (loop for cat in categories
	  do (cl-who:htm
	      (:category :scheme category-scheme :term cat)))
    (:author
     (:name (cl-who:str author-name))
     (:uri (cl-who:str author-uri)))
    (:link :rel "alternate" :href link-alt)
    (:published (cl-who:str published))
    (:updated (cl-who:str updated))
    (:summary :type "html" (cl-who:str summary)))))


(defun rss-entries-from-class
  (objects &key id title category-scheme categories author-name
	   author-uri link-alt published updated summary)
  "keyword arguments passed (symbols) will be used as slot names"
  (reduce
   #'+s
   (loop
    for o in objects collecting
    (let ((arglist nil))
      (progn
	(if id (setf (getf arglist :id) (slot-value o id)))
	(if title (setf (getf arglist :title) (slot-value o title)))
	(if category-scheme
	    (setf (getf arglist :category-scheme)
		  (slot-value o category-scheme)))
	(if categories (setf (getf arglist :categories)
			     (slot-value o categories)))
	(if author-name (setf (getf arglist :author-name)
			      (slot-value o author-name)))
	(if author-uri (setf (getf arglist :author-uri)
			     (slot-value o author-uri)))
	(if link-alt (setf (getf arglist :link-alt)
			   (slot-value o link-alt)))
	(if published (setf (getf arglist :published)
			    (slot-value o published)))
	(if updated (setf (getf arglist :updated)
			  (slot-value o updated)))
	(if summary (setf (getf arglist :summary)
			  (slot-value o summary)))
	(apply #'rss-entry arglist))))))