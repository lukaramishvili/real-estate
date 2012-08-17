(in-package :re)

(ql:quickload :css-lite)

(defun abs-pos (div x y w h)
  (css-lite:css ((div) ((:position "absolute")
	       (:left x) (:top y) (:width w) (:height h)))))

(defun style-firstpage ()
  (+s
   "
  body { background-color:black; }
  #search-bar { position:fixed; left:-250px; top:0px; width:250px; height:100%;
    background:transparent url(css/img/gray-bg.png) left top repeat; 
    padding:12px 10px 0px 12px; }
  #btn-toggle-search { position:absolute; right:-25px; top:45%; color:white;
    display:block; width:45px; height:45px;
    background:transparent url(css/img/search-btn.png) left top no-repeat; }
  
  .fancybox-skin { background:transparent url(css/img/gray-bg.png) 
      left top repeat !important; }

  #search-bar input,#search-bar label,#search-bar select 
      { float:left; display:block; margin-bottom:4px; }
  #search-bar label { width:100px; color:#ccc; }
  #search-bar label.label-left { clear: left; }
  #search-bar label.label-right { clear: right; }
  #search-bar input[type='checkbox'] { clear:left; }

  #fp-pics { height:100%; }
  /*#fp-pics a img {  }*/
  #fp-pics .table-cont { /*width:1500px;*/ float:left; }
  .fp-estate-link img { width:150px; height:150px; margin:0px 1px 0px 1px; }
  td.td-4x a img { width:300px; height:300px; }
  
  
  .grid-10 { width:20%; float:left; height:100%; }
  .grid-10 a { width:100%; height:18.6%; display:block; margin:1px; }
  #view-estate { display:none; width:900px; height:500px; color:white; }
  #view-estate #estate-main-img { width:250px; height:250px; }
  #view-estate #estate-images { width:250px; float:left; margin-right:20px; }
  #view-estate #other-imgs { width:250px; }
  #view-estate #other-imgs img { width:50px; height:50px; }
  #view-estate #estate-fields { width:250px; float:left; margin-right:20px; }
  #view-estate #estate-map-div { width:250px; float:left; height:250px;
      margin-right:20px; }
  #view-estate #single-estate-map { width:250px; height:250px; }
  "))

(defun style-edit-estate-form ()
  "
  #edit-estate-form-div { background-color:white; }
  #edit-estate-form-div { padding:20px; }
  #edit-estate-form-div iframe { border:0; height:auto; width:auto; 
    width:400px; height:150px;
  }
  #edit-estate-map { width:300px; height:300px; }
  
  #edit-estate-form-div h4 { clear:both; }
  #edit-estate-form-div input,#edit-estate-form-div label
   ,#edit-estate-form-div select { float:left; display:block;
      margin-bottom:4px; }
  #edit-estate-form-div label { width:150px; }
  #edit-estate-form-div label.label-left { clear: left; }
  #edit-estate-form-div label.label-right { clear: right; }
  #edit-estate-form-div input[type='checkbox'] 
      { clear:left; margin-right:135px; }
  #edit-estate-form-div input[type='submit'] 
      { clear:both; }
  
  #estate-pics { clear:both; }
  ")
(defun style-pic-box-iframe()
  "
  .div-in-pic-box-iframe img { max-width:150px; height:80px; }
  ")

(defun re-gen-css ()
  (+s
   (css-lite:css
     (("body") ((:height "100%")))
     (("#main-container") ((:position "absolute") (:left "0px") 
			   (:top "0px") (:width "100%" :height "100%")))
     (("#main") ((:position "absolute") (:left "0px") (:top "0px")
		 (:width "100%" :height "100%")))
     ((".grid-10") ((:width "10%") (:float "left") (:height "100%"))))
   (style-firstpage)
   (style-edit-estate-form)))