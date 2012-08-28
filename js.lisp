(in-package :re)

(ql:quickload :parenscript)

(defpsmacro $$ (selector &body chains)
  `(chain (j-query ,selector)
      ,@chains))

(defpsmacro += (var &rest what-to-append)
      `(setf ,var (+ ,var ,@what-to-append)))

(defun re-main-js ()
  (+s 
   "
   //by default, one page = 9x4 images
   var imgsPerRow = 9;
   var imgsPerCol = 4;
   var bigImgsPerPage = 1;
   
   //ch - img count horizontally, cv - img count vertically
   function imgsNeededPerPage (ch, cv, bigImgCount){
     return ch * cv - bigImgCount * 3;
   }
   
   function imgsLoadedForNow(){
     return $('.fp-estate-link').length;
   }
   
   var currPage = 1;
   var lastPage = 1;//max. page no. that's currently loaded
   var wCarImg = 1300;//carousel image width
   var fAllowFurtherScrolling = true;

   $('html').mousewheel(function(event, delta) {
     if(fAllowFurtherScrolling){
       $('html')[0].scrollLeft -= (delta * 100);
       event.preventDefault();
       //
       var carScroll = $('html')[0].scrollLeft;
       if((1 + (carScroll - carScroll%wCarImg)/wCarImg) != lastPage){
         currPage = 1 + ((carScroll - carScroll%wCarImg)/wCarImg);
         if(currPage >= lastPage){
           fAllowFurtherScrolling = false;
           loadResults({ count: 99, offset: imgsLoadedForNow(), 
             callback: function(){
               fAllowFurtherScrolling = true;
               lastPage += 3;
             }, clearPrevs: false });
         }
       }
     }
   });

   function estateIdFromArgument(a){
     var m = a.match('estate-[0-9]+');
     if(m){
       var d = m[0].toString().match('[0-9]+');
       return d ? d[0] : 0;
     }
     else{
       return 0;
     }
   }
   function linkForEstate (ix){
     //TODO: generate using document.location
     return 'http://' + document.location.host + '/#estate-' + ix;
   }
   "
   (ps:ps
     (defun get-estate (id callback)
       (chain 
	$ (ajax
	   (create
	    url "./get-estate"
	    data (create id id)
	    data-type "json"
	    success (lambda (d)
		      (callback d))
	    error (lambda ())
	    )))
       (return false))
     
     (defun gen-estate-div (e)
       (let ((fields (@ e fields))
	     (main-pic (@ e main-pic))
	     (other-pics (@ e other-pics)))
	 (var div "<div>")
	 (+= div "<div id='estate-images'>")
	 (+= div "<a href='" (@ main-pic path) 
	     "' id='estate-main-img-a' "
	     " rel='estate-gallery'>" "<img id='estate-main-img' src='" 
	     (@ main-pic path) "' /></a>")
	 (+= div "<div id='other-imgs'>")
	 (for-in 
	  (op other-pics)
	  (let ((next-img (@ (aref other-pics op) path)))
	    (+= div "<a href='" next-img "' rel='estate-gallery'>"
		"<img src='" next-img "' /></a>")))
	 (+= div "</div>");</#other-imgs>
	 (+= div "<a id='estate-toggle-fav'>Favorite</a>" "<br><br>")
	 (+= div (fb-like-btn (link-for-estate (@ e ix-estate))) "<br><br>")
	 (+= div (fb-share-btn (link-for-estate (@ e ix-estate))) "<br><br>")
	 (+= div "</div>");</#estate-images>
	 (+= div "<div id='estate-fields'>")
	 (for-in (k fields)
		 (+= div k ": " (aref fields k) "<br>"))
	 (+= div "</div>");</#estate-fields>
	 (+= div "<div id='estate-map-div'>")
	 (+= div "<div id='single-estate-map'>")
	 (+= div "</div>");</#estate-map>
	 (+= div "</div>");</#estate-map-div>
	 (+= div "</div>")
	 div))

     (defun fill-estate-div (estate-div)
       ($$ "#view-estate-inner" (html estate-div)))

     (defun show-estate-div ()
       ($$ "#view-estate" (show)))
     
     (defun hide-estate-div ()
       ($$ "#view-estate" (hide))
       ;;remove #estate-\d+ from the url after closing
       (setf document.location.href 
	     (chain document.location.href
		    (replace document.location.hash 
			     "#"))))       
     
     (defun view-e (id)
       ($$ "#fp-preloader" (show))
       (get-estate 
	id (lambda (e)
	     (if (!= e null)
		 (progn 
		   (fill-estate-div (gen-estate-div e))
		   ($$ "#estate-main-img-a,#other-imgs > a" (fancybox))
					;($$ "" (fancybox))
		   (when (not (= "undefined" (typeof google)))
		     (defvar estate-loc (new (google.maps.-lat-lng 
					      (@ e loc-lat)
					      (@ e loc-lat))))
		     (defvar estate-map 
		       (create-map-for-id "single-estate-map"))
		     (defvar loc-marker 
		       (create-marker "Real estate map location"
				      estate-loc))
		     (chain loc-marker (set-map estate-map))
		     (chain estate-map (set-center estate-loc)))
		   (show-estate-div))
		 (alert "Loading estate failed, please try again."))
	     e))
       ($$ "#fp-preloader" (hide))
       (return false))

     ($$ ".fp-estate-link"
	 (live "click" (lambda () 
			 (view-e ($$ this (attr "ixestate")))
			 ;;return true so #estate-\d+ becomes current url
			 t)))
     );end ps:ps
     "
    function createMapForId (id, options){
    	var initPos = new google.maps.LatLng(41.5, 44.8);
        var DefaultOptions = {
          center: initPos,
          zoom: 8,
          mapTypeId: google.maps.MapTypeId.ROADMAP
        };
        options = options || {};
        for(var argOpt in options){
            DefaultOptions[argOpt] = options[argOpt];
        }
        var map = new google.maps.Map(document.getElementById(id),
            DefaultOptions);
        return map;
    }
    function createMarker(title, pos){
      var marker = new google.maps.Marker({
    	position: pos,
    	title:    title
      });
      return marker;
    }
    function MapMarkerACCombo (selInput, selLat, selLong, argMap, argMarker) {
            var geocoder = new google.maps.Geocoder();
	    $(selInput).autocomplete({
	      //This bit uses the geocoder to fetch address values
	      source: function(request, response) {
		geocoder.geocode( {'address': request.term }, 
                                  function(results, status) {
		  response($.map(results, function(item) {
		    return {
		      label:  item.formatted_address,
		      value: item.formatted_address,
		      latitude: item.geometry.location.lat(),
		      longitude: item.geometry.location.lng()
		    }
		  }));
		})
	      },
	      //This bit is executed upon selection of an address
	      select: function(event, ui) {
		$(selLat).val(ui.item.latitude);
		$(selLong).val(ui.item.longitude);
		var location = new google.maps.LatLng(ui.item.latitude, 
                                                      ui.item.longitude);
		argMarker.setPosition(location);
		argMap.setCenter(location);
	      }
	    });
		
	  //Add listener to marker for reverse geocoding
	  google.maps.event.addListener(argMarker, 'drag', function() {
	    geocoder.geocode({'latLng': argMarker.getPosition()}, 
                             function(results, status) {
	      if (status == google.maps.GeocoderStatus.OK) {
		if (results[0]) {
		  $(selInput).val(results[0].formatted_address);
		  $(selLat).val(argMarker.getPosition().lat());
		  $(selLong).val(argMarker.getPosition().lng());
		}
	      }
	    });
	  });	  
    }
    /*end function MapMarkerACCombo*/
    
    /* facebook share */
    function fbs_click() {u=location.href;t=document.title;
        window.open('http://www.facebook.com/sharer.php?u='
        + encodeURIComponent(u)+'&t='+encodeURIComponent(t),'sharer',
        'toolbar=0,status=0,width=626,height=436');return false;
    }
    /* end facebook share */
    /* generate fb like btn */
    function fbLikeBtn(arg_url){
      var url = encodeURIComponent(arg_url);
      var w = 170;
      var h = 35;
      return '<iframe src=\\'//www.facebook.com/plugins/like.php?href=' + url + '&amp;send=false&amp;layout=standard&amp;width='+ w + '&amp;show_faces=false&amp;action=like&amp;colorscheme=dark&amp;font=arial&amp;height=' + h + '\\' scrolling=\\'no\\' frameborder=\\'0\\' style=\\'border:none; overflow:hidden; width:' + w + 'px; height:' + h + 'px;\\' allowTransparency=\\'true\\'></iframe>';
    }
    /* end generate fb like btn */
    /* generate facebook share btn */
    function fbShareBtn (url, arg_caption){
      var caption = arg_caption ? arg_caption : 'Share on Facebook';
      return '<a rel=\\'nofollow\\' href=\\'http://www.facebook.com/share.php?u=' + url + '\\' onclick=\\'return fbs_click()\\' target=\\'_blank\\' class=\\'fb_share_link\\'>' + caption + '</a>';
    }
    /* end generate facebook share btn */
    
    "))

(defun fp-search-js ()
  (+s 
   "
  var fSearchOpen = false;
  function toggleSearchBar(){
    hideEstateDiv();
    $('#search-bar').animate({ 'left' : fSearchOpen ? -272 : 0 }, 'slow');
    /*$('#top-menu').animate({ 'padding-left' : fSearchOpen ? 12 : 282 }, 
      'slow');*/
    $('#main').animate({ 'padding-left' : fSearchOpen ? 0 : 250 }, 'slow');
    fSearchOpen = !fSearchOpen;
  }
  $('#btn-toggle-search').click(function(){
    toggleSearchBar();
  });
  "
   (ps
     (defun inp-pos-val (selector)
       (let ((val ($$ selector (val))))
	 (and (< 0 (@ val length))
	      (< 0 val))))

     (defun gen-json-filter ()
       (var ff (create 
		:apt-type ($$ "#input_apt-type" (val))
		:status ($$ "#input_status" (val))
		:ix-country ($$ "#input_ix-country" (val))
		:constr ($$ "#input_counstr" (val))
		:terrace (@ (@ ($$ "#input_terrace") 0) :checked)
		:garden (@ (@ ($$ "#input_garden") 0) :checked)
		:building-permit (@ (@ ($$ "#input_building-permit") 0) 
                                    :checked)
		:summons ($$ "#input_summons" (val))
		:preemption ($$ "#input_preemption" (val))
		:subdiv-permit ($$ "#input_subdiv-permit" (val))
		))
       (if (inp-pos-val "#input_total-min")
	   (setf (@ ff :total-min) ($$ "#input_total-min" (val))))
       (if (inp-pos-val "#input_total-max")
	   (setf (@ ff :total-max) ($$ "#input_total-max" (val))))
       (if (inp-pos-val "#input_price-min") 
	   (setf (@ ff :price-min) ($$ "#input_price-min" (val))))
       (if (inp-pos-val "#input_price-max")
	   (setf (@ ff :price-max) ($$ "#input_price-max" (val))))
       (if (inp-pos-val "#input_bedrooms-min")
	   (setf (@ ff :bedrooms-min) ($$ "#input_bedrooms-min" (val))))
       (if (inp-pos-val "#input_bedrooms-min") 
	   (setf (@ ff :bathrooms-min) ($$ "#input_bathrooms-min" (val))))
       ff)
     
     (defun load-results (args)
       (let* ((args (or args (create)))
	      (count (or (@ args count) (* 3 (imgs-needed-per-page 
					      imgs-per-row imgs-per-col 
					      big-imgs-per-page))))
	      (offset (or (@ args offset) 0))
	      (callback (@ args callback))
	      (clear-prevs (if (not (= "undefined" 
				       (typeof (@ args clear-prevs))))
			       (@ args clear-prevs)
			       true)))
	 ($$ "#fp-preloader" (show))
	 ($.ajax
	  (create 
	   url "/filter" type :post data-type :json
	   data (create :preds (-j-s-o-n.stringify 
				(gen-json-filter))
			:short "t" 
			:count count
			:offset offset)
	   success 
	   (lambda (data)
	     ($$ "#fp-preloader" (hide))
	     ;;store received estates in es
	     (var es (new (-array)))
	     (for-in (raw-e data)
		     (chain es (push (eval (+ "(" (aref data raw-e) ")")))))
	     ;;clear existing estates
	     (if clear-prevs 
		 ($$ "#fp-pics-table-tr > td" (remove)))
	     ;;add received estates to document
	     (var tbl-def 
		  (+ "<td align='left' valign='top'>" 
		     "<table border='0' cellspacing='0' cellpadding='0'><tr>"))
	     (var tbl tbl-def)
	     (let ((img-per-row 9) (img-per-col 4)
		   (row-offset 0) (col-offset 0)
		   (this-row-skip-count 0)
		   (next-row-skip-count 0))
	       (for-in 
		(ie es)
		(when
		    (and (@ (aref es ie) ix-estate) 
			 (@ (aref es ie) main-pic))
		  (var this-pic-4x-p false)
		  (if (and (== col-offset 1) (== row-offset 1))
		      (setf this-pic-4x-p true))
		  (when this-pic-4x-p 
		    (+= this-row-skip-count 1)
		    ;;prepare a count of how many images to skip on next row
		    (+= next-row-skip-count 2))
		  (var td-4x-spec (if this-pic-4x-p
				      " class='td-4x' colspan='2' rowspan='2' "
				      ""))
		  (let ((e (aref es ie)))
		    (var e-gen (+ "<td align='left' valign='top' " 
				  td-4x-spec ">" 
				  "<a href='#estate-" (@ e ix-estate) "' " 
				  " class='fp-estate-link'" 
				  " ixestate=" (@ e ix-estate) ">"
				  "<img src='" (@ (@ e main-pic) path)  
				  "' /></a></td>"))
		    (+= tbl e-gen))
		  (+= col-offset 1)
		  ;;start new tr
		  (when (>= (+ col-offset this-row-skip-count) img-per-row) 
		    (+= tbl "</tr>")
		    (setf col-offset 0)
		    (+= row-offset 1)
		    ;;here, we are starting a new row
		    ;;on this row, skip images 2x count of this row's 4x images
		    (setf this-row-skip-count next-row-skip-count)
		    ;;ain't no big imgs on new row yet,so there's nothing to skip
		    (setf next-row-skip-count 0)
		    ;;start new table
		    (when (>= row-offset img-per-col)
		      (setf row-offset 0)
		      (+= tbl (+ "</table></div>" tbl-def)))
		    (+= tbl "<tr>"))
		  )))
	     (+= tbl "</tr></table></td>")
	     ($$ "#fp-pics-table-tr" (append tbl))
	     (if (not (= "undefined" (typeof callback)))
		 (chain callback (call))))
	   ))))

     (var timeout-on-change 0)
     ($$ "#search-bar select"
	 (change (lambda () 
		   (load-results))))

     ($$ "#search-bar input"
	 (keydown (lambda ()
		    ;;when the user types filters, update results
		    ;;but update according only to last change
		    (clear-timeout timeout-on-change)
		    (setf timeout-on-change 
			  (set-timeout 
			   (lambda ()
			     (load-results))
			   2200)))))
     ($$ "body" (keydown
		 (lambda (evt)
		   (if (== 27 evt.key-code) (hide-estate-div))
		   t)))
     ($$ "#top-reg-link" (fancybox))
     ($$ "#top-reg-broker-link" (fancybox))
     ($$ "#top-contact-link" (fancybox))
     ($$ document (ready (lambda ()
			   (load-results))))
     (let ((arg-estate-id (estate-id-from-argument document.location.href)))
       (if (> arg-estate-id 0)
	   (view-e arg-estate-id))))))