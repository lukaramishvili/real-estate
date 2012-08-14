(in-package :re)

(ql:quickload :parenscript)

(defpsmacro $$ (selector &body chains)
  `(chain (j-query ,selector)
      ,@chains))

(defpsmacro += (var &rest what-to-append)
      `(setf ,var (+ ,var ,@what-to-append)))

(defun re-main-js ()
  (+s 
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
	 (+= div "<a id='estate-toggle-fav'>Favorite</a>")
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

     (defun show-estate-div (estate-div)
       ($$ "#view-estate" (html estate-div)))
     
     (defun view-e (id)
       (get-estate 
	id (lambda (e)
	     (if (!= e null)
		 (progn 
		   (show-estate-div (gen-estate-div e))
		   ($$ "#estate-main-img-a,#other-imgs > a" (fancybox))
					;($$ "" (fancybox))
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
		 (alert "Loading estate failed, please try again."))
	     e))
       (return false))
     ($$ ".fp-estate-link"
	 (live "click" (lambda () 
			 (view-e ($$ this (attr "ixestate")))
			 false))
	 (fancybox))
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
    "))

(defun fp-search-js ()
  (+s 
   "
  var fSearchOpen = false;
  $('#btn-toggle-search').click(function(){
       $('#search-bar').animate({ 'left' : fSearchOpen ? -272 : 0 }, 'slow');
       //$('#main').animate({ 'padding-left' : fSearchOpen ? 0 : 250 }, 'slow');
       fSearchOpen = !fSearchOpen;
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
     (defun load-results ()
       ($.ajax
	(create 
	 url "/filter" type :post data-type :json
	 data (create :preds (-j-s-o-n.stringify 
			       (gen-json-filter)))
	 success (lambda (data) (console.log data)))
	))

     (var timeout-on-change 0)
     ($$ "#search-bar select"
	 (change (lambda () (load-results))))
     ($$ "#search-bar input"
	 (keydown (lambda ()
		    ;;when the user types filters, update results
		    ;;but update according only to last change
		    (clear-timeout timeout-on-change)
		    (setf timeout-on-change 
			  (set-timeout (lambda ()
					 (load-results))
				       2200)))))
     )))