(in-package :re)

(ql:quickload :parenscript)

(defun re-main-js ()
  ;(ps:ps
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
    /*end MapMarkerACCombo*/
    ");)