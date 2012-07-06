(in-package :re)

(ql:quickload :parenscript)

(defun re-main-js ()
  (ps:ps
    "
    //this code is needed for google maps
  /*  function initialize() {
    	var initPos = new google.maps.LatLng(41.5, 44.8);
        var myOptions = {
          center: initPos,
          zoom: 8,
          mapTypeId: google.maps.MapTypeId.ROADMAP
        };
        var map = new google.maps.Map(document.getElementById('map_canvas'),
            myOptions);
        //lukas code 
	      var marker = new google.maps.Marker({
	    	    position: initPos,
	    	    title:'Hello World!'
	    	});
	
	    	// To add the marker to the map, call setMap(); 
	    	marker.setMap(map);
        //end lukas code 
      }
  */
      "))