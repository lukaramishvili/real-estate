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
    function CreateMarker(title, pos){
      var marker = new google.maps.Marker({
    	position: pos,
    	title:    title
      });
    }
    ");)