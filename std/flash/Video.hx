package flash;

extern class Video
{
	var deblocking : Float;
	var height : Float;
	var smoothing : Bool;
	var width : Float;

	function attachVideo( source : Dynamic ) : Void;
	function clear() : Void;

	private static function __init__() : Void untyped {
 		flash.Video = _global["Video"];
	}

}
