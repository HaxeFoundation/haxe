package flash;

extern class Video
{
	var deblocking : Float;
	var height : Float;
	var smoothing : Bool;
	var width : Float;

	function attachVideo( source : Dynamic ) : Void;
	function clear() : Void;

	#if flash_lite
	/** flash lite only **/
	function play():Bool;
	/** flash lite only **/
	function close():Void;
	/** flash lite only **/
	function stop():Void;
	/** flash lite only **/
	function pause():Void;
	/** flash lite only **/
	function resume():Void;
	#end

	private static function __init__() : Void untyped {
 		flash.Video = _global["Video"];
	}

}
