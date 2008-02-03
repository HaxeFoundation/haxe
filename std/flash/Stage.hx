package flash;

extern class Stage
{
	static var width:Float;
	static var height:Float;
	static var scaleMode:String;
	static var align:String;
	static var showMenu:Bool;
	static function addListener(listener:Dynamic):Void;
	static function removeListener(listener:Dynamic):Void;

#if flash_v9
	/**
		Set to "fullScreen" in order to set Flash to fullscreen
		(can only be done in Mouse/Keyboard event listener).
		Don't forget allowfullscreen="true" in your SWF parameters.
	**/
	static var displayState : String;
	static var fullScreenSourceRect : flash.geom.Rectangle<Int>;
	static function onFullScreen( full : Bool ) : Void;
#end

	private static function __init__() : Void untyped {
		flash.Stage = _global["Stage"];
	}

}
