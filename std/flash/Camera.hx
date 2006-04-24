package flash;

extern class Camera {

	static property names(default,null) : Array<String>;
	static function get( index : Int ) : Camera;

	property bandwidth(default,null) : Int;

	property fps(default,null) : Float;
	property currentFps(default,null) : Float;

	property width(default,null) : Int;
	property height(default,null) : Int;

	property index(default,null) : Int;

	property activityLevel(default,null) : Float;
	property motionLevel(default,null) : Float;

	property muted(default,null) : Bool;
	property name(default,null) : String;

	property quality(default,null) : Int;

	function setMode(width:Int,height:Int,fps:Float,favorArea:Bool):Void;
	function setMotionLevel(motionLevel:Float,timeOut:Float):Void;
	function setQuality(bandwidth:Int,quality:Int):Void;

	function onActivity(active:Bool):Void;
	function onStatus(infoObject:Dynamic):Void;

	// ? not documented ?

	function setKeyFrameInterval(keyFrameInterval:Float):Void;
	function setLoopback(compress:Bool):Void;

	var nativeModes:Array<Dynamic>;
	var keyFrameInterval:Float;
	var motionTimeOut:Float;
	var loopback:Bool;

}


