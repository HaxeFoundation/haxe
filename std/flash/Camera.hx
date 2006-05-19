package flash;

extern class Camera {

	static var names(default,null) : Array<String>;
	static function get( index : Int ) : Camera;

	var bandwidth(default,null) : Int;

	var fps(default,null) : Float;
	var currentFps(default,null) : Float;

	var width(default,null) : Int;
	var height(default,null) : Int;

	var index(default,null) : Int;

	var activityLevel(default,null) : Float;
	var motionLevel(default,null) : Float;

	var muted(default,null) : Bool;
	var name(default,null) : String;

	var quality(default,null) : Int;

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


