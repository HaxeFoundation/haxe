package flash;

extern class Camera
{
	static var names:Array<String>;
	static function get(index:Int):Camera;

	var nativeModes:Array<Dynamic>;
	var keyFrameInterval:Float;
	var bandwidth:Float;
	var motionLevel:Float;
	var motionTimeOut:Float;
	var quality:Float;
	var loopback:Bool;
	var width:Float;
	var height:Float;
	var fps:Float;
	var activityLevel:Float;
	var muted:Bool;
	var currentFps:Float;
	var name:String;
	var index:Float;

	function setKeyFrameInterval(keyFrameInterval:Float):Void;
	function setLoopback(compress:Bool):Void;
	function setMode(width:Float,height:Float,fps:Float,favorArea:Bool):Void;
	function setMotionLevel(motionLevel:Float,timeOut:Float):Void;
	function setQuality(bandwidth:Float,quality:Float):Void;

	function onActivity(active:Bool):Void;
	function onStatus(infoObject:Dynamic):Void;
}


