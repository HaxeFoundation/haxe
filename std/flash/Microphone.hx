package flash;

extern class Microphone
{

	static var names(default,null) : Array<String>;
	static function get(index:Int) : Microphone;

	var activityLevel(default,null) : Float;
	var gain(default,null) : Float;
	var index(default,null) : Int;
	var muted(default,null) : Bool;
	var name(default,null) : String;
	var silenceLevel(default,null) : Int;
	var silenceTimeOut(default,null) : Float;
	var rate(default,null) : Float;
	var useEchoSuppression(default,null) : Bool;

	function setSilenceLevel(silenceLevel:Int,timeOut:Float):Void;
	function setRate(rate:Float):Void;
	function setGain(gain:Float):Void;
	function setUseEchoSuppression(useEchoSuppression:Bool):Void;

	function onActivity(active:Bool):Void;
	function onStatus(infoObject:Dynamic):Void;
}



