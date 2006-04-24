package flash;

extern class Microphone
{

	static property names(default,null) : Array<String>;
	static function get(index:Int) : Microphone;

	property activityLevel(default,null) : Float;
	property gain(default,null) : Float;
	property index(default,null) : Int;
	property muted(default,null) : Bool;
	property name(default,null) : String;
	property silenceLevel(default,null) : Int;
	property silenceTimeOut(default,null) : Float;
	property rate(default,null) : Float;
	property useEchoSuppression(default,null) : Bool;

	function setSilenceLevel(silenceLevel:Int,timeOut:Float):Void;
	function setRate(rate:Float):Void;
	function setGain(gain:Float):Void;
	function setUseEchoSuppression(useEchoSuppression:Bool):Void;

	function onActivity(active:Bool):Void;
	function onStatus(infoObject:Dynamic):Void;
}



