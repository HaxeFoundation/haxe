extern class Microphone
{
	static var names:Array<Dynamic>;
	static function get(index:Float):Microphone;

	var gain:Float;
	var index:Float;
	var activityLevel:Float;
	var name:String;
	var silenceLevel:Float;
	var silenceTimeOut:Float;
	var rate:Float;
	var useEchoSuppression:Bool;
	var muted:Bool;
	
	function setSilenceLevel(silenceLevel:Float,timeOut:Float):Void;
	function setRate(rate:Float):Void;
	function setGain(gain:Float):Void;
	function setUseEchoSuppression(useEchoSuppression:Bool):Void;
	
	function onActivity(active:Bool):Void;
	function onStatus(infoObject:Dynamic):Void;
}



