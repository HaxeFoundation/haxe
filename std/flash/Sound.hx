package flash;

extern class Sound
{
	var duration(default,null):Float;
	var id3(default,null):Dynamic;
	var position(default,null):Float;
#if flash_v9
	var checkPolicyFile : Bool;
#end

	function new( ?target:Dynamic ) : Void;

	dynamic function onLoad(success:Bool):Void;
	dynamic function onSoundComplete():Void;
	dynamic function onID3():Void;

	function getPan():Float;
	function getTransform():Dynamic;
	function getVolume():Float;
	function setPan(value:Float):Void;
	function setTransform(transformObject:Dynamic):Void;
	function setVolume(value:Float):Void;
	function stop(?linkageID:String):Void;
	function attachSound(id:String):Void;
	function start(?secondOffset:Float, ?loops:Float):Void;
	function loadSound(url:String, isStreaming:Bool):Void;
	function getBytesLoaded():Float;
	function getBytesTotal():Float;

	private static function __init__() : Void untyped {
		flash.Sound = _global["Sound"];
	}

}
