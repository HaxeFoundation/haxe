extern class Sound
{
	var duration:Float;
	var id3:Dynamic;
	var position:Float;

	function new(target:Dynamic) : Void;

	function onLoad(success:Bool):Void;
	function onSoundComplete():Void;
	function onID3():Void;

	function getPan():Float;
	function getTransform():Dynamic;
	function getVolume():Float;
	function setPan(value:Float):Void;
	function setTransform(transformObject:Dynamic):Void;
	function setVolume(value:Float):Void;
	function stop(linkageID:String):Void;
	function attachSound(id:String):Void;
	function start(secondOffset:Float, loops:Float):Void;
	function getDuration():Float;
	function setDuration(value:Float):Void;
	function getPosition():Float;
	function setPosition(value:Float):Void;
	function loadSound(url:String, isStreaming:Bool):Void;
	function getBytesLoaded():Float;
	function getBytesTotal():Float;
}


