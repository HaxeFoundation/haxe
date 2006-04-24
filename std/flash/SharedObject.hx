package flash;

extern class SharedObject
{
	static function getLocal(name:String,localPath:String):SharedObject;
	static function getRemote(name:String,remotePath:String,persistence:Dynamic):SharedObject;
	static function deleteAll(url:String) : Void;
	static function getDiskUsage(url:String) : Int;

	function send(handlerName:String):Void;
	function flush(minDiskSpace:Float):Dynamic;
	function close():Void;
	function getSize():Float;
	function setFps(updatesPerSecond:Float):Bool;

	function onStatus(infoObject:Dynamic):Void;
	function onSync(objArray:Array<Dynamic>):Void;

	function clear() : Void;

	var data:Dynamic;
}
