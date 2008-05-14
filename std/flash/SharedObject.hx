package flash;

extern class SharedObject
{
	#if flash8
	static function getLocal(name:String,?localPath:String,?secure:Bool):SharedObject;
	#else true
	static function getLocal(name:String,?localPath:String):SharedObject;
	#end

	static function getRemote(name:String,remotePath:String,?persistence:Dynamic):SharedObject;
	static function deleteAll(url:String) : Void;
	static function getDiskUsage(url:String) : Int;

	function send( handler : String, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic ) : Void;
	function flush(?minDiskSpace:Float):Dynamic;
	function connect( cnx : NetConnection ) : Bool;
	function close():Void;
	function getSize():Float;
	function setFps(updatesPerSecond:Float):Bool;

	dynamic function onStatus(infoObject:Dynamic):Void;
	dynamic function onSync(objArray:Array<Dynamic>):Void;

	function clear() : Void;


	#if flash_lite
	/** flash lite only **/
	static function getMaxSize():Float;

	/** flash lite only **/
	static function addListener():Void;

	/** flash lite only **/
	static function removeListener(soName:String):Void;
	#end

	var data:Dynamic;

	private static function __init__() : Void untyped {
		flash.SharedObject = _global["SharedObject"];
	}

}
