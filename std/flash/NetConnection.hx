package flash;

extern class NetConnection
#if flash_strict
#else true
implements Dynamic
#end
{
	var isConnected : Bool;
	var uri : String;

	function new() : Void;
	function connect( targetURI : String, p1 : Dynamic, p2 : Dynamic, p3 : Dynamic, p4 : Dynamic, p5 : Dynamic, p6 : Dynamic ) : Bool;
	function call( remoteMethod : String, resultObject : Dynamic, p1 : Dynamic, p2 : Dynamic, p3 : Dynamic, p4 : Dynamic, p5 : Dynamic, p6 : Dynamic) : Void;
	function addHeader( header : String, mustUnderstand : Bool, object : Dynamic ) : Void;
	function close() : Void;

	// events
	function onStatus(infoObject : Dynamic) : Void;
	function onResult(infoObject : Dynamic) : Void;

}
