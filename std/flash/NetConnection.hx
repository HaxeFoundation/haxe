package flash;

extern class NetConnection implements Dynamic
{
	var isConnected : Bool;
	var uri : String;

	function new() : Void;
	function connect( targetURI : String) : Bool;
	function call( remoteMethod : String, resultObject : Dynamic) : Void;
	function onStatus(infoObject : Dynamic) : Void;
	function onResult(infoObject : Dynamic) : Void;
	function addHeader() : Void;
	function close() : Void;
}


