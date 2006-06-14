package flash;

extern class XMLSocket {

	function new() : Void;
	function connect( url : String, port : Int ) : Void;
	function send( data : Dynamic ) : Bool;
	function close() : Bool;
	function onData( src : String ) : Void;
	//function onXML( src : Xml ) : Void;
	function onConnect( success : Bool ) : Void;
	function onClose() : Void;

	private static function __init__() : Void untyped {
		flash.XMLSocket = _global["XMLSocket"];
	}

}
