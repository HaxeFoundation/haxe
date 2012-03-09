package flash;

extern class XMLSocket {

	function new() : Void;
	function connect( url : String, port : Int ) : Void;
	function send( data : Dynamic ) : Bool;
	function close() : Bool;
	dynamic function onData( src : String ) : Void;
	//dynamic function onXML( src : Xml ) : Void;
	dynamic function onConnect( success : Bool ) : Void;
	dynamic function onClose() : Void;

	private static function __init__() : Void untyped {
		flash.XMLSocket = _global["XMLSocket"];
	}

}
