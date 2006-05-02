package flash;

extern class XMLSocket {

	function new() : Void;
	function connect( url : String, port : Int ) : Bool;
	function send( data : Dynamic ) : Bool;
	function close() : Bool;
	function onData( src : String ) : Void;
	//function onXML( src : Xml ) : Void;
	function onConnect( success : Bool ) : Void;
	function onClose() : Void;
}
