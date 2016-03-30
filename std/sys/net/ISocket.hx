package sys.net;

interface ISocket {

	var input(default,null) : haxe.io.Input;

	var output(default, null) : haxe.io.Output;
	
	var custom : Dynamic;

	function close() : Void;

	function read() : String;

	function write( content : String ) : Void;

	function connect( host : Host, port : Int ) : Void;

	function listen( connections : Int ) : Void;

	function shutdown( read : Bool, write : Bool ) : Void;

	function bind( host : Host, port : Int ) : Void;

	function accept() : ISocket;

	function peer() : { host : Host, port : Int };

	function host() : { host : Host, port : Int };

	function setTimeout( timeout : Float ) : Void;

	function waitForRead() : Void;

	public function setBlocking( b : Bool ) : Void;

	public function setFastSend( b : Bool ) : Void;

}
