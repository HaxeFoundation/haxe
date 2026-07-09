package mbedtls;

class Error {
	extern static public var WANT_READ:Int;
	extern static public var WANT_WRITE:Int;
	extern static public var PEER_CLOSE_NOTIFY:Int;

	extern static public function strerror(code:Int):String;
}
