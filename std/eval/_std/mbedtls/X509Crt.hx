package mbedtls;

extern class X509Crt {
	function new():Void;

	function parse_file(path:String):Int;
	function parse_path(path:String):Int;
}
