package mbedtls;

import haxe.io.Bytes;

extern class X509Crt {
	function new():Void;

	function next():Null<X509Crt>;
	function parse(buf:Bytes):Int;
	function parse_file(path:String):Int;
	function parse_path(path:String):Int;
}
