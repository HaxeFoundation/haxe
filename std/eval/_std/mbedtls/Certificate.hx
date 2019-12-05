package mbedtls;

extern class Certificate {
	function new():Void;

	function parse_file(path:String):Int;
	function parse_path(path:String):Int;
}
