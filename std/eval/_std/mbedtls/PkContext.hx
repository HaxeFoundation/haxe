package mbedtls;

import haxe.io.Bytes;

extern class PkContext {
	function new():Void;

	function parse_key(key:Bytes, ?pwd:String):Int;
	function parse_keyfile(path:String, ?password:String):Int;
	function parse_public_key(key:Bytes):Int;
	function parse_public_keyfile(path:String):Int;
}
