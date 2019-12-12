package mbedtls;

import mbedtls.X509Crt;
import haxe.io.Bytes;

extern class Ssl {
	function new():Void;

	function get_peer_cert():Null<X509Crt>;
	function handshake():Int;
	function read(buf:Bytes, pos:Int, len:Int):Int;
	function set_hostname(hostname:String):Int;
	function setup(conf:Config):Int;
	function write(buf:Bytes, pos:Int, len:Int):Int;
}
