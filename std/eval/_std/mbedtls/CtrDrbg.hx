package mbedtls;

import haxe.io.Bytes;

extern class CtrDrbg {
	function new():Void;

	function random(output:Bytes, output_len:Int):Int;
	function seed(entropy:Entropy, ?custom:String):Int;
}
