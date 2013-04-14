package rust.bigint;

@:runtimeValue extern abstract BigInt {
	function to_str():String;
	function to_int():Int;
	static function from_str(s:String):BigInt;
	static function from_int(v:Int):BigInt;
}