package lua;
@:keep
class Global {
	public static function hxClamp(x:Int){
		return (x & 2147483647) - (x & cast 2147483648);
	}
	public static function hxBnot(x:Int){
		return hxClamp(Bit.bnot(x));
	}
}
