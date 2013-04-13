@:nativeGen class Std {
	@:functionCode('return i32.from_str(v);')
	public static function parseInt(x:String):Int {
		return 0;
	}
	@:functionCode('return f32.from_str(v);')
	public static function parseFloat(x:String):Float {
		return 0;
	}
	public static function int(x:Float):Int {
		return cast x;
	}
	public static function is(v:Dynamic, t:Dynamic):Bool {
		return false;
	}
	@:functionCode('return s.to_str();')
	public static function string(s:Dynamic):String {
		return null;
	}
	public static function random(x:Int):Int {
		return Std.int(Math.random()*x);
	}
}