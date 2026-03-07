class Inl {
	public var i : Int = 0;
	public extern inline function new() {}
}

class Extern {
	public static function main() {
		var a = new Inl();
		trace(a);
	}
}