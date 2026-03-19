private class Child extends Lib2 {
	public function new() {
		super();

		// protected field/property/method access
		var _x:Int = x;
		x = 50;
		var _i:Int = i;
		i = 10;
		var _f:String = f();
		var _f2:String = call(f);

		// static protected access
		var _sx:Int = Lib.sx;
		Lib.sx = 50;
		var _si:Int = Lib.si;
		Lib.si = 10;
		var _sf:String = Lib.sf();
	}

	static function call(f:()->String):String {
		return f();
	}
}

class Main {
	static function main() {
		new Child();
	}
}
