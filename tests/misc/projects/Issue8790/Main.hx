class Main {
	public static function main() {
		1 + new Int8(1);
	}
}

abstract Int8(Int) {

	public inline function new(value:Int) {
		this = value;
	}

	@:op(A + B) function addition(b:Int8):Int8;

	@:op(A + B) static function intAddition(a:Int8, b:Int):Int {
		return b + cast a;
	}
}