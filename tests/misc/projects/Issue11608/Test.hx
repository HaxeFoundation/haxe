using Test;

class Test {
	static public function main() {}

	static function doF2<T>(f: T -> Float) : T {
		var arr = [];
		arr.doF(f);
		arr.doF(f);
		return arr.doF(f);
	}

	@:generic
	static function doF<T>(array: Array<T>, f: T -> Float) : T {
		return null;
	}
}
