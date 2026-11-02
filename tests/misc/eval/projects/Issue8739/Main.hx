interface IA<T> {}

class AInt implements IA<Int> {
	public function new() {}
}

@:multiType
abstract Above<T>(IA<T>) {
	public function new();

	@:from static inline function fromInt(v:Int) {
		return (v : Above<Int>);
	}

	@:to static inline function toInt(t:IA<Int>):AInt {
		return new AInt();
	}
}

class Main {
	static function main() {
		var a = new Above<Int>();
		a = 1;
	}
}