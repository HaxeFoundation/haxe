class C {
	public var value:String;
	public function new() {
		value = "foo";
	}
}

abstract A(C) to C {
	public function f() {
		return "bar";
	}
}

@:generic
class G<T:haxe.Constraints.Constructible<()->Void>> {
	public function new() {}
	public function make():T return new T();
}

class Main {
	static function main() {
		var g = new G<A>();
	}
}