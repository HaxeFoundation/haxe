class Main {
	static var doThings : Foo -> Void;

	static function main() {
		var foo = new Foo();
		doThings = (foo -> doThingsImpl(foo));
		doThings(foo);
	}

	static function doThingsImpl(foo) {
		foo.doWithBar();
		$type(foo);
		$type(foo.doWithBar);

		if (foo != null) trace(foo);
		$type(foo);
		$type(foo.doWithBar);
	}
}

class Foo {
	public function new() {}
	public function doWithBar(?bar:Bar) {
		trace(bar);
	}
}

@:keep
class Bar {
	public function new() {}
}
