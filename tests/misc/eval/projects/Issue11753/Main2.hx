class Foo {
	public function new() {}

	public function test() {}

	public function doWithBar(?bar:Bar) {
		trace(bar);
	}
}

@:keep
class Bar {
	public function new() {}
}

function doThingsImpl(foo) {
	$type(foo); // Unknown<0>
	foo.doWithBar();
	$type(foo); // Unknown<0> : { doWithBar : () -> Unknown<1> }
	$type(foo.doWithBar); // () -> Unknown<0>
	if (foo != null)
		trace(foo);
	$type(foo); // Null<{ doWithBar : () -> Unknown<0> }>
	$type(foo.doWithBar); // () -> Unknown<0>
	foo.test(); // Null<{ doWithBar : () -> Unknown<0> }> has no field test
}

function main() {}
