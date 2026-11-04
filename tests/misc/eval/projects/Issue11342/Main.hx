@:structInit
abstract class Foo {
	public function new(a:Int, b:Int) {}
}

function main() {
	var x:Foo = {a: 1, b: 2};
}
