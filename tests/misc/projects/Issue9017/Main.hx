class Main {
	static function main() {
		var f:Foo = { field:1 };
		trace(f.field);
	}
}

@:structInit
interface Foo {
	var field:Int;
}