interface Foo {
	function foo():Void;
}

@:build(Macro.build())
class Main implements Foo {
	public function foo():Void {}
}

function main() {}
