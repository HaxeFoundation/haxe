package unit.issues;

private class Hoge {
	public var x:Int;

	public function new(x:Int) {
		this.x = x;
	}
}

class Issue9484 extends Test {
	function test() {
		var a = new Hoge(5);
		var serializedA = haxe.Serializer.run(a);
		var unserialized:Hoge = haxe.Unserializer.run(serializedA);
		eq(unserialized.x, 5);
		var serializedB = haxe.Serializer.run(unserialized);
		eq(serializedA, serializedB);
	}
}
