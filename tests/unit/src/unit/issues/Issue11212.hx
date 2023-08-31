package unit.issues;

class Issue11212 extends Test {
	var value = 123;

	function test() {
		#if !macro
		var foo:Foo = value; // on JS this generates `let foo = function() { return this.value; };` on JS
		eq(123, foo());
		#end
	}
}

@:callable
private abstract Foo(() -> Int) from () -> Int {
	@:from macro static function ofExpr(e) {
		return macro @:pos(e.pos) (function():Int return $e : Foo);
	}
}
