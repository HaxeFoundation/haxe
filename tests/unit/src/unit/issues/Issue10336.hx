package unit.issues;

private abstract JurajFunc<T>(T->Unit) {
	@:from
	public static function fromFuncReturningSameType<T>(f:T->T):JurajFunc<T> {
		throw 'irrelevant';
	}
}

private abstract KevinFunc<T>(T->Unit) from T->Unit to T->Unit {
	@:from
	public static function fromFuncReturningSameType<T>(f:T->T):KevinFunc<T> {
		throw 'irrelevant';
	}
}

private class Unit {}

private abstract Callback<T>(T->Void) from(T -> Void) {
	@:from static function ofSingle<A>(cb:A->Void):Callback<A>
		return null;

	@:from static function fromMany<A>(callbacks:Array<Callback<A>>):Callback<A>
		throw 0;
}

private abstract Foo(Dynamic) {
	public var value(get, never):Int;

	inline function get_value()
		return 1;
}

class Issue10336 extends Test {
	function testJuraj() {
		var value = 'foo';
		exc(() -> {
			var f:JurajFunc<String> = function(v:String) return value = v;
		});
	}

	function testKevin() {
		var value = 'foo';
		exc(() -> {
			var f:KevinFunc<String> = function(v:String) return value = v;
		});
	}

	function testJurajReloaded() {
		var cb:Callback<String> = foo -> trace(foo.length);
		var cb:Callback<Foo> = foo -> trace(foo.value);
		utest.Assert.pass();
	}
}
