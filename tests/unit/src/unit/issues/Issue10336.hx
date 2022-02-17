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
}
