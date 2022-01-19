package unit.issues;

@:structInit @:publicFields
private class Tuple2<T, U> {
	final _1:T;
	final _2:U;

	inline function new(_1:T, _2:U) {
		this._1 = _1;
		this._2 = _2;
	}
}

@:generic
@:structInit @:publicFields
private class Tuple2Generic<T, U> {
	final _1:T;
	final _2:U;

	inline function new(_1:T, _2:U) {
		this._1 = _1;
		this._2 = _2;
	}
}

class Issue10550 extends Test {
	function test() {
		final foo:Tuple2<Int, String> = {_1: 1, _2: "abc"}; // works
		final bar:Tuple2Generic<Int, String> = {_1: 1, _2: "abc"}; // doesn't work
		utest.Assert.pass();
	}
}
