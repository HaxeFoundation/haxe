package cases;

import SignatureHelp;

class Issue5767 extends DisplayTestCase {
	/**
	class Main {
		static function doStuff(options:{a:Float, b:String}) {}

		static function main () {
			doStuff({ a: 0.5, {-1-}
		}
	}
	**/
	function testGama11() {
		eq(true, hasField(fields(pos(1)), "b", "String"));
	}

	/**
	class Main {
		static function doStuff(options:{a:Float, b:String}) {}

		static function main () {
			doStuff({ a: 0.5, {-1-}});
		}
	}
	**/
	function testGama11Intact() {
		eq(true, hasField(fields(pos(1)), "b", "String"));
	}

	/**
	typedef T = {
		a:String,
		b:Int,
		c:Bool
	}

	class Main {
		static function main() {
			var c:T = {a:"foo", {-1-}
		}
	}
	**/
	function testOrder() {
		var fields = fields(pos(1));
		eq(true, hasField(fields, "b", "Int"));
		eq(true, hasField(fields, "c", "Bool"));
	}

	/**
	typedef T1 = {
		a:String,
		b:T2
	}

	typedef T2 = {
		a:Bool,
		b:Int
	}

	class Main {
		static function main() {
			var c:T1 = {a:"foo", b: {b:1, {-1-}
		}
	}
	**/
	function testNested() {
		eq(true, hasField(fields(pos(1)), "a", "Bool"));
	}

	/**
	typedef T1<T> = {
		a:String,
		b:T2<T>
	}

	typedef T2<T> = {
		a:T,
		b:Int
	}

	class Main {
		static function main() {
			var c:T1<Bool> = { b: { b:1, {-1-}
		}
	}
	**/
	function testFirstArg() {
		eq(true, hasField(fields(pos(1)), "a", "Bool"));
	}

	/**
	typedef T1<T> = {
		a:String,
		b:String,
		c:String
	}

	class Main {
		static function main() {
			var c:T1 = { a: "foo",{-1-} b: "bar" };
		}
	}
	**/
	function testIntact() {
		eq(true, hasField(fields(pos(1)), "c", "String"));
	}
}