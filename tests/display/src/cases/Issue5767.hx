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
		sigEq(0, [["b:String"]], signature(pos(1)));
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
		sigEq(0, [["b:String"]], signature(pos(1)));
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
		sigEq(0, [["b:Int", "c:Bool"]], signature(pos(1)));
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
		sigEq(0, [["a:Bool"]], signature(pos(1)));
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
		sigEq(0, [["a:Bool"]], signature(pos(1)));
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
		sigEq(0, [["c:String"]], signature(pos(1)));
	}

	function sigEq(arg:Int, params:Array<Array<String>>, sig:SignatureHelp, ?pos:haxe.PosInfos) {
		eq(arg, sig.activeParameter, pos);
		eq(params.length, sig.signatures.length, pos);
		for (i in 0...params.length) {
			var sigInf = sig.signatures[i];
			var args = params[i];
			eq(sigInf.parameters.length, args.length, pos);
			for (i in 0...args.length) {
				eq(sigInf.parameters[i].label, args[i], pos);
			}
		}
	}
}