package unit;

import haxe.Int64;

private class Helper {
	public static var counter = 0;
	// non-inline so a default referencing it is genuinely non-constant (and must
	// survive DCE)
	public static var seven = 7;

	public static function make():Int {
		counter++;
		return 40 + counter;
	}

	public static function reset() {
		counter = 0;
	}
}

class TestDefaultArgs extends Test {
	// --- Non-constant defaults of object/string types (work on every target) ---
	static function fArr(a:Array<Int> = [1, 2, 3]):Int
		return a.length;

	static function fObj(o:{x:Int} = {x: 9}):Int
		return o.x;

	static function fStr(s:String = "a" + "b"):String
		return s;

	static function defArr(a:Array<Int> = []):Array<Int>
		return a;

	public function testNonConstReference() {
		eq(3, fArr());
		eq(0, fArr([]));
		eq(9, fObj());
		eq(5, fObj({x: 5}));
		eq("ab", fStr());
		eq("z", fStr("z"));

		// a fresh instance is created on each defaulted call (not shared)
		var a1 = defArr();
		var a2 = defArr();
		a1.push(99);
		eq(0, a2.length);
	}

	// --- haxe.Int64 defaults (the original motivation; Int64 is never a TConst) ---
	static function fI64(v:Int64 = 5):String
		return Int64.toStr(v);

	static function fI64Lit(v:Int64 = 0x7FFFFFFFFFFFFFFFi64):String
		return Int64.toStr(v);

	static function fI64Neg(v:Int64 = -5i64):String
		return Int64.toStr(v);

	public function testInt64Default() {
		eq("5", fI64());
		eq("42", fI64(42i64));
		eq("9223372036854775807", fI64Lit());
		eq("-5", fI64Neg());
	}

	// --- Non-constant defaults of basic value types. Not supported on flash,
	//     where a basic type can't hold the `null` "not-passed" sentinel. ---
	#if !flash
	static function fStatic(x:Int = Helper.seven):Int
		return x;

	static function fCall(x:Int = Helper.make()):Int
		return x;

	public function testValueTypeDefault() {
		eq(7, fStatic());
		eq(3, fStatic(3));
		// explicit falsy value must win over the default
		eq(0, fStatic(0));
	}

	public function testEvaluationTiming() {
		Helper.reset();
		// default expression is evaluated on each defaulted call...
		eq(41, fCall());
		eq(42, fCall());
		eq(2, Helper.counter);
		// ...and not evaluated at all when the argument is passed
		eq(100, fCall(100));
		eq(2, Helper.counter);
	}

	// --- A default value may reference a preceding argument ---
	static function fSibling(a:Int, b:Int = a):Int
		return a + b;

	static function fChain(a:Int = 1, b:Int = a, c:Int = b):String
		return '$a,$b,$c';

	public function testSiblingArg() {
		eq(10, fSibling(5));
		eq(8, fSibling(5, 3));
		// chained defaults are injected in argument order
		eq("1,1,1", fChain());
		eq("2,2,2", fChain(2));
		eq("2,3,3", fChain(2, 3));
		eq("2,3,4", fChain(2, 3, 4));
	}

	// --- A (non-constructor) method default may reference `this` ---
	var instanceField = 10;

	function mThis(x:Int = this.instanceField):Int
		return x;

	public function testThisInDefault() {
		instanceField = 10;
		eq(10, mThis());
		eq(3, mThis(3));
		// evaluated at call time
		instanceField = 99;
		eq(99, mThis());
	}
	#end
}
