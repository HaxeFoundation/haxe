package unit;

import haxe.Int64;

private class Helper {
	public static var counter = 0;
	public static inline final CONST = 7;

	public static function make():Int {
		counter++;
		return 40 + counter;
	}

	public static function reset() {
		counter = 0;
	}
}

class TestDefaultArgs extends Test {
	// --- Non-value-type, non-constant defaults (work everywhere but cpp) ---
	#if !cpp
	static function fArr(a:Array<Int> = [1, 2, 3]):Int
		return a.length;

	static function fObj(o:{x:Int} = {x: 9}):Int
		return o.x;

	static function fStr(s:String = "a" + "b"):String
		return s;

	public function testNonConstReference() {
		// array/object/string defaults
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

	static function defArr(a:Array<Int> = []):Array<Int>
		return a;
	#end

	// --- Value-type, non-constant defaults. Not supported on cpp (can't
	//     render the default) nor flash (basic types can't hold the null
	//     "not passed" sentinel). ---
	#if (!cpp && !flash)
	static function fStatic(x:Int = Helper.CONST):Int
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
	#end

	// --- haxe.Int64 defaults (the original motivation). Int64 is a boxed
	//     value on flash, so only cpp is excluded. ---
	#if !cpp
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
	#end
}
