package unit.issues;

@:notNull
@:fromNull
private abstract NotInt64(Int) {
	@:op(A == B) public static function eqNoNo(a:NotInt64, b:NotInt64):String {
		return "eqNoNo";
	}

	@:op(A == B) public static function eqYesNo(a:Null<NotInt64>, b:NotInt64):String {
		return "eqYesNo";
	}

	@:op(A == B) public static function eqNoYes(a:NotInt64, b:Null<NotInt64>):String {
		return "eqNoYes";
	}

	@:op(A == B) public static function eqYesYes(a:Null<NotInt64>, b:Null<NotInt64>):String {
		return "eqYesYes";
	}

	@:op(A != B) public static function neqNoNo(a:NotInt64, b:NotInt64):String {
		return "neqNoNo";
	}

	@:op(A != B) public static function neqYesNo(a:Null<NotInt64>, b:NotInt64):String {
		return "neqYesNo";
	}

	@:op(A != B) public static function neqNoYes(a:NotInt64, b:Null<NotInt64>):String {
		return "neqNoYes";
	}

	@:op(A != B) public static function neqYesYes(a:Null<NotInt64>, b:Null<NotInt64>):String {
		return "neqYesYes";
	}
}

private abstract LastDigit(Int) from Int {
	@:op(A == B) public static function eq(a:LastDigit, b:LastDigit):Bool {
		Issue12444.calls++;
		return (cast a : Int) % 10 == (cast b : Int) % 10;
	}

	@:op(A != B) public static function neq(a:LastDigit, b:LastDigit):Bool {
		Issue12444.calls++;
		return (cast a : Int) % 10 != (cast b : Int) % 10;
	}
}

class Issue12444 extends Test {
	public static var calls = 0;

	static var order = "";

	static function get(name:String, v:Null<LastDigit>):Null<LastDigit> {
		order += name;
		return v;
	}

	function testLifted() {
		var v:LastDigit = 1;
		var nv:Null<LastDigit> = 11;
		var nn:Null<LastDigit> = null;
		calls = 0;
		// non-null operands go through the overload
		t(v == nv);
		t(nv == v);
		f(v != nv);
		t(nv == nv);
		eq(4, calls);
		// a null operand compares by nullness without calling the overload
		f(v == nn);
		f(nn == v);
		t(nn == nn);
		t(v != nn);
		f(nn != nn);
		f(nv == nn);
		eq(4, calls);
		// each operand is evaluated once, left to right
		order = "";
		t(get("L", 1) == get("R", 21));
		eq("LR", order);
	}

	function testLiftedInt64() {
		var a:Null<haxe.Int64> = haxe.Int64.make(1, 5);
		var b:Null<haxe.Int64> = haxe.Int64.make(1, 5);
		var n:Null<haxe.Int64> = null;
		t(a == b);
		f(a != b);
		f(a == n);
		f(n == a);
		t(n == n);
		t(a != n);
	}

	function test() {
		var nullable:Null<NotInt64> = null;
		var notNullable:NotInt64 = null;
		var nullableInt:Null<Int> = null;
		var notNullableInt:Int = 0;
		// ==
		eq("eqYesYes", nullable == nullable);
		eq("eqYesNo", nullable == notNullable);
		eq("eqNoYes", notNullable == nullable);
		eq("eqNoNo", notNullable == notNullable);
		// !=
		eq("neqYesYes", nullable != nullable);
		eq("neqYesNo", nullable != notNullable);
		eq("neqNoYes", notNullable != nullable);
		eq("neqNoNo", notNullable != notNullable);
	}
}
