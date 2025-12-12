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

class Issue12444 extends Test {
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
