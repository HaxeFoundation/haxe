package unit.issues;

class Issue6406 extends unit.Test {
	function test() {
		eq(42, fIf(1));
		eq(42, fSwitch(1));
		// eq(42, fTry(1));
	}

	inline static function fIf(i:Int):Dynamic return if (i == 0) true else 42;

	inline static function fSwitch(i:Int):Dynamic return switch (i) { case 0: true; case _: 42; }

	// not sure if this is supposed to work
	// inline static function fTry(i:Int):Dynamic return try 42 catch(e:Dynamic) true;
}