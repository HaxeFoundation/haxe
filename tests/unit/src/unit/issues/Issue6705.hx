package unit.issues;

class Issue6705 extends unit.Test {

	function memberFunction() { }
	static function staticFunction() { }

	function memberFunction1(i:Int) { }
	static function staticFunction1(i:Int) { }

	static function alias<T>(t:T) return t;

	function test() {
		function localFunction() { }

		var localClosure = localFunction;
		var memberClosure = memberFunction;
		var staticClosure = staticFunction;

		t(localFunction == alias(localFunction));
		t(localFunction == alias(localClosure));
		t(memberFunction == alias(memberFunction));
		t(memberFunction == alias(memberClosure));
		t(staticFunction == alias(staticFunction));
		t(staticFunction == alias(staticClosure));
		t(localClosure == alias(localClosure));
		t(memberClosure == alias(memberClosure));
		t(staticClosure == alias(staticClosure));

		t(Reflect.compareMethods(localFunction, alias(localFunction)));
		t(Reflect.compareMethods(localFunction, alias(localClosure)));
		t(Reflect.compareMethods(memberFunction, alias(memberFunction)));
		t(Reflect.compareMethods(memberFunction, alias(memberClosure)));
		t(Reflect.compareMethods(staticFunction, alias(staticFunction)));
		t(Reflect.compareMethods(staticFunction, alias(staticClosure)));
		t(Reflect.compareMethods(localClosure, alias(localClosure)));
		t(Reflect.compareMethods(memberClosure, alias(memberClosure)));
		t(Reflect.compareMethods(staticClosure, alias(staticClosure)));
	}

	function test1() {
		function localFunction1() { }

		var localClosure1 = localFunction1;
		var memberClosure1 = memberFunction1;
		var staticClosure1 = staticFunction1;

		t(localFunction1 == alias(localFunction1));
		t(localFunction1 == alias(localClosure1));
		t(memberFunction1 == alias(memberFunction1));
		t(memberFunction1 == alias(memberClosure1));
		t(staticFunction1 == alias(staticFunction1));
		t(staticFunction1 == alias(staticClosure1));
		t(localClosure1 == alias(localClosure1));
		t(memberClosure1 == alias(memberClosure1));
		t(staticClosure1 == alias(staticClosure1));

		t(Reflect.compareMethods(localFunction1, alias(localFunction1)));
		t(Reflect.compareMethods(localFunction1, alias(localClosure1)));
		t(Reflect.compareMethods(memberFunction1, alias(memberFunction1)));
		t(Reflect.compareMethods(memberFunction1, alias(memberClosure1)));
		t(Reflect.compareMethods(staticFunction1, alias(staticFunction1)));
		t(Reflect.compareMethods(staticFunction1, alias(staticClosure1)));
		t(Reflect.compareMethods(localClosure1, alias(localClosure1)));
		t(Reflect.compareMethods(memberClosure1, alias(memberClosure1)));
		t(Reflect.compareMethods(staticClosure1, alias(staticClosure1)));
	}

	function testTypeChange() {
		function f1(x:Float) { }
		var f2:Int -> Void = f1;
		t(f1 == f2);
	}
}