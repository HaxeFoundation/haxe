package unit.issues;

class Issue6705 extends unit.Test {

	function memberFunction() { }
	static function staticFunction() { }

	function memberFunction1(i:Int) { }
	static function staticFunction1(i:Int) { }

	@:pure(false) static function alias<T>(t:T) return t;

	@:pure(false) static function equalsT<T>(a:T, b:T) return a == b;

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
		t(localFunction == alias(localFunction));

		t(equalsT(localFunction, localClosure));
		t(equalsT(memberFunction, memberFunction));
		t(equalsT(memberFunction, memberClosure));
		t(equalsT(staticFunction, staticFunction));
		t(equalsT(staticFunction, staticClosure));
		t(equalsT(localClosure, localClosure));
		t(equalsT(memberClosure, memberClosure));
		t(equalsT(staticClosure, staticClosure));

		t(Reflect.compareMethods(localFunction, alias(localFunction)));
		t(Reflect.compareMethods(localFunction, alias(localClosure)));
		t(Reflect.compareMethods(memberFunction, alias(memberFunction)));
		t(Reflect.compareMethods(memberFunction, alias(memberClosure)));
		t(Reflect.compareMethods(staticFunction, alias(staticFunction)));
		t(Reflect.compareMethods(staticFunction, alias(staticClosure)));
		t(Reflect.compareMethods(localClosure, alias(localClosure)));
		t(Reflect.compareMethods(memberClosure, alias(memberClosure)));
		t(Reflect.compareMethods(staticClosure, alias(staticClosure)));

		var array = [localFunction, memberFunction, staticFunction];
		eq(0, array.indexOf(localFunction));
		eq(1, array.indexOf(memberFunction));
		eq(2, array.indexOf(staticFunction));
	}

	function test1() {
		function localFunction1(i:Int) { }

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

		t(equalsT(localFunction1, localFunction1));
		t(equalsT(localFunction1, localClosure1));
		t(equalsT(memberFunction1, memberFunction1));
		t(equalsT(memberFunction1, memberClosure1));
		t(equalsT(staticFunction1, staticFunction1));
		t(equalsT(staticFunction1, staticClosure1));
		t(equalsT(localClosure1, localClosure1));
		t(equalsT(memberClosure1, memberClosure1));
		t(equalsT(staticClosure1, staticClosure1));

		t(Reflect.compareMethods(localFunction1, alias(localFunction1)));
		t(Reflect.compareMethods(localFunction1, alias(localClosure1)));
		t(Reflect.compareMethods(memberFunction1, alias(memberFunction1)));
		t(Reflect.compareMethods(memberFunction1, alias(memberClosure1)));
		t(Reflect.compareMethods(staticFunction1, alias(staticFunction1)));
		t(Reflect.compareMethods(staticFunction1, alias(staticClosure1)));
		t(Reflect.compareMethods(localClosure1, alias(localClosure1)));
		t(Reflect.compareMethods(memberClosure1, alias(memberClosure1)));
		t(Reflect.compareMethods(staticClosure1, alias(staticClosure1)));

		var array = [localFunction1, memberFunction1, staticFunction1];
		eq(0, array.indexOf(localFunction1));
		eq(1, array.indexOf(memberFunction1));
		eq(2, array.indexOf(staticFunction1));
	}
#if !hl
	function testTypeChange() {
		function f1(x:Float) { }
		var f2:Int -> Void = f1;
		t(f1 == f2);
	}
#end
}