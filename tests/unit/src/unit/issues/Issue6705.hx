package unit.issues;

class Issue6705 extends unit.Test {

	function memberFunction() { }
	static function staticFunction() { }

	function test() {
		function localFunction() { }
		var localClosure = localFunction;
		var memberClosure = memberFunction;
		var staticClosure = staticFunction;

		function alias<T>(t:T) return t;

		t(localFunction == alias(localFunction));
		t(memberFunction != alias(memberFunction));
		t(staticFunction == alias(staticFunction));
		t(localClosure == alias(localClosure));
		t(memberClosure == alias(memberClosure));
		t(staticClosure == alias(staticClosure));

		t(Reflect.compareMethods(localFunction, alias(localFunction)));
		t(Reflect.compareMethods(memberFunction, alias(memberFunction)));
		t(Reflect.compareMethods(staticFunction, alias(staticFunction)));
		t(Reflect.compareMethods(localClosure, alias(localClosure)));
		t(Reflect.compareMethods(memberClosure, alias(memberClosure)));
		t(Reflect.compareMethods(staticClosure, alias(staticClosure)));
	}
}