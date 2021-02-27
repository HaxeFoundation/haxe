package unit.issues;

//targets with pf_supports_rest_args = true
#if (js || lua || php || cs || java || python || flash)

class Issue10143 extends Test {
	function test1() {
		noAssert();
		eq('String', HelperMacros.typeString(Win.test('hello')));
		eq('Bool', HelperMacros.typeString(Win.test({field:'world'})));
	}
}

private extern class Win {
	overload static function test<T:{field:String}>(items:...T):Bool;
	overload static function test(items:...String):String;
}

#else
class Issue10143 extends Test {}
#end