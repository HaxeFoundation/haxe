package unit.issues;

//targets with pf_supports_rest_args = true
#if (js || lua || php || cs || java || python || flash)

class Issue10143 extends Test {
	function test1() {
		noAssert();
		eq('String', HelperMacros.typeString(Win.test('hello')));
		eq('Bool', HelperMacros.typeString(Win.test({field:'world'})));
		eq('String', HelperMacros.typeString(Win.test(('hello':AString))));
		eq('Bool', HelperMacros.typeString(Win.test(({field:'world'}:AObj))));
	}
}

private extern class Win {
	overload static function test<T:{field:String}>(items:...T):Bool;
	overload static function test(items:...String):String;
}

private abstract AString(String) from String {
	@:to public function toString():String {
		return this;
	}
}

private abstract AObj({field:String}) from {field:String} {
	@:to public function toObj():{field:String} {
		return this;
	}
}

#else
class Issue10143 extends Test {}
#end