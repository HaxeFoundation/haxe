package unit.issues;

class Issue7600 extends unit.Test {
	function test() {
		var def = macro class {
			public static function foo<E:EnumValue>(e:E):E
				return e;
		}
		noAssert();
	}
}