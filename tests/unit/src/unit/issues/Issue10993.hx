package unit.issues;

enum Issue10993_TestEnum {
	FOO;
}


class Issue10993 extends Test {
	#if !eval
	function testHasFieldWithEnum() {
		final foo = Issue10993_TestEnum.FOO;
		eq(false, Reflect.hasField(foo, "bar"));
	}

	function testHasFieldWithFunction() {
		final foo = () -> null;
		eq(false, Reflect.hasField(foo, "bar"));
	}
	#end
}
