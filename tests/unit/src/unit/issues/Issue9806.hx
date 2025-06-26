package unit.issues;

class Issue9806 extends unit.Test {
	function test() {
		eq('Two', Type.enumConstructor(One));
		eq('TheirEnum', Type.getEnumName(MyEnum));
		aeq(['Two'], Type.getEnumConstructs(MyEnum));
	}
}

@:native('TheirEnum')
private enum MyEnum {
	@:native('Two')
	One;
}