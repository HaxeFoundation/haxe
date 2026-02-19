package unit.issues;

private function wrap<T>(a:Array<T>):Array<T> {
	return a;
}

private class Parent {
	public var value:Null<Array<Int>>;

	public function new(?a:Array<Int>) {
		value = a?.copy();
	}

	public function method(a:Array<Int>):Array<Int> {
		return a.copy();
	}
}

private class Child extends Parent {
	public function new(?a:Array<Int>) {
		super(a != null ? wrap(a) : null);
	}

	override public function method(a:Array<Int>):Array<Int> {
		return super.method(wrap(a));
	}

	// the type parameter here triggers a static cast on return
	public function forStaticCast<T>(a:Array<Int>):T {
		value = a.copy();
		return cast this;
	}
}

private enum MyEnum {
	Value(arr:Array<Int>);
}

class Issue9990 extends Test {
	function checkArray(array:Array<Int>) {
		eq(10, array[0]);
		eq(20, array[1]);
		eq(30, array[2]);
		eq(3, array.length);
	}

	function testInstanceMethodCast() {
		final child = new Child();
		final result:Child = child.forStaticCast(wrap([10, 20, 30]));
		checkArray(result.value);
	}

	function testSuperMethod() {
		final child = new Child();
		final result = child.method([10, 20, 30]);
		checkArray(result);
	}

	function extractEnumArg(e:MyEnum) {
		return switch (e) {
			case Value(a): a.copy();
		};
	}

	function testEnumConstruct() {
		final e = MyEnum.Value(wrap([10, 20, 30]));
		final result = extractEnumArg(e);
		checkArray(result);
	}

	function testSuperConstructor() {
		final child = new Child([10, 20, 30]);
		checkArray(child.value);
	}

	function extractArray(array:Array<Array<Int>>) {
		return array[0].copy();
	}

	function testArrayDeclaration() {
		final arr:Array<Array<Int>> = [wrap([10, 20, 30])];
		final result = extractArray(arr);
		checkArray(result);
	}

	// these tests passed before, but are here for good measure:

	function testMapSet() {
		final map:Map<Int, Array<Int>> = [];

		map[0] = wrap([10, 20, 30]);

		// map.get uses a type parameter so it will convert to Array<Int> anyway
		final result = map[0].copy();
		checkArray(result);
	}

	function testDynamicFieldCall() {
		final obj:{
			function method(arr:Array<Int>):Array<Int>;
		} = new Parent();
		// cppia automatically handles Array<Dynamic> -> Array<Int> conversion for dynamic methods calls anyway
		final result = obj.method(wrap([10, 20, 30]));
		checkArray(result);
	}

	function extractObject(obj:{ value: Array<Int> }) {
		return obj.value.copy();
	}

	function testObjectDeclaration() {
		final obj = {
			value: wrap([10, 20, 30])
		};
		// The generator always converts to Array<Int> on dynamic field access
		final result = extractObject(obj);
		checkArray(result);
	}
}
