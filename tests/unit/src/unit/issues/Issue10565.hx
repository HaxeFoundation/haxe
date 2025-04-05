package unit.issues;

private abstract MapProxy<K, V>(Dynamic) {
	@:from static function fromMap<K, V>(map:Map<K, V>):MapProxy<K, V> {
		return null;
	}
}

private class Foo {}
private abstract ReactTypeOf<T:{}>(Foo) to Foo {}

@:forward
private abstract ReactContext<T>(IReactContext<T>) from IReactContext<T> to IReactContext<T> {
	public function new() {
		this = new Context();
	}

	@:to
	public function toReactType<T1:{children:T->String}>():ReactTypeOf<T1> {
		return cast this;
	}
}

private interface IReactContext<T> {
	var Consumer:ReactContext<T>;
}

private class Context<T> implements IReactContext<T> {
	public var Consumer = null;

	public function new() {}
}

private typedef Bar = {
	children:{foo:String}->String
}

class Issue10565 extends Test {
	function test() {
		var m:MapProxy<String, Int> = new Map();
		utest.Assert.pass();
	}

	public static var Consumer:ReactTypeOf<Bar>;

	function testReactMess() {
		var context = createContext();
		Consumer = context.Consumer;
		utest.Assert.pass();
	}

	public static function createContext<TContext>():ReactContext<TContext> {
		return new ReactContext();
	}
}
