package unit.issues;

abstract class ExampleAbstract<T> {
	public function new(v:T) {
		this.value = v;
	}

	public var value(get, set):T;

	abstract public function get_value():T;

	abstract public function set_value(value:T):T;
}

@:generic
class ExampleGeneric<T> extends ExampleAbstract<Array<T>> {
	public function new(v:Array<T>) {
		super(v);
	}

	final wrapped:Array<{value:T}> = [];

	public function get_value():Array<T> {
		return [for (w in wrapped) w.value];
	}

	public function set_value(value:Array<T>):Array<T> {
		wrapped.resize(0);
		for (v in value) {
			wrapped.push({value: v});
		}
		return value;
	}
}

class Issue11010 extends Test {
	#if (!cppia)
	function test() {
		var test = new ExampleGeneric<Int>([1, 2, 3, 4]);
		utest.Assert.same([1, 2, 3, 4], test.value);
	}
	#end
}
