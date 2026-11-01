class Main {
	static function main() {
		var comp = new AComponent([1, 2, 3]);
		trace(comp.doSomething());
	}
}

interface Component<T> {
	function doSomething():T;
}

@:forward
@:multiType
abstract AComponent<T>(Component<T>) {
	public function new(value:T);

	@:to public static inline function toInt(t:Component<Int>, value:Int):IntComponent {
		return new IntComponent(value);
	}

	@:to public static inline function toIntArray(t:Component<Array<Int>>, value:Array<Int>):ArrayComponent<Int> {
		return new ArrayComponent(value);
	}
}

@:generic
@:remove
class ArrayComponent<T> implements Component<Array<T>> {
	final value:Array<T>;

	public function new(value:Array<T>) {
		this.value = value;
		var x = [];
		for (i in 0...value.length) {
			var y = new AComponent(this.value[i]).doSomething();
			x.push(y);
		}
	}

	public function doSomething():Array<T> {
		return this.value;
	}
}

class IntComponent implements Component<Int> {
	final value:Int;

	public function new(value:Int) {
		this.value = value;
	}

	public function doSomething():Int {
		return value;
	}
}
