import rust.*;
import rust.io.*;
enum Item<T> {
	value(v:T);
	none;
}
class Test<T> implements TestInterface<Item<T>> {
	public var value:Item<T>;
	public function new(val:Item<T>) {
		this.value = val;
	}
	public function get():Item<T> {
		return value;
	}
	static function main() {
		var b = new Array<String>();
		b.push("8989");
		var a = [56, 78, 42, 35];
		b.push(Std.string(a));
		Std.string(b.length);
		for(i in b)
			i;
		new SubTest();
	}
}
class SubTest extends Test<Int> {
	public function new() {
		super(Item.value(87));
	}
}
interface TestInterface<T> {
	public var value:T;
	public function get():T;
}