import rust.*;
enum Item<T> {
	value(v:T);
	none;
}
class Test<T> implements TestInterface<Item<T>> {
	var value:Item<T>;
	public function new(val:Item<T>) {
		this.value = val;
	}
	public function get():Item<T> {
		return value;
	}
	static function main() {
		new SubTest();
		Sys.println(Std.string(898687));
		triangular(20);
		reltest();
	}
	@:functionCode('return Some(@((io::stdout() as io::ReaderUtil).readLine()));')
	static function reltest():Null<String> {
		return "";
	}
	static function triangular(n:Int) {
		for(i in 1...n)
			n += i;
		return n;
	}
}
class SubTest extends Test<Int> {
	public function new() {
		super(Item.value(87));
	}
}
interface TestInterface<T> {
	public function get():T;
}