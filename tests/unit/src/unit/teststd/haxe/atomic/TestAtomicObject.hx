package unit.teststd.haxe.atomic;

class TestAtomicObject extends unit.Test {
	public function test() {
		var a = new haxe.atomic.AtomicObject("Hey World!");
		eq(a.load(), "Hey World!");
		eq(a.store("Hello World!"), "Hello World!");
		eq(a.load(), "Hello World!");
		eq(a.compareExchange("Hello World!", "Goodbye World!"), "Hello World!");
		eq(a.load(), "Goodbye World!");
		eq(a.exchange("Hello World!"), "Goodbye World!");
		eq(a.load(), "Hello World!");

	}
}
