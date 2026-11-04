package unit.teststd.haxe.atomic;

private class Box {
	public final value:Int;

	public function new(value:Int) {
		this.value = value;
	}
}

class TestAtomicObject extends unit.Test {
	public function testFieldAccessOnResult() {
		final a = new haxe.atomic.AtomicObject(new Box(1));
		eq(a.load().value, 1);
		eq(a.store(new Box(2)).value, 2);
		eq(a.exchange(new Box(3)).value, 2);
		eq(a.compareExchange(a.load(), new Box(4)).value, 3);
		eq(a.load().value, 4);
	}

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
