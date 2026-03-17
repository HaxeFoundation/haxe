package unit.teststd.haxe.atomic;

class TestAtomicInt extends unit.Test {
	public function test() {
		var a = new haxe.atomic.AtomicInt(0);

		eq(a.load(), 0);

		eq(a.store(5), 5);
		eq(a.load(), 5);

		eq(a.add(5), 5);
		eq(a.load(), 10);

		eq(a.sub(5), 10);
		eq(a.load(), 5);

		eq(a.and(20), 5);
		eq(a.load(), 4);

		eq(a.or(3), 4);
		eq(a.load(), 7);

		eq(a.xor(2), 7);
		eq(a.load(), 5);

		eq(a.compareExchange(0, 0), 5);
		eq(a.load(), 5);
		eq(a.compareExchange(5, 0), 5);
		eq(a.load(), 0);

		eq(a.exchange(10), 0);
		eq(a.load(), 10);
	}
}
