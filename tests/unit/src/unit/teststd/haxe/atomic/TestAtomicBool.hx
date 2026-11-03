package unit.teststd.haxe.atomic;

class TestAtomicBool extends unit.Test {
	public function test() {
		var a = new haxe.atomic.AtomicBool(true);

		t(a.load());
		f(a.store(false));
		f(a.load());

		f(a.compareExchange(false, true));
		t(a.load());

		t(a.compareExchange(false, false));
		t(a.load());

		t(a.exchange(true));
		t(a.load());

	}
}
