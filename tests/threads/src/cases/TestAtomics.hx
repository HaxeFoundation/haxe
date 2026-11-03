package cases;


#if target.threaded
import haxe.atomic.AtomicInt;
#end

@:timeout(2000)
class TestAtomics extends ThreadTestBase {
	#if target.threaded
	function test(async:Async) {
		var a = new AtomicInt(5);
		final thread = Thread.create(() -> {
			while(a.compareExchange(0, 2) != 0) { gcSafePoint(); }
		});
		isTrue(a.compareExchange(5, 0) == 5);

		while (a.compareExchange(2, 2) != 2) { gcSafePoint(); }

		async.done();
	}

	// hl/cpp GC must stop all threads: a non-allocating spin loop would never stop and deadlock it.
	static inline function gcSafePoint() {
		#if hl
		hl.Gc.safepoint();
		#elseif cpp
		cpp.vm.Gc.safePoint();
		#end
	}
	#end
}
