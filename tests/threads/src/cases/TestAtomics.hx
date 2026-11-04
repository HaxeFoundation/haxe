package cases;


#if target.threaded
import haxe.atomic.AtomicInt;
import haxe.atomic.AtomicObject;
#end

private enum Outcome {
	Succeeded;
	Failed(error:String);
}

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

	// Enum values are reference types on every target, so an enum with payloads should be
	// usable with AtomicObject like a class instance is.
	function testAtomicObjectEnum(async:Async) {
		final outcome = new AtomicObject<Null<Outcome>>(null);
		final done = new AtomicInt(0);
		final failed = Failed("boom");
		isTrue(outcome.compareExchange(null, failed) == null);
		Thread.create(() -> {
			// The outcome is already set, so this exchange must lose.
			outcome.compareExchange(null, Succeeded);
			done.store(1);
		});
		while (done.load() == 0) { gcSafePoint(); }
		isTrue(outcome.load() == failed);
		switch (outcome.load()) {
			case Failed("boom"):
			case other:
				fail('unexpected outcome $other');
		}
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
