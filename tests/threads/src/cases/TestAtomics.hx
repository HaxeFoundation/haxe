package cases;


#if target.atomics
import haxe.atomic.AtomicInt;
#end

@:timeout(2000)
class TestAtomics extends utest.Test {
	#if target.atomics
	function test(async:Async) {
		var a = new AtomicInt(5);
		final thread = Thread.create(() -> {
			while(a.compareExchange(0, 2) != 0) {}
		});
		isTrue(a.compareExchange(5, 0) == 5);

		while (a.compareExchange(2, 2) != 2) {}

		async.done();
	}
	#end
}
