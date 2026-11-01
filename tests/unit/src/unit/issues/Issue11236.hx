package unit.issues;

#if jvm
import haxe.Int64;
import java.lang.Runnable;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

private final exec = Executors.newSingleThreadScheduledExecutor();
private var called = false;

private function schedule(f:() -> Void)
	exec.schedule(f, 0, TimeUnit.MILLISECONDS);

private function greeter():Void {
	called = true;
	exec.shutdown();
}
#end

#if target.threaded
private function fun()
	return 0;
#end

class Issue11236 extends Test {
	#if jvm
	function test() {
		schedule(greeter);

		t(exec.awaitTermination(Int64.fromInt(1), TimeUnit.SECONDS));
		t(called);
	}
	#end

	#if target.threaded
	function testValueReturningThreadFunctions() {
		var lock = new sys.thread.Lock();
		// the last statement of the block returns a value
		sys.thread.Thread.create(() -> {
			lock.release();
			fun();
		});
		t(lock.wait(2.0));
		var lock2 = new sys.thread.Lock();
		sys.thread.Thread.create(() -> {
			fun();
			lock2.release();
		});
		t(lock2.wait(2.0));
		// the function itself returns a value
		var count = 0;
		var lock3 = new sys.thread.Lock();
		var f = function():Int {
			count++;
			lock3.release();
			return count;
		}
		sys.thread.Thread.create(f);
		t(lock3.wait(2.0));
		eq(1, count);
	}
	#end
}
