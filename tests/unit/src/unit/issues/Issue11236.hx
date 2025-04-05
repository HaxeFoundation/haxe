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

class Issue11236 extends Test {
	#if jvm
	function test() {
		schedule(greeter);

		t(exec.awaitTermination(Int64.ofInt(1), TimeUnit.SECONDS));
		t(called);
	}
	#end
}
