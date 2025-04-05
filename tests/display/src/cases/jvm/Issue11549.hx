package cases.jvm;

class Issue11549 extends DisplayTestCase {
	/**
		import java.util.concurrent.Executors;
		import java.util.concurrent.TimeUnit;
		import java.lang.Runnable;

		final exec = Executors.newSingleThreadScheduledExecutor();

		function schedule(f:() -> Void)
			exec.schedule(f, 0, TimeUnit.MILLISECONDS);

		function greeter():Void {
			trace("hello");
			exec.shutdown();
		}

		function main() {
			schedule(greeter);
			exec.{-1-}
		}
	**/
	function test() {
		eq(true, hasField(fields(pos(1)), "isTerminated", "() -> Bool"));
	}
}
