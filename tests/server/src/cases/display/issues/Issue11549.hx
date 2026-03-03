package cases.display.issues;

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
	function test(_) {
		var args = ["--jvm", "no.jar"];
		runHaxe(args);
		runHaxeJson(args, DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		eq(true, hasField(parseCompletion().result.items, "isTerminated", "() -> Bool"));
	}
}
