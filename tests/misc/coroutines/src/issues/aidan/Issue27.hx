package issues.aidan;

import haxe.ValueException;
import haxe.coro.schedulers.Scheduler;
import haxe.coro.schedulers.VirtualTimeScheduler;
import haxe.coro.context.Key;
import haxe.coro.context.IElement;
import hxcoro.task.ICoroTask;

class DebugName implements IElement<DebugName> {
	static public final key = new Key<DebugName>("DebugName");

	public var name:String;

	public function new(name:String) {
		this.name = name;
	}

	public function getKey() {
		return key;
	}

	public function toString() {
		return '[DebugName: $name]';
	}
}

class Issue27 extends utest.Test {
	@:coroutine
	function logDebug() {
		return suspend(cont -> {
			cont.resume(cont.context.get(DebugName).name, null);
		});
	}

	@:coroutine
	function modifyDebug(name:String) {
		suspend(cont -> {
			cont.context.get(DebugName).name = name;
			cont.resume(null, null);
		});
	}

	function test() {
		CoroRun.runScoped(scope ->  {
			scope.with(new DebugName("first name")).async(_ -> {
				Assert.equals("first name", logDebug());
				modifyDebug("second name");
				Assert.equals("second name", logDebug());
			});
		});
	}

	function testScope() {
		CoroRun.runScoped(node -> {
			node.with(new DebugName("first name")).async(_ -> {
				scope(_ -> {
					Assert.equals("first name", logDebug());
					modifyDebug("second name");
					Assert.equals("second name", logDebug());
				});
			});
		});
	}

	function testEntrypoint() {
		CoroRun.with(new DebugName("first name")).run(scope -> {
			Assert.equals("first name", logDebug());
			modifyDebug("second name");
			Assert.equals("second name", logDebug());
		});

		CoroRun
			.with(new DebugName("wrong name"))
			.with(new DebugName("first name"))
			.run(scope -> {
				Assert.equals("first name", logDebug());
				modifyDebug("second name");
				Assert.equals("second name", logDebug());
		});
	}

	// function testSchedulerReplacement() {
	// 	final delayed = 10000000;

	// 	final scheduler = new VirtualTimeScheduler();
	// 	final task = Coroutine.with(scheduler).create(_ -> {
	// 		delay(delayed);
	// 		"done";
	// 	});

	// 	task.start();
	// 	scheduler.advanceBy(delayed);

	// 	if (Assert.isFalse(task.isActive())) {
	// 		Assert.equals("done", task.get());
	// 	}

	// 	final scheduler = new VirtualTimeScheduler();
	// 	final task      = Coroutine.with(scheduler).create(_ -> {
	// 		delay(delayed);
	// 		throw "oh no";
	// 	});

	// 	task.start();
	// 	scheduler.advanceBy(delayed);

	// 	if (Assert.isFalse(task.isActive())) {
	// 		Assert.isOfType(task.getError(), ValueException);
	// 	}
	// }
}