package issues.aidan;

import haxe.coro.schedulers.VirtualTimeScheduler;
import hxcoro.CoroRun;
import hxcoro.Coro.*;

class Issue130 extends utest.Test {
	public function test() {
		final count     = 10;
		final actual    = [];
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			for (i in 0...count) {
				node.async(_ -> {
					final randomDelay = 100 + Std.random(400);
					delay(randomDelay);
					delay(500 - randomDelay);
					actual.push(scheduler.now());
				});
			}
		});

		task.start();
		while (task.isActive()) {
			scheduler.advanceBy(1);
		};

		Assert.equals(count, actual.length);
		for (time in actual) {
			Assert.isTrue(time == 500i64);
		}
	}
}