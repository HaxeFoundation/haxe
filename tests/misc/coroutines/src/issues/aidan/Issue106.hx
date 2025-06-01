package issues.aidan;

import haxe.coro.schedulers.VirtualTimeScheduler;
import hxcoro.CoroRun;
import hxcoro.Coro.*;
import hxcoro.task.AbstractTask;

class Issue106 extends utest.Test {
	public function test() {
		var scheduler = new VirtualTimeScheduler();

		final numChildren = 1000;

		final task = CoroRun.with(scheduler).create(node -> {
			var k = 0;
			for (_ in 0...numChildren) {
				node.async(_ -> {
					// https://github.com/Aidan63/haxe/issues/98 prevents writing a test utilizing loop variables
					delay(Math.random() > 0.5 ? 5 : 10);
					k++;
				});
			}
			delay(11);
			k;
		});
		task.start();
		final atask:AbstractTask<Any> = cast task;
		scheduler.advanceTo(5);
		for (child in @:privateAccess atask.children) {
			Assert.isTrue(child == null || child.isActive());
		}
		scheduler.advanceTo(10);
		for (child in @:privateAccess atask.children) {
			Assert.isTrue(child == null);
		}
		scheduler.advanceTo(11);
		Assert.equals(numChildren, task.get());
	}
}