package ds;

import haxe.coro.schedulers.VirtualTimeScheduler;
import hxcoro.Coro.*;
import hxcoro.CoroRun;
import hxcoro.ds.Channel;

class TestChannel extends utest.Test {
	function test() {
		final size = 100;
		final channel = new Channel();
		final scheduler = new VirtualTimeScheduler();
		final task = CoroRun.with(scheduler).create(node -> {
			final output = [];
			final writer = node.async(_ -> {
				var i = size;

				while (i >= 0) {
					channel.write(i);

					i--;

					delay(Std.random(5));
				}
			});
			for (_ in 0...size + 1) {
				output.push(channel.read());
				delay(Std.random(5));
			}
			writer.cancel();
			output;
		});
		task.start();
		while (task.isActive()) {
			scheduler.run();
			scheduler.advanceBy(1);
		}
		final expected = [for (i in 0...size + 1) i];
		expected.reverse();
		Assert.same(expected, task.get());
	}
}