package issues.aidan;

import haxe.coro.schedulers.VirtualTimeScheduler;
import haxe.coro.schedulers.Scheduler;
import hxcoro.ds.channels.Channel;
import hxcoro.ds.PagedDeque;

// class Junction {
// 	var leftOpen:Bool;
// 	var waiters:PagedDeque<SuspendedRead<Any>>;

// 	public function new(leftOpen:Bool) {
// 		this.leftOpen = leftOpen;
// 		waiters = new PagedDeque();
// 	}

// 	function flushWaiters() {
// 		while (!waiters.isEmpty()) {
// 			final cont = waiters.pop();
// 			cont.context.get(Scheduler).schedule(0, () -> cont.resume(null, null));
// 		}
// 	}

// 	public function switchDirections() {
// 		leftOpen = !leftOpen;
// 		flushWaiters();
// 	}

// 	@:coroutine public function goLeft() {
// 		if (leftOpen) {
// 			return;
// 		}
// 		suspendCancellable(cont -> new SuspendedRead(cont, waiters));
// 	}

// 	public function openLeft() {
// 		if (leftOpen) {
// 			return;
// 		}
// 		leftOpen = true;
// 		flushWaiters();
// 	}

// 	@:coroutine public function goRight() {
// 		if (!leftOpen) {
// 			return;
// 		}
// 		suspendCancellable(cont -> new SuspendedRead(cont, waiters));
// 	}

// 	public function openRight() {
// 		if (!leftOpen) {
// 			return;
// 		}
// 		leftOpen = false;
// 		flushWaiters();
// 	}
// }

class Issue126 extends utest.Test {
	function test() {
		Assert.pass('TODO!');
		// final scheduler = new VirtualTimeScheduler();
		// final task = CoroRun.with(scheduler).create(node -> {
		// 	final channel = Channel.create(Bounded(1));
		// 	@:coroutine function log(s:String) {
		// 		channel.write('${scheduler.now()}: $s');
		// 	}
		// 	final junction = new Junction(true);
		// 	final leftChild = node.async(node -> {
		// 		while (true) {
		// 			junction.goLeft();
		// 			log("left");
		// 			delay(500);
		// 		}
		// 	});
		// 	final rightChild = node.async(node -> {
		// 		while (true) {
		// 			junction.goRight();
		// 			log("right");
		// 			delay(500);
		// 		}
		// 	});
		// 	final directionSwitcher = node.async(node -> {
		// 		while (true) {
		// 			delay(2000);
		// 			log("switching");
		// 			junction.switchDirections();
		// 		}
		// 	});
		// 	final output = [];
		// 	while (output.length < 20) {
		// 		output.push(channel.read());
		// 	}
		// 	leftChild.cancel();
		// 	rightChild.cancel();
		// 	directionSwitcher.cancel();
		// 	output;
		// });
		// task.start();
		// while (task.isActive()) {
		// 	scheduler.advanceBy(1);
		// }

		// trace(task.get());

		// Assert.same([
		// 	   "0: left",
		// 	 "500: left",
		// 	"1000: left",
		// 	"1500: left",
		// 	"2000: switching",
		// 	"2000: right",
		// 	"2500: right",
		// 	"3000: right",
		// 	"3500: right",
		// 	"4000: switching",
		// 	"4000: left",
		// 	"4500: left",
		// 	"5000: left",
		// 	"5500: left",
		// 	"6000: switching",
		// 	"6000: right",
		// 	"6500: right",
		// 	"7000: right",
		// 	"7500: right",
		// 	"8000: switching",
		// ], task.get());
	}
}