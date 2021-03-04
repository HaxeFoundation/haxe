package cases;

import utest.Assert;
import haxe.ds.GenericStack;
import utest.ITest;

class DequeBrackets implements ITest {
	public function new() {}

	/**
		Spawns a thread to insert bracket pairs into a Deque. The opening
		one is placed in front, the closing one in the back. This is going
		to result in something like `([{<>}])` which we check for at the end.
	**/
	@:timeout(2000)
	public function test(async:utest.Async) {
		Thread.create(() -> {
			var deque = new Deque();
			var dequeMutex = new Mutex();
			function add(open:String, close:String) {
				dequeMutex.acquire();
				deque.push(open);
				deque.add(close);
				dequeMutex.release();
			}

			var pairs = [
				{open: "(", close: ")"},
				{open: "[", close: "]"},
				{open: "{", close: "}"},
				{open: "<", close: ">"}
			];
			var iterationsPerThread = 100;

			var lock = new Lock();
			var self = Thread.current();
			Thread.create(() -> {
				for (_ in 0...pairs.length) {
					Assert.isTrue(lock.wait());
				}
				self.sendMessage("done");
			});
			var threads = [];
			for (pair in pairs) {
				threads.push(Thread.create(() -> {
					Thread.readMessage(true);
					for (_ in 0...iterationsPerThread) {
						add(pair.open, pair.close);
						Sys.sleep(0.0001); // sleep a bit to increase chaos
					}
					lock.release();
				}));
			}
			for (thread in threads) {
				thread.sendMessage("go");
			}
			switch (Thread.readMessage(true)) {
				case "done":
				case s:
					Assert.fail("Unexpected message: " + s);
			}
			var stack = new GenericStack<String>();
			function pop() {
				return deque.pop(false);
			}
			for (_ in 0...pairs.length * iterationsPerThread) {
				stack.add(pop());
			}
			for (elt in stack) {
				var expected = switch (elt) {
					case "(": ")";
					case "<": ">";
					case "{": "}";
					case "[": "]";
					case s:
						Assert.fail("Unexpected " + s);
						s;
				}
				Assert.equals(expected, pop());
			}
			async.done();
		});
	}
}
