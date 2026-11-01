package cases;

import utest.Assert;
import sys.thread.Condition;
import sys.thread.Thread;
import sys.thread.Deque;
#if eval
import eval.vm.NativeThread;
#end

class TestEval extends ThreadTestBase {
	#if eval
	function testNative() {
		final deque = new Deque<Dynamic>();

		function threadEntry() {
			final self = NativeThread.self();
			final id = self.id();
			final firstMessage = NativeThread.readMessage(false);
			deque.push(self);
			final secondMessage = NativeThread.readMessage(true);
			NativeThread.delay(0.1);
			NativeThread.yield();
			deque.push({id: id, firstMessage: firstMessage, secondMessage: secondMessage});
			NativeThread.exit();
			throw "unreachable";
		}

		function mainThreadCheck() {
			final subThread = deque.pop(true);
			subThread.sendMessage("Hello from the main thread!");
			final subThreadResult = deque.pop(true);
			Assert.equals(subThreadResult.id, subThread.id());
			Assert.isNull(subThreadResult.firstMessage);
			Assert.equals(subThreadResult.secondMessage, "Hello from the main thread!");
		}

		sys.thread.Thread.create(threadEntry);
		mainThreadCheck();

		eval.luv.Thread.create(threadEntry);
		mainThreadCheck();

		eval.luv.ThreadPool.queueWork(eval.luv.Loop.defaultLoop(), null, threadEntry, _ -> {});
		mainThreadCheck();
	}

	/**
		Strings cache character offsets, and threads that use the same string share that cache.
		Updating it from several threads used to corrupt it, which ended a thread with an
		internal compiler error. `join` re-raises such an error in this thread.
	**/
	function testSharedStringOffsetCache() {
		final strings = [for (i in 0...NUM_STRINGS) '/some/path/to/a/file/in/a/package/File$i.hx'];
		final hits = [for (_ in 0...NUM_THREADS) 0];
		final threads = [
			for (t in 0...NUM_THREADS) new NativeThread(() -> {
				for (s in strings) {
					// The start indices of these calls are more than 20 characters apart,
					// which adds a second entry to the cache.
					if (StringTools.endsWith(s, ".hx")) {
						hits[t]++;
					}
					if (StringTools.endsWith(s, "path/to/a/file/in/a/package/File")) {
						hits[t]++;
					}
				}
			})
		];
		for (thread in threads) {
			NativeThread.join(thread);
		}
		for (t in 0...NUM_THREADS) {
			Assert.equals(NUM_STRINGS, hits[t]);
		}
	}

	static inline final NUM_THREADS = 4;
	static inline final NUM_STRINGS = 20000;
	#end
}