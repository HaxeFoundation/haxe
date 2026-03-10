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
	#end
}