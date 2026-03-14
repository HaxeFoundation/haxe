package cases;

import utest.Assert;
import sys.thread.Deque;

@:timeout(2000)
class TestDeque extends ThreadTestBase {
	#if !jvm // doesn't work on Java's LinkedBlockingDeque
	function testPopNullMessage() {
		final deque = new Deque<Dynamic>();
		deque.add(null);
		final result = deque.pop(true);
		Assert.isNull(result);
	}

	function testSendNullMessage() {
		final main = Thread.current();
		Thread.create(() -> {
			main.sendMessage(null);
		});
		final result = Thread.readMessage(true);
		Assert.isNull(result);
	}

	function testSendMultipleWithNull() {
		final main = Thread.current();
		final numThreads = 5;
		for (_ in 0...numThreads) {
			Thread.create(() -> {
				main.sendMessage(null);
			});
		}
		for (_ in 0...numThreads) {
			final result = Thread.readMessage(true);
			Assert.isNull(result);
		}
	}

	function testMixedNullAndNonNull() {
		final deque = new Deque<Dynamic>();
		deque.add("hello");
		deque.add(null);
		deque.add(42);
		Assert.equals("hello", deque.pop(true));
		Assert.isNull(deque.pop(true));
		Assert.equals(42, deque.pop(true));
	}

	function testNonBlockingPop() {
		final deque = new Deque<Dynamic>();
		Assert.isNull(deque.pop(false));
		deque.add("value");
		Assert.equals("value", deque.pop(false));
		Assert.isNull(deque.pop(false));
	}
	#end
}
