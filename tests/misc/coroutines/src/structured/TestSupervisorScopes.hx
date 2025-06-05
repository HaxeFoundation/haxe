package structured;

import haxe.Exception;
import haxe.coro.schedulers.VirtualTimeScheduler;

class TestSupervisorScopes extends utest.Test {
	function testChildThrow() {
		final result = CoroRun.runScoped(node -> {
			supervisor(node -> {
				final throwingChild = node.async(_ -> throw "oh no");
				node.awaitChildren();
				"ok";
			});
		});
		Assert.equals("ok", result);
	}

	function testChildThrowAwaitChildren() {
		final result = CoroRun.runScoped(node -> {
			supervisor(node -> {
				final throwingChild = node.async(_ -> throw "oh no");
				node.awaitChildren();
				"ok";
			});
		});
		Assert.equals("ok", result);
	}

	function testChildThrowAwait() {
		CoroRun.runScoped(node -> {
			AssertAsync.raises(() -> {
				supervisor(node -> {
					final throwingChild = node.async(_ -> throw "oh no");
					throwingChild.await();
				});
			}, String);
		});
	}

	function testChildThrowAwaitTransitive() {
		CoroRun.runScoped(node -> {
			AssertAsync.raises(() -> {
				supervisor(node -> {
					final throwingChild = node.async(_ -> throw "oh no");
					final awaitingChild = node.async(_ -> throwingChild.await());
					awaitingChild.await();
				});
			}, String);
		});
	}

	function testThrowSelf() {
		CoroRun.runScoped(node -> {
			AssertAsync.raises(() -> {
				supervisor(node -> {
					throw "oh no";
				});
			}, String);
		});
	}
}