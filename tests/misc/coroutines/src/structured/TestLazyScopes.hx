package structured;

import structured.TestThrowingScopes.FooException;

class TestLazyScopes extends utest.Test {
	function test_create_return() {
		final result = CoroRun.runScoped(node -> {
			final child = node.lazy(_ -> return "foo");
			return child.await();
		});
		Assert.equals("foo", result);
	}

	function test_create_throw() {
		Assert.raises(() -> CoroRun.runScoped(node -> {
			final child = node.lazy(_ -> throw new FooException());
			AssertAsync.raises(() -> child.await(), FooException);
		}), FooException);
	}

	function test_create_unlaunched() {
		Assert.raises(() -> CoroRun.runScoped(node -> {
			node.lazy(_ -> {
				throw new FooException();
			});
		}), FooException);
	}

	function test_create_unlaunched_nested() {
		Assert.raises(() -> CoroRun.runScoped(node -> {
			node.lazy(node -> {
				node.lazy(node -> {
					throw new FooException();
				});
			});
		}), FooException);
	}

	function test_create_unlaunched_yield() {
		Assert.raises(() -> CoroRun.runScoped(node -> {
			node.lazy(_ -> {
				yield();
				throw new FooException();
			});
		}), FooException);
	}

	function test_create_unlaunched_yield_nested() {
		Assert.raises(() -> CoroRun.runScoped(node -> {
			node.lazy(node -> {
				yield();
				node.lazy(node -> {
					yield();
					throw new FooException();
				});
			});
		}), FooException);
	}

	function test_create_catch() {
		final result = CoroRun.runScoped(node -> {
			try {
				scope(node -> {
					final child = node.lazy(_ -> throw new FooException());
					child.await();
				});
				return "wrong";
			} catch (exc:FooException) {
				return exc.message;
			}
		});
		Assert.equals("foo", result);
	}
}
