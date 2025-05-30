package structured;

import haxe.Exception;
import haxe.exceptions.CancellationException;
import haxe.coro.schedulers.VirtualTimeScheduler;

private class FooException extends Exception {
	public function new() {
		super('foo');
	}
}

function has(what:Array<String>, has:Array<String>, hasNot:Array<String>, ?p:haxe.PosInfos) {
	for (has in has) {
		Assert.contains(has, what, null, p);
	}
	for (hasNot in hasNot) {
		Assert.notContains(hasNot, what, null, p);
	}
}

class TestCoroutineScope extends utest.Test {
	function test_scope_returning_value_suspending() {
		final expected = 'Hello, World';
		final actual   = CoroRun.runScoped(_ -> {
			return scope(_ -> {
				yield();

				return expected;
			});
		});

		Assert.equals(expected, actual);
	}

	function test_scope_throwing_suspending() {
		CoroRun.runScoped(_ -> {
			AssertAsync.raises(() -> CoroRun.runScoped(_ -> {
				yield();

				throw new FooException();
			}), FooException);
		});
	}

	function test_scope_with_children() {
		final actual    = [];
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(_ -> {
			scope(node -> {
				node.async(_ -> {
					delay(500);

					actual.push(0);
				});

				node.async(_ -> {
					delay(500);

					actual.push(1);
				});
			});
		});

		task.start();

		scheduler.advanceTo(499);
		Assert.same(actual, []);
		Assert.isTrue(task.isActive());

		scheduler.advanceTo(500);
		Assert.same(actual, [ 0, 1 ]);
		Assert.isFalse(task.isActive());
	}

	function test_try_raise() {
		final acc = [];
		Assert.raises(() ->
			CoroRun.runScoped(node -> {
				scope(_ -> {
					acc.push("before yield");
					yield();
					acc.push("after yield");
					throw new FooException();
					acc.push("after throw");
				});
				acc.push("at exit");
			}), FooException);
		has(acc, ["before yield", "after yield"], ["after throw", "at exit"]);
	}

	function test_try_catch() {
		final acc = [];
		CoroRun.runScoped(node -> {
			try {
				scope(_ -> {
					acc.push("before yield");
					yield();
					acc.push("after yield");
					throw new FooException();
					acc.push("after throw");
				});
				acc.push("after scope");
			} catch(e:FooException) {
				acc.push("in catch");
			}
			acc.push("at exit");
		});
		has(acc, ["before yield", "after yield", "in catch", "at exit"], ["after throw", "after scope"]);
	}

	function test_try_raise_async() {
		final acc = [];
		Assert.raises(() -> CoroRun.runScoped(node -> {
			node.async(_ -> {
				scope(_ -> {
					acc.push("before yield");
					yield();
					acc.push("after yield");
					throw new FooException();
					acc.push("after throw");
				});
			});
			acc.push("at exit");
		}), FooException);
		has(acc, ["before yield", "after yield", "at exit"], ["after throw"]);
	}

	function test_parent_scope_cancelling() {
		final acc       = [];
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			final child = node.async(_ -> {
				try {
					scope(node -> {
						while (true) {
							yield();
						}
						acc.push("scope 1");
					});
				} catch (e:CancellationException) {
					acc.push("scope 2");
				}
			});

			delay(1000);
			child.cancel();
			acc.push("scope 3");
		});

		task.start();
		scheduler.advanceBy(1000);

		has(acc, ["scope 2", "scope 3"], ["scope 1"]);
	}

	function test_cancel_due_to_sibling_exception() {
		final acc = [];
		Assert.raises(() -> CoroRun.runScoped(node -> {
			node.async(_ -> {
				scope(_ -> {
					acc.push("before yield 2");
					yield();
					acc.push("after yield 2");
					throw new FooException();
					acc.push("after throw 2");
				});
			});
			node.async(_ -> {
				scope(_ -> {
					acc.push("before yield 1");
					while (true) {
						yield();
					}
					acc.push("after yield 1");
				});
			});
			acc.push("at exit");
		}), FooException);
		has(acc, ["before yield 1", "before yield 2", "after yield 2", "at exit"], ["after yield 1", "after throw 2"]);

		acc.resize(0);
		Assert.raises(() -> CoroRun.runScoped(node -> {
			node.async(_ -> {
				scope(_ -> {
					acc.push("before yield 1");
					while (true) {
						yield();
					}
					acc.push("after yield 1");
				});
			});
			node.async(_ -> {
				scope(_ -> {
					acc.push("before yield 2");
					yield();
					acc.push("after yield 2");
					throw new FooException();
					acc.push("after throw 2");
				});
			});
			acc.push("at exit");
		}), FooException);
		has(acc, ["before yield 1", "before yield 2", "after yield 2", "at exit"], ["after yield 1", "after throw 2"]);
	}
}