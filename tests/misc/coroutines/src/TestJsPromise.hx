import js.lib.Promise;

@:coroutine
private function await<T>(p:Promise<T>):T {
	return Coroutine.suspend(cont -> p.then(cont));
}

private function promise<T>(c:Coroutine<()->T>):Promise<T> {
	return new Promise((resolve,_) -> c.start(resolve));
}

class TestJsPromise extends utest.Test {
	function testAwait(async:Async) {
		var p = Promise.resolve(41);

		@:coroutine function awaiting() {
			var x = await(p);
			return x + 1;
		}

		awaiting.start(result -> {
			Assert.equals(42, result);
			async.done();
		});
	}

	function testPromise(async:Async) {
		var p = promise(() -> 42);
		p.then(result -> {
			Assert.equals(42, result);
			async.done();
		});
	}

	function testAsyncAwait(async:Async) {
		var p1 = Promise.resolve(41);

		var p2 = promise(() -> {
			var x = await(p1);
			return x + 1;
		});

		p2.then(result -> {
			Assert.equals(42, result);
			async.done();
		});
	}
}
