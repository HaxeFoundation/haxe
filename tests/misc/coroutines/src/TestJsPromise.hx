import js.lib.Error;
import js.lib.Promise;

@:coroutine
private function await<T>(p:Promise<T>):T {
	return Coroutine.suspend(cont -> p.then(r -> cont(r, null), e -> cont(null, e)));
}

private function promise<T>(c:Coroutine<()->T>):Promise<T> {
	return new Promise((resolve,reject) -> c.start((result, error) -> if (error != null) reject(error) else resolve(result)));
}

class TestJsPromise extends utest.Test {
	function testAwait(async:Async) {
		var p = Promise.resolve(41);

		@:coroutine function awaiting() {
			var x = await(p);
			return x + 1;
		}

		awaiting.start((result,error) -> {
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

	function testAwaitRejected(async:Async) {
		var p = Promise.reject("oh no");

		@:coroutine function awaiting() {
			var x = await(p);
			return x + 1;
		}

		awaiting.start((result,error) -> {
			Assert.equals("oh no", error);
			async.done();
		});
	}

	function testThrowInPromise(async:Async) {
		var p = promise(() -> throw new Error("oh no"));
		p.then(
			function(result) {
				Assert.fail();
			},
			function(error) {
				Assert.isOfType(error, Error);
				Assert.equals("oh no", (error : Error).message);
				async.done();
			}
		);
	}
}
