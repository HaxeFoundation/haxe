import hxcoro.Coro.*;

private class C<T> {
	final value:T;

	public function new(value:T) {
		this.value = value;
	}

	@:coroutine public function await() {
		return value;
	}

	@:coroutine public function awaitYield() {
		yield();
		return value;
	}
}

function run<T>(f:Coroutine<() -> T>) {
	return CoroRun.run(f);
}

class TestTexpr extends utest.Test {
	function testField() {
		var c = new C("foo");

		Assert.equals("f", run(() -> c.await().charAt(0)));
		Assert.equals("f", run(() -> c.awaitYield().charAt(0)));
	}

	function testOp() {
		var c = new C(8);
		Assert.equals(16, run(() -> c.await() + c.await()));
		// extensively tested in Issue93
	}

	function testCall() {
		function id<T>(v:T) {
			return v;
		}
		var c = new C(id);
		var d = new C("foo");
		Assert.equals("foo", run(() -> c.await()(d.await())));
		Assert.equals("foo", run(() -> c.awaitYield()(d.await())));
		Assert.equals("foo", run(() -> c.await()(d.awaitYield())));
		Assert.equals("foo", run(() -> c.awaitYield()(d.awaitYield())));
	}

	function testArray() {
		var a = [];
		var c = new C(a);
		var d = new C("foo");
		var e = new C(0);
		run(() -> {
			Assert.same(a, c.await());
			Assert.equals("foo", c.await()[e.await()] = d.await());
			Assert.equals("foo", c.await()[e.await()]);
			a.resize(0);
			Assert.equals("foo", c.awaitYield()[e.await()] = d.await());
			Assert.equals("foo", c.awaitYield()[e.await()]);
			a.resize(0);
			Assert.equals("foo", c.await()[e.awaitYield()] = d.await());
			Assert.equals("foo", c.await()[e.awaitYield()]);
			a.resize(0);
			Assert.equals("foo", c.await()[e.await()] = d.awaitYield());
			Assert.equals("foo", c.await()[e.await()]);
			a.resize(0);
			Assert.equals("foo", c.awaitYield()[e.awaitYield()] = d.await());
			Assert.equals("foo", c.awaitYield()[e.awaitYield()]);
			a.resize(0);
			Assert.equals("foo", c.awaitYield()[e.await()] = d.awaitYield());
			Assert.equals("foo", c.awaitYield()[e.await()]);
			a.resize(0);
			Assert.equals("foo", c.await()[e.awaitYield()] = d.awaitYield());
			Assert.equals("foo", c.await()[e.awaitYield()]);
			a.resize(0);
			Assert.equals("foo", c.awaitYield()[e.awaitYield()] = d.awaitYield());
			Assert.equals("foo", c.awaitYield()[e.awaitYield()]);
		});
	}
}