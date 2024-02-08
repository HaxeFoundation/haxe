package unit.issues;

private abstract Continue(Dynamic) {
	@:commutative @:op(a + b)
	extern static inline function then<A>(e:Continue, a:A):A
		return a;
}

class Issue9771 extends unit.Test {
	var buf:StringBuf;

	function append(v:Dynamic):Continue {
		buf.add(v);
		return v;
	}

	function test() {
		function bar() {
			append('bar');
			return 'bar';
		}
		buf = new StringBuf();
		eq('bar', append('foo') + bar());
		eq('foobar', buf.toString());
		buf = new StringBuf();
		eq('bar', bar() + append('foo'));
	}

	function testMutableLocal() {
		var bar = "bar";
		function append(v:Dynamic):Continue {
			bar = "notbar";
			return v;
		}
		eq('notbar', append('foo') + bar);
		bar = "bar";
		eq('bar', bar + append('foo'));
	}
}
