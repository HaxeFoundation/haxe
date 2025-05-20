import haxe.CallStack;
import haxe.Exception;
import callstack.CallStackInspector;

class TestCallStack extends utest.Test {
	function test() {
		try {
			callstack.Bottom.entry();
			Assert.fail("Exception expected");
		} catch(e:haxe.exceptions.NotImplementedException) {
			final stack = e.stack.asArray();
			var inspector = new CallStackInspector(stack);
			var r = inspector.inspect([
				File('callstack/Top.hx'),
					Line(4),
					Line(8),
					Line(12),
				File('callstack/CoroUpper.hx'),
					Line(10),
					Line(8),
					Line(8),
					Line(8),
					Line(8),
					Line(17),
				Skip('callstack/SyncMiddle.hx'),
					Line(4),
					Line(8),
				File('callstack/CoroLower.hx'),
					Line(8),
				Skip('callstack/Bottom.hx'),
					Line(4)
			]);
			checkFailure(stack, r);
		}
	}

	function checkFailure(stack:Array<StackItem>, r:Null<CallStackInspectorFailure>) {
		if (r == null) {
			Assert.pass();
		} else {
			var i = 0;
			var lines = stack.map(item -> '\t[${i++}] $item');
			Assert.fail('${r.toString()}\n${lines.join("\n")}');
		}
	}

	function testFooBazBaz() {
		try {
			Coroutine.run(callstack.FooBarBaz.foo);
			Assert.fail("Exception expected");
		} catch(e:Exception) {
			final stack = e.stack.asArray();
			var inspector = new CallStackInspector(stack);
			var r = inspector.inspect([
				File('callstack/FooBarBaz.hx'),
				#if (cpp && coroutine.noopt)
				// TODO: cpp has inaccurate positions which causes the top stack to be wrong
				Line(6),
				Line(12),
				Line(12),
				Line(16),
				#else
				Line(7),
				Line(12),
				#end
				// TODO: sync stack doesn't work yet
				// Line(16)
			]);
			checkFailure(stack, r);
		}
	}
}