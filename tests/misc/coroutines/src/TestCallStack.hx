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
					Line(8),
					Line(6),
					Line(6),
					Line(6),
					Line(6),
					Line(15),
				Skip('callstack/SyncMiddle.hx'),
					Line(4),
					Line(8),
				File('callstack/CoroLower.hx'),
					Line(6),
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
		function checkStack(e:Exception) {
			final stack = e.stack.asArray();
			var inspector = new CallStackInspector(stack);
			var r = inspector.inspect([
				File('callstack/FooBarBaz.hx'),
				#if cpp
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
		try {
			CoroRun.run(callstack.FooBarBaz.foo);
			Assert.fail("Exception expected");
		} catch(e:Exception) {
			checkStack(e);
		}

		try {
			CoroRun.runScoped(scope -> {
				scope.async(scope -> {
					scope.async(_ -> {
						callstack.FooBarBaz.foo();
					});
				});
			});
			Assert.fail("Exception expected");
		} catch (e:Exception) {
			checkStack(e);
		}
	}
}