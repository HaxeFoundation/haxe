import callstack.CallStackInspector;

class TestCallStack extends utest.Test {
	function test() {
		try {
			callstack.Bottom.entry();
			Assert.fail("Exception expected");
		} catch(e:haxe.exceptions.NotImplementedException) {
			var inspector = new CallStackInspector(e.stack.asArray());
			var r = inspector.inspect([
				File('callstack/Top.hx'),
					Line(4),
					Line(8),
					Line(12),
				File('callstack/CoroUpper.hx'),
					Line(10),
				#if hl
					Line(5), // I still don't think this should be here
				#end
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
			if (r == null) {
				Assert.pass();
			} else {
				var i = 0;
				var lines = e.stack.asArray().map(item -> '\t[${i++}] $item');
				Assert.fail('${r.toString()}\n${lines.join("\n")}');
			}
		}
	}
}