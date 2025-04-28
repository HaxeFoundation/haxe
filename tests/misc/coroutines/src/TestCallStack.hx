import callstack.CallStackInspector;

class TestCallStack extends utest.Test {
	function test() {
		try {
			callstack.Bottom.entry();
			Assert.fail("Exception expected");
		} catch(e:haxe.exceptions.NotImplementedException) {
			var inspector = new CallStackInspector(e.stack.asArray());
			final prefix = #if hl "" #else "src/" #end;
			var r = inspector.inspect([
				File('${prefix}callstack/Top.hx'),
					Line(4),
					Line(8),
					Line(12),
				File('${prefix}callstack/CoroUpper.hx'),
					Line(10),
					Line(8),
					Line(8),
					Line(8),
					Line(8),
					Line(17),
				Skip('${prefix}callstack/SyncMiddle.hx'),
					Line(4),
					Line(8),
				File('${prefix}callstack/CoroLower.hx'),
					Line(8),
				Skip('${prefix}callstack/Bottom.hx'),
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