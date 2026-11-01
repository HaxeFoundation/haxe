package unit.teststd.haxe;

class TestCallStack extends unit.Test {
	public function test() {
		var stack = haxe.CallStack.callStack();
		t((stack is Array));

		var stack = haxe.CallStack.exceptionStack();
		t((stack is Array));

		function throw2() {
			throw false;
		}
		function throw1() {
			throw2();
		}
		try {
			throw1();
		} catch (_:Dynamic) {
			var stack = haxe.CallStack.exceptionStack();
			t((stack is Array));
			#if (!lua && !flash && !hl)
			t(stack.length > 0);
			#end
		}
		#if js
		var old = @:privateAccess haxe.NativeStackTrace.lastError;
		@:privateAccess haxe.NativeStackTrace.lastError = null;
		var stack = haxe.CallStack.exceptionStack();
		t((stack is Array));
		eq(stack.length, 0);
		@:privateAccess haxe.NativeStackTrace.lastError = old;
		#end
	}
}
