package unit;

class TestGcFinalizer extends Test {
	function testConstructNoThrow() {
		#if (js || python || eval || lua || jvm || cpp)
		var finalizer = new haxe.GcFinalizer(function(v:String) {});
		t(finalizer != null);
		#else
		noAssert();
		#end
	}

	function testRegisterNoThrow() {
		#if (js || python || eval || lua || jvm || cpp)
		var finalizer = new haxe.GcFinalizer(function(v:String) {});
		var target = {id: 1};
		var handle = finalizer.register(target, "hello");
		t(handle != null);
		#else
		noAssert();
		#end
	}

	function testCloseNoThrow() {
		#if (js || python || eval || lua || jvm || cpp)
		var finalizer = new haxe.GcFinalizer(function(v:String) {});
		var target = {id: 1};
		var handle = finalizer.register(target, "hello");
		handle.close();
		t(true);
		#else
		noAssert();
		#end
	}

	function testUnsupportedTargetThrows() {
		#if !(js || python || eval || lua || jvm || cpp)
		exc(function() new haxe.GcFinalizer(function(v:String) {}));
		#else
		noAssert();
		#end
	}

	function testCallbackFiresAfterGc() {
		#if eval
		var called = false;
		var heldResult:Null<String> = null;
		var finalizer = new haxe.GcFinalizer(function(v:String) {
			called = true;
			heldResult = v;
		});
		finalizer.register({id: 1}, "collected");
		// Force GC
		eval.vm.Gc.full_major();
		eval.vm.Gc.full_major();
		t(called);
		eq(heldResult, "collected");
		#else
		noAssert();
		#end
	}

	function testClosePreventsCallback() {
		#if eval
		var called = false;
		var finalizer = new haxe.GcFinalizer(function(v:String) {
			called = true;
		});
		var handle = finalizer.register({id: 1}, "collected");
		handle.close();
		// Force GC
		eval.vm.Gc.full_major();
		eval.vm.Gc.full_major();
		f(called);
		#else
		noAssert();
		#end
	}
}
