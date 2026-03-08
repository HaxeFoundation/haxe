package unit;

class TestGcFinalizer extends Test {
	function testConstructNoThrow() {
		#if (js || python || cpp || eval || lua || jvm)
		var finalizer = new haxe.GcFinalizer(function(v:String) {});
		t(finalizer != null);
		#else
		noAssert();
		#end
	}

	function testRegisterNoThrow() {
		#if (js || python || cpp || eval || lua || jvm)
		var finalizer = new haxe.GcFinalizer(function(v:String) {});
		var target = {id: 1};
		finalizer.register(target, "hello");
		t(true);
		#else
		noAssert();
		#end
	}

	function testRegisterWithToken() {
		#if (js || python || cpp || eval || lua || jvm)
		var finalizer = new haxe.GcFinalizer(function(v:String) {});
		var target = {id: 1};
		var token = {id: 99};
		finalizer.register(target, "hello", token);
		t(true);
		#else
		noAssert();
		#end
	}

	function testUnregisterNoThrow() {
		#if (js || python || cpp || eval || lua || jvm)
		var finalizer = new haxe.GcFinalizer(function(v:String) {});
		var target = {id: 1};
		var token = {id: 99};
		finalizer.register(target, "hello", token);
		finalizer.unregister(token);
		t(true);
		#else
		noAssert();
		#end
	}

	function testUnsupportedTargetThrows() {
		#if !(js || python || cpp || eval || lua || jvm)
		exc(function() new haxe.GcFinalizer(function(v:String) {}));
		#else
		noAssert();
		#end
	}

	function testCallbackFiresAfterGc() {
		#if (cpp || eval)
		var called = false;
		var heldResult:Null<String> = null;
		var finalizer = new haxe.GcFinalizer(function(v:String) {
			called = true;
			heldResult = v;
		});
		finalizer.register({id: 1}, "collected");
		// Force GC
		#if cpp
		cpp.vm.Gc.run(true);
		cpp.vm.Gc.run(true);
		#elseif eval
		eval.vm.Gc.full_major();
		eval.vm.Gc.full_major();
		#end
		t(called);
		eq(heldResult, "collected");
		#else
		noAssert();
		#end
	}

	function testUnregisterPreventsCallback() {
		#if (cpp || eval)
		var called = false;
		var finalizer = new haxe.GcFinalizer(function(v:String) {
			called = true;
		});
		var token = {id: 99};
		finalizer.register({id: 1}, "collected", token);
		finalizer.unregister(token);
		// Force GC
		#if cpp
		cpp.vm.Gc.run(true);
		cpp.vm.Gc.run(true);
		#elseif eval
		eval.vm.Gc.full_major();
		eval.vm.Gc.full_major();
		#end
		f(called);
		#else
		noAssert();
		#end
	}
}
