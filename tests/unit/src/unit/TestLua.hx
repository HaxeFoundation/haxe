package unit;

import utest.Assert;

class TestLua extends Test {
	function testMultiReturnWrap(){
		var multi : Multi = untyped MultiCall.doit();
		var l = lua.Lua.type(multi);
		// test that the original multi variable was wrapped
		eq(lua.Lua.type(untyped __lua__("multi")), "table");
		eq(l, "table");
	}

	function testMultiReturnPlainFunctionCall(){
		var multi : Multi = untyped MultiCall.doit();
		// this shouldn't box the result
		eq(untyped __lua__("multi"), null);
	}

	function testMultiReturnValue(){
		var multi : Multi = untyped MultiCall.doit();
		var l = lua.Lua.type(multi.b);
		// test that the original multi wrapped variable was never created
		eq(untyped __lua__("multi"), null);
		eq(l, "string");
	}

	function testMultiReturnArgument(){
		// make sure multireturns passed as arguments are wrapped
		eq(MultiCall.acceptMr(untyped MultiCall.doit()), true);
	}

	function testMultiReturnValueHandled(){
		// make sure that multireturn is not wrapped if return values not used
		var old_hx_box_mr = untyped _hx_box_mr;
		var called = false;
		untyped _hx_box_mr = function(){
			called = true;
		}
		var k = lua.NativeStringTools.find("foo bar", "foo");
		eq(called,false);
		called = true;
		// make sure that multireturn is wrapped if return values not used
		var l = lua.NativeStringTools.find("foo bar", "foo");
		var m = l + '';
		eq(called, true);
		untyped _hx_box_mr = old_hx_box_mr;
	}

	function testSwitchSubjectSideEffects() {
		// Test that switch subject is evaluated for side effects even with empty cases.
		// This was a bug where switch with only default (no explicit cases) would
		// skip evaluating the subject expression entirely.
		var counter = 0;
		var getValue = function() {
			counter++;
			return 42;
		};

		// Switch with only default (empty cases array) - subject must still be evaluated
		switch (getValue()) {
			default:
		}
		eq(counter, 1);

		// Switch with only default that has a body
		counter = 0;
		var result = 0;
		switch (getValue()) {
			default: result = 99;
		}
		eq(counter, 1);
		eq(result, 99);
	}

	// Issue #11842: Non-function fields should not be wrapped with _hx_funcToField
	function testNoFuncToFieldForNonFunctions() {
		var a:Issue11842Slot = { data: 0, func: null };
		var b:Issue11842Slot = { data: 42, func: function(x) return x * 2 };

		// Non-function field assignment should work correctly
		a.data = b.data;
		eq(a.data, 42);

		// Function field assignment should also work correctly
		a.func = b.func;
		eq(a.func(10), 20);
	}

	function testMetatablesAreShared() {

		// New class instances get metatables assigned to them
		final a = new TLA();
		t(lua.Lua.getmetatable(cast a) != null);

		// Instances of the same class share a metatable
		final a2 = new TLA();
		eq(lua.Lua.getmetatable(cast a), lua.Lua.getmetatable(cast a2));

		// Subclass does not share a metatable with the parent
		final aChild = new TLAChild();
		t(lua.Lua.getmetatable(cast aChild) != null);
		Assert.notEquals(lua.Lua.getmetatable(cast a), lua.Lua.getmetatable(cast aChild));

		// Neither do any other arbitrary two classes
		final b = new TLB();
		t(lua.Lua.getmetatable(cast b) != null);
		Assert.notEquals(lua.Lua.getmetatable(cast a), lua.Lua.getmetatable(cast b));
		Assert.notEquals(lua.Lua.getmetatable(cast aChild), lua.Lua.getmetatable(cast b));
	}

	function testSelfCallMethod() {
		// Create a callable object using Lua metatables
		final callable:SelfCallable = untyped __lua__("setmetatable({value = 10}, {__call = function(self, x) return self.value + x end})");
		// @:selfCall method should generate callable(5) instead of callable:call(5)
		eq(callable.call(5), 15);
	}

	// Issue #10089: Function callbacks passed via anonymous objects should work correctly
	function testFunctionCallbackInAnonObject() {
		var result:String = null;
		var callback = function(arg:String) {
			result = arg;
		};
		var callable = new Issue10089Callable({callback: callback});
		callable.invoke();
		eq(result, "Argument String");
	}

	// Issue #11901: Function from Dynamic object stored in class Var field
	function testFunctionFromDynamicObject() {
		var a = new Issue11901Test({
			test: function(k:Dynamic, v:Dynamic) {
				return Std.string(k) + "," + Std.string(v);
			}
		});
		eq(a.func("a", 1), "a,1");
		eq(a.call("b", 2), "b,2");
	}

	// Issue #7738: Nested function in typedef-based anonymous object
	function testNestedFunctionInTypedef() {
		var result:String = null;
		Issue7738Helper.process({
			time: 1000,
			onComplete: function() {
				result = "completed";
			}
		});
		eq(result, "completed");
	}

	// Issue #10055: Function extracted from typedef anon object to local should be unwrapped
	function testFunctionExtractedFromTypedefAnon() {
		var result:String = null;
		Issue10055Helper.take({
			callback: function(arg:String) {
				result = arg;
			}
		});
		eq(result, "test");
	}

	// Issue #7539: Closure in conditional expression should capture correct parameter
	function testClosureInConditionalExpression() {
		var obj = new Issue7539Test();
		var args:Array<String> = [];
		args.push("bar");
		obj.foo(args);
		eq(args.length, 2);
		eq(args[0], "bar");
		eq(args[1], "added");
	}

	// Issue #10090: Many map operations should not exceed Lua's 200 local variable limit
	function testLocalVariableReuse() {
		// This test would fail with "too many local variables" before the fix
		// Each map operation generates temp vars that should be reused
		var map = new Map<Issue10090Object, Bool>();
		map[new Issue10090Object()] = true;
		map[new Issue10090Object()] = true;
		map[new Issue10090Object()] = true;
		map[new Issue10090Object()] = true;
		map[new Issue10090Object()] = true;
		map[new Issue10090Object()] = true;
		map[new Issue10090Object()] = true;
		map[new Issue10090Object()] = true;
		map[new Issue10090Object()] = true;
		map[new Issue10090Object()] = true;
		// If we got here without error, the optimization is working
		var count = 0;
		for (_ in map.keys()) count++;
		eq(10, count);
	}

	// Issue #12192: Closure inside try-catch inside loop should not inherit loop context
	function testClosureBreakInTryCatchLoop() {
		// Test 1: Closure with try-catch inside loop should not generate pcall_break check
		var closureCalled = false;
		var outerLoopRan = false;
		while (!outerLoopRan) {
			outerLoopRan = true;
			var f = function() {
				try {
					closureCalled = true;
				} catch (e:Dynamic) {}
			};
			f();
		}
		t(closureCalled);

		// Test 2: Break inside loop inside closure should use plain break
		var afterInnerLoop = false;
		var closureResult:String = null;
		while (true) {
			try {
				var g = function() {
					var i = 0;
					while (true) {
						i++;
						if (i > 3) {
							break; // should be plain break, not _G.error
						}
					}
					afterInnerLoop = true;
					return "done";
				};
				closureResult = g();
			} catch (e:Dynamic) {}
			break;
		}
		t(afterInnerLoop);
		eq(closureResult, "done");
	}
}

@:multiReturn extern class Multi {
	var a : Int;
	var b : String;
}

class MultiCall {
	public static function doit() : Dynamic {
		return untyped __lua__("1,'hi'");
	}
	public static function acceptMr(m:Multi){
		return lua.Lua.type(m) == "table";
	}
}

class TLA { private var foo: String; public function new() { this.foo = "A"; } }
class TLAChild extends TLA { public function new() { super(); this.foo = "AChild"; } }
class TLB { private var foo: String; public function new() { this.foo = "B"; } }

// Issue #11842
typedef Issue11842Slot = {
	var data:Int;
	var func:(Int) -> Int;
}

// Issue #9369
extern class SelfCallable {
	@:selfCall function call(x:Int):Int;
}

// Issue #10089
typedef Issue10089CallableParams = {
	var callback:String->Void;
}

class Issue10089Callable {
	var callback:String->Void;
	public function new(params:Issue10089CallableParams) {
		callback = params.callback;
	}
	public function invoke() {
		callback("Argument String");
	}
}

// Issue #11901
class Issue11901Test {
	public var func:(Dynamic, Dynamic) -> String;
	public function new(obj:Dynamic) {
		this.func = obj.test;
	}
	public function call(k:Dynamic, v:Dynamic):String {
		return func(k, v);
	}
}

// Issue #7738
typedef Issue7738Args = {
	?time:Int,
	?onComplete:Void->Void
}

class Issue7738Helper {
	public static function process(args:Issue7738Args) {
		if (args.onComplete != null) {
			args.onComplete();
		}
	}
}

// Issue #10055
typedef Issue10055Params = {
	?callback:(String)->Void
}

class Issue10055Helper {
	public static function take(p:Issue10055Params) {
		// Extract to local variable - this should unwrap the anon function
		var cb = p.callback;
		if (cb != null) {
			cb("test");
		}
	}
}

// Issue #7539: Closure in conditional should not capture wrong variable
class Issue7539Test {
	public var foo:Array<String>->Void;

	public function new(?foo:Array<String>->Void) {
		this.foo = if (foo != null) foo else function(args) {
			// 'args' here should be the function parameter, not 'this'
			args.push("added");
		};
	}
}

// Issue #10090: Helper class for local variable reuse test
class Issue10090Object {
	public function new() {}
}
