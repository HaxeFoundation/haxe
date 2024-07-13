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
