package unit.issues;

import haxe.Int64;

class Issue12435 extends Test {
	function testCast() {
		var i:Int = cast null;
		#if static
		#if !cppia // TODO
		t(i == 0);
		#end
		t(i == cast null);
		#else
		f(i == 0);
		t(i == null);
		#end
	}

	function testPropagation() {
		var Null:Null<Int> = null;
		var i:Int = Null;
		#if static
		t(i == 0);
		f(i == Null);
		#else
		f(i == 0);
		t(i == null);
		#end
	}

	function testPropagationNullable() {
		var Null:Null<Int> = null;
		var i:Null<Int> = Null;
		f(i == 0);
		t(i == null);
	}

	function testCast64() {
		var i:Int64 = cast null;
		#if static
		#if !cppia // TODO
		t(i == 0);
		#end
		t(i == cast null);
		#else
		f(i == 0);
		t(i == null);
		#end
	}

	function testPropagation64() {
		var Null:Null<Int64> = null;
		var i:Int64 = Null;
		#if static
		t(i == 0);
		f(i == Null);
		#else
		f(i == 0);
		t(i == null);
		#end
	}

	function testPropagationNullable64() {
		var Null:Null<Int64> = null;
		var i:Null<Int64> = Null;
		f(i == 0);
		t(i == null);
	}
}
