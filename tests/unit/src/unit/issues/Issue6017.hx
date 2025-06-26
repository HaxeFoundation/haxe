package unit.issues;

import utest.Assert;
import haxe.PosInfos;

class Issue6017 extends Test {
	function checkPos(i:Any, ?p:PosInfos) {
		Assert.same(p, Abstr.p);
	}

	function test() {
		checkPos((123:Abstr));
		var i:Abstr = 123;
		checkPos(i + 100);
		checkPos((i:String));
		checkPos(i[0]);
		checkPos(i[0] = 1);
		checkPos(i());
		checkPos(i.hello);
		checkPos(i.hello = 'world');
	}
}

private abstract Abstr(Int) {
	static public var p:PosInfos;

	@:from static public function fromInt(i:Int, ?p:PosInfos) {
		Abstr.p = p;
		return new Abstr(i);
	}

	@:to public function toString(?p:PosInfos):String {
		Abstr.p = p;
		return 'Abstr $this';
	}

	@:op(A + B) public function addInt(i:Int, ?p:PosInfos) {
		Abstr.p = p;
		return new Abstr(i + this);
	}

	@:op([]) public function arrayGet(i:Int, ?p:PosInfos) {
		Abstr.p = p;
		return i;
	}

	@:op([]) public function arraySet(i:Int, v:Int, ?p:PosInfos) {
		Abstr.p = p;
		return v;
	}

	@:op(a()) public function call(?p:PosInfos) {
		Abstr.p = p;
		return Std.random(10);
	}

	@:op(a.b) public function resolveGet(f:String, ?p:PosInfos) {
		Abstr.p = p;
		return f;
	}

	@:op(a.b) public function resolveSet(f:String, v:String, ?p:PosInfos) {
		Abstr.p = p;
		return v;
	}

	function new(i:Int) {
		this = i;
	}
}