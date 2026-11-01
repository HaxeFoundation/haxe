package unit.issues;

import haxe.macro.Context;
import haxe.macro.Expr;
import utest.Assert;

@:structInit
private class Student {
	public final name:String;
	public final score:Int;
}

private abstract A(String) {
	public function new() {
		this = "";
	}

	@:arrayAccess public function arrayWrite(k:String, v:Array<Dynamic>) {}
}

private abstract MultiSetter(Array<String>) {
	public function new() {
		this = [];
	}

	@:arrayAccess public function setInt(k:String, v:Int):Int {
		this.push('$k:int:$v');
		return v;
	}

	@:arrayAccess public function setString(k:String, v:String):String {
		this.push('$k:str:$v');
		return v;
	}

	public function toString():String {
		return this.join(",");
	}
}

private abstract GenericSetter(Array<String>) {
	public function new() {
		this = [];
	}

	@:arrayAccess public function set<T>(k:String, v:Array<T>):Array<T> {
		this.push('$k:${v.length}');
		return v;
	}

	public function toString():String {
		return this.join(",");
	}
}

private abstract FailKey(String) {
	@:from static macro function fromInt(e:ExprOf<Int>):ExprOf<FailKey> {
		Context.error("Unreached setter macro was executed!", e.pos);
		return macro cast "";
	}
}

private abstract MultiMacro(Array<String>) {
	public function new() {
		this = [];
	}

	@:arrayAccess public function setInt(k:Int, v:Int):Int {
		this.push('int:$k=$v');
		return v;
	}

	@:arrayAccess public function setMacro(k:FailKey, v:String):String {
		this.push('macro:$k=$v');
		return v;
	}

	public function toString():String {
		return this.join(",");
	}
}

private abstract MonoKeyPolluteRead(Array<String>) {
	public function new() {
		this = ["hello"];
	}

	@:arrayAccess public function get1<T>(k:Array<T>):T {
		return cast this[0];
	}

	@:arrayAccess public function get2(k:Array<String>):Int {
		return this.length;
	}
}

private abstract MonoKeyPolluteWrite(Array<String>) {
	public function new() {
		this = [];
	}

	@:arrayAccess public function set1<T>(k:Array<T>, v:T):T {
		this.push('set1:${k.length}=$v');
		return v;
	}

	@:arrayAccess public function set2(k:Array<String>, v:Int):Int {
		this.push('set2:${k.length}=$v');
		return v;
	}

	public function toString():String {
		return this.join(",");
	}
}

class Issue10655 extends Test {
	function test() {
		var map = new Map<String, Student>();

		var student1:Student = {name: "Boris", score: 20};
		map["the best"] = student1;

		map["the worst"] = {name: "Vianney", score: -1};
		Assert.pass();
	}

	function test2() {
		var a = new A();
		a["foo"] = [1, "foo"];
		Assert.pass();
	}

	function testMultiSetter() {
		var m = new MultiSetter();
		m["a"] = 42;
		m["b"] = "hello";
		eq("a:int:42,b:str:hello", m.toString());
	}

	function testGenericSetter() {
		var g = new GenericSetter();
		g["x"] = [];
		g["y"] = [1, 2, 3];
		eq("x:0,y:3", g.toString());
	}

	function testUnreachedMacroKey() {
		var m = new MultiMacro();
		m[1] = 42;
		eq("int:1=42", m.toString());
	}

	function testMonoKeyPolluteRead() {
		var rp = new MonoKeyPolluteRead();
		var arr = [];
		var v = rp[arr];
		arr.push(1);
		eq(1, arr[0]);
	}

	function testMonoKeyPollute() {
		var mp = new MonoKeyPolluteWrite();
		var arr = [];
		mp[arr] = 42;
		arr.push(1);
		eq(1, arr[0]);
		eq("set1:0=42", mp.toString());
	}
}
