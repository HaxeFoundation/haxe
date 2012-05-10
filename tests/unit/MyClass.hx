package unit;

class MyClass {

	#if as3 public #end var val : Int;

	public var ref : MyClass;
	public var intValue : Int;
	public var stringValue : String;

	public function new(v) {
		val = v;
		intValue = 55;
	}

	public function get() {
		return val;
	}

	public function set(v) {
		val = v;
	}

	public function add(x,y) {
		return val + x + y;
	}
}

class MyParent {
	public function new() { }
	function a() return 11
}

class MyChild1 extends MyParent {
	public override function a() { return 12; }
}

interface I1 { }
class Base { public var s:String; public function new() { } }
class Child1 extends Base { public function new() { super(); } }
class Child2 extends Base, implements I1 { public function new() { super(); } }
class Child2_1 extends Child2 { public function new() { super(); } }
class Unrelated implements I1 { public var s:String; public var t:Int;  public function new() { } }

interface I2 implements I1 { }
class ClassI2 implements I2 { public function new() { } }

class CI1 extends Base, implements I1 { public function new() { super(); } }
class CI2 extends Base, implements I1 { public function new() { super(); } }
class CII1 extends CI1 { public function new() { super(); } }
class CII2 extends CI2 { public function new() { super(); } }

class PClassBase<T> { public function new() {  } }
class PClass1<T> extends PClassBase<Float> { public function new() { super(); } }
class PClass2<T> extends PClassBase<T> { public function new(t:T) { super(); } }