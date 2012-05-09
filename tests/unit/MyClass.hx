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

class MyChild2 extends MyParent {
	
}