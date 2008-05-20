package unit;

class MyClass {

	var val : Int;

	public var ref : MyClass;
	public var intValue : Int;
	public var stringValue : String;

	public function new(v) {
		val = v;
	}

	public function get() {
		return val;
	}

	public function set(v) {
		val = v;
	}

}