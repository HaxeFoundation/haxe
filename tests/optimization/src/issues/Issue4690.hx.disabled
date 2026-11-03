package issues;

import TestJs.use;

class Parent {
	public var x:Int;
	public var y:String;

	public inline function new(x:Int, y:String) {
		use("Parent.new: Before assign");
		this.x = x;
		this.y = y;
		use("Parent.new: After assign");
	}
}

class Child extends Parent {

	public var z:Int;

	public inline function new(x:Int, y:Int, z:Int) {
		use("Child.new: Before super");
		super(x, Std.string(y));
		use("Child.new: After super");
		this.z = z;
		use("Child new: After assign");
	}
}

class Issue4690 {
	@:js('
		var c_z;
		var c_y;
		var c_x;
		TestJs.use("Child.new: Before super");
		TestJs.use("Parent.new: Before assign");
		c_x = 1;
		var c_y1 = false;
		if(c_y1) {
			c_y = "null";
		} else {
			c_y = "" + 2;
		}
		TestJs.use("Parent.new: After assign");
		TestJs.use("Child.new: After super");
		c_z = 3;
		TestJs.use("Child new: After assign");
		TestJs.use(c_x);
		TestJs.use(c_y);
		TestJs.use(c_z);
	')
	@:analyzer(no_const_propagation, no_fusion)
	static function test() {
		var c = new Child(1, 2, 3);
		use(c.x);
		use(c.y);
		use(c.z);
	}
}