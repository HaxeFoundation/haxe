package issues;

class Parent {
	public var x:Int;
	public var y:String;

	public inline function new(x:Int, y:String) {
		trace("Parent.new: Before assign");
		this.x = x;
		this.y = y;
		trace("Parent.new: After assign");
	}
}

class Child extends Parent {

	public var z:Int;

	public inline function new(x:Int, y:Int, z:Int) {
		trace("Child.new: Before super");
		super(x, Std.string(y));
		trace("Child.new: After super");
		this.z = z;
		trace("Child new: After assign");
	}
}

class Issue4690 {
	@:js('
		console.log("Child.new: Before super");
		console.log("Parent.new: Before assign");
		var c_x = 1;
		var c_y = "" + 2;
		console.log("Parent.new: After assign");
		console.log("Child.new: After super");
		var c_z = 3;
		console.log("Child new: After assign");
		console.log(c_x);
		console.log(c_y);
		console.log(c_z);
	')
	@:analyzer(no_const_propagation, no_fusion)
	static function test() {
		var c = new Child(1, 2, 3);
		trace(c.x);
		trace(c.y);
		trace(c.z);
	}
}