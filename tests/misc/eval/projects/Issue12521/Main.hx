class PhysicalParent {
	public var x:Int;
	public function new() {}
}

class PhysicalPropertyParent {
	@:isVar public var x(get, set):Int;
	public function new() {}
	function get_x() return x;
	function set_x(v) return x = v;
}

class RedefinePhysical extends PhysicalParent {
	public var x:Int;
}

class RedefinePhysicalProperty extends PhysicalPropertyParent {
	@:isVar public var x(get, set):Int;
}

class RedefinePhysicalPropAsProperty extends PhysicalPropertyParent {
	public var x(get, set):Int;
}

class RedefinePhysicalAsProperty extends PhysicalParent {
	public var x(get, set):Int;
	function get_x() return 0;
	function set_x(v) return v;
}

class NonPhysicalParent {
	public var x(get, set):Int;
	public function new() {}
	function get_x() return 0;
	function set_x(v) return v;
}

class NarrowRead extends NonPhysicalParent {
	public var x(never, set):Int;
}

class NarrowWrite extends NonPhysicalParent {
	public var x(get, never):Int;
}

class NarrowWrite2 extends NonPhysicalParent {
	public var x(get, private set):Int;
}

class PrivateVar extends NonPhysicalParent {
	var x(get, set):Int;
}

function main() {}
