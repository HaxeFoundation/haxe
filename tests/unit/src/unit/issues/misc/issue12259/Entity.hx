package unit.issues.misc.issue12259;

class Entity {
	function new() {}

	var foo(get, set):Bool;

	function get_foo():Bool
		return true;

	function set_foo(b : Bool):Bool
		return true;
}
