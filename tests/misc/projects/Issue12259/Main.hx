@:access(Element)
class Main {
	public static function main() {
		new Element().foo;
	}
}

class Element extends Entity {}

class Entity {
	function new() {}

	var foo(get, never):Bool;
	function get_foo():Bool return true;
}
