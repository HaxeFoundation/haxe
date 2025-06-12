typedef BadType = {
	// should error but not yet
	var foo(private get, private set):Int;
	var foo2(never, never):Int;
}

typedef FooPrivateGetType = {
	var foo(private get, set):Int;
}

typedef FooType = {
	var foo(get, set):Int;
}

class Main {
	static function main() {
		final main = new Main();
		main.foo = 1;

		var privateObj:FooPrivateGetType = main;
		var obj:FooType = main; // err, should be allowed?

		privateObj = obj;
		obj = privateObj; // err
	}

	public var foo(private get, set):Int;

	function get_foo():Int {
		return 0;
	}

	function set_foo(v) {
		return v;
	}

	public var notAllowed(private get, private set):Int; // err
	function set_notAllowed(v):Int return 0;
	function get_notAllowed():Int return 0;

	public function new() {
		foo = 1;
		foo;

		@:bypassAccessor Rect.staticFoo = 1;
		@:privateAccess Rect.staticFoo = 1;
		Rect.staticFoo = 1; // err

		final rect = new Rect();
		rect.width = 1; // err
		rect.width;

		final shape:Shape = rect;
		shape.width = 1;
		shape.width; // err

		final bar = new Bar();

		bar.defaultPrivateSet = 1; // err
		@:bypassAccessor bar.defaultPrivateSet = 1;
		@:privateAccess bar.defaultPrivateSet = 1;

		@:privateAccess bar.width = 1;
		bar.width = 1; // err
		bar.width; // err

		bar.defaultNull = 1; // err

		bar.age;
		@:bypassAccessor bar.age = 1;
		@:privateAccess bar.age = 1;
		bar.age = 1; // err

		final child = new Child();
		@:privateAccess child.width = 1;
	}
}

interface Shape {
	var width(private get, set):Int;
}

interface PublicShape {
	var width(get, set):Int;
}

class PrivateRect implements PublicShape {
	public var width(get, private set):Int; // err

	function set_width(value:Int):Int {
		return value;
	}

	function get_width():Int {
		return 0;
	}
}

class Rect implements Shape {
	public static var staticFoo(default, private set):Int = 0;
	static function set_staticFoo(v) {
		return v;
	}

	public function new() {}
	public var width(get, private set):Int; // err
	function set_width(v):Int return 0;
	function get_width():Int return 0;
}

@:build(PropertyMacro.addIntProperty("age"))
class Bar {
	public function new() {
		width = 2;
	}

	public var defaultNull(default, null):Int;

	public var defaultPrivateSet(default, private set):Int;
	function set_defaultPrivateSet(v):Int return v;

	var width(private get, private set):Int;

	function set_width(value:Int):Int {
		return value;
	}

	function get_width():Int {
		return 0;
	}
}

class Parent {
	var width(private get, private set):Int;
	function set_width(v):Int return 0;
	function get_width():Int return 0;
}

class Child extends Parent {
	public function new() {
		width = 0;
		super.width;
		super.width = 0;
	}
}

@:access(Element)
class MainElement {
	public static function main() {
		new Element().foo;
		new Element().fooSet = false;
	}
}

class Element extends Entity {}

class Entity {
	function new() {}

	var foo(private get, never):Bool;

	function get_foo():Bool
		return true;

	var fooSet(default, private set):Bool;

	function set_fooSet(v):Bool
		return fooSet = v;
}
