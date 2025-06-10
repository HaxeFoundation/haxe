typedef BadType = {
	// should error but not yet
	var foo(private get, private set):Int;
	var foo2(never, never):Int;
}

typedef PrivateVariationsType = {
	var privateBoth(private get, private set):Int;
	var privateGetSet(private get, set):Int;
	var getPrivateSet(get, private set):Int;
	var defaultPrivateSet(default, private set):Int;
	var getNever(get, never):Int;
}

class PrivateVariations {
	public function new() {}
	var privateBoth(private get, private set):Int;
	public var privateGetSet(private get, set):Int;
	public var getPrivateSet(get, private set):Int;
	public var defaultPrivateSet(default, private set):Int;

	static var sprivateBoth(private get, private set):Int;
	static public var sprivateGetSet(private get, set):Int;
	static public var sgetPrivateSet(get, private set):Int;
	static public var sdefaultPrivateSet(default, private set):Int;

	function set_privateBoth(v):Int return 0;
	function get_privateBoth():Int return 0;
	function set_privateGetSet(v):Int return 0;
	function get_privateGetSet():Int return 0;
	function set_getPrivateSet(v):Int return 0;
	function get_getPrivateSet():Int return 0;
	function set_defaultPrivateSet(v):Int return 0;

	static function set_sprivateBoth(v):Int return 0;
	static function get_sprivateBoth():Int return 0;
	static function set_sprivateGetSet(v):Int return 0;
	static function get_sprivateGetSet():Int return 0;
	static function set_sgetPrivateSet(v):Int return 0;
	static function get_sgetPrivateSet():Int return 0;
	static function set_sdefaultPrivateSet(v):Int return 0;
}

class CheckVariations {
	static function main() {
		final type:PrivateVariationsType = null;
		type.getNever = 1; // err
		type.getPrivateSet;
		type.getPrivateSet = 1; // err
		type.privateGetSet; // err
		type.privateGetSet = 1;
		type.privateBoth; // err
		type.privateBoth = 1; // err
		@:privateAccess type.privateBoth;
		@:privateAccess type.privateBoth = 1;
		@:bypassAccessor type.privateBoth; // err
		@:bypassAccessor type.privateBoth = 1; // err

		type.defaultPrivateSet;
		type.defaultPrivateSet = 1; // err
		@:privateAccess type.defaultPrivateSet = 1;
		@:bypassAccessor type.defaultPrivateSet = 1;

		final vars = new PrivateVariations();
		vars.getPrivateSet;
		vars.getPrivateSet = 1; // err
		vars.privateGetSet; // err
		vars.privateGetSet = 1;
		vars.privateBoth; // err
		vars.privateBoth = 1; // err
		@:privateAccess vars.privateBoth;
		@:privateAccess vars.privateBoth = 1;
		vars.defaultPrivateSet;
		vars.defaultPrivateSet = 1; // err
		@:privateAccess vars.defaultPrivateSet = 1;
		@:bypassAccessor vars.defaultPrivateSet = 1;

		PrivateVariations.sgetPrivateSet;
		PrivateVariations.sgetPrivateSet = 1; // err
		PrivateVariations.sprivateGetSet; // err
		PrivateVariations.sprivateGetSet = 1;
		PrivateVariations.sprivateBoth; // err
		PrivateVariations.sprivateBoth = 1; // err
		@:privateAccess PrivateVariations.sprivateBoth;
		@:privateAccess PrivateVariations.sprivateBoth = 1;
		PrivateVariations.sdefaultPrivateSet;
		PrivateVariations.sdefaultPrivateSet = 1; // err
		@:privateAccess PrivateVariations.sdefaultPrivateSet = 1;
		@:bypassAccessor PrivateVariations.sdefaultPrivateSet = 1;
	}
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
