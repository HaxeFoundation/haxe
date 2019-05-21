package unit.issues;

class Issue7671 extends unit.Test {
	function test() {
		var d = new Dummy();

		eq("getter", d.getterInstance);
		eq("getter", Reflect.getProperty(d, "getterInstance"));

		eq("getter", Dummy.getterStatic);
		eq("getter", Reflect.getProperty(Dummy, "getterStatic"));

		d.setterInstance = "hello";
		eq("hello", d.instanceValue);
		Reflect.setProperty(d, "setterInstance", "world");
		eq("world", d.instanceValue);

		Dummy.setterStatic = "hello";
		eq("hello", Dummy.staticValue);
		Reflect.setProperty(Dummy, "setterStatic", "world");
		eq("world", Dummy.staticValue);
	}
}

private class Dummy {
	static public var getterStatic (get,null):String;
	static function get_getterStatic() {
		return "getter";
	}

	public var getterInstance (get,null):String;
	function get_getterInstance() {
		return "getter";
	}

	static public var setterStatic (null,set):String;
	static public var staticValue:String;
	static function set_setterStatic(v:String):String {
		return staticValue = v;
	}

	public var setterInstance (null,set):String;
	public var instanceValue:String;
	function set_setterInstance(v:String) {
		return instanceValue = v;
	}

	public function new(){}
}