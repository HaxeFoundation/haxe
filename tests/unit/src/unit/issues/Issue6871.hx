package unit.issues;

class Issue6871 extends unit.Test {
	static inline var GETTER_ERROR = 'getter exception';
	static inline var SETTER_ERROR = 'setter exception';

	@:keep static public var field(get,set):Int;
	static function get_field():Int throw GETTER_ERROR;
	static function set_field(v:Int):Int throw SETTER_ERROR;

	function test() {
		inline function exception(e:Dynamic) {
			return #if cs e.InnerException.Message #else e #end;
		}

		try {
			Reflect.getProperty(Issue6871, 'field');
			t(false);
		} catch(e:Dynamic) {
			eq(GETTER_ERROR, e);
		}

		try {
			Reflect.setProperty(Issue6871, 'field', 123);
			t(false);
		} catch(e:Dynamic) {
			eq(SETTER_ERROR, e);
		}
	}
}