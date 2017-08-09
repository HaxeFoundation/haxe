package issues;

import TestJs.use;

class C {
	public var field:String;
	public inline function new() {
		if (Math.random() > 0.5) {
			field = "foo";
		}
	}
}

class Issue5855 {
	@:js('
		var c_field;
		if(Math.random() > 0.5) {
			c_field = "foo";
		}
		TestJs.use(c_field);
	')
	@:analyzer(ignore)
	static function main() {
		var c = new C();
		use(c.field);
	}
}