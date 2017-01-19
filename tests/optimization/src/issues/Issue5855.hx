package issues;

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
		console.log(c_field);
	')
	@:analyzer(ignore)
	static function main() {
		var c = new C();
		trace(c.field);
	}
}