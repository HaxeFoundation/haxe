package unit.issues;

class Issue7539 extends Test {
	var foo:Array<String>->Void;

	var result:Array<String>;

	public function setup() {
		result = [];
		foo = if (false) null else function(args:Array<String>) {
				result.push(args.join(","));
				args.push("foo");
				result.push(args.join(","));
			}
	}

	function test() {
		setup();
		var args:Array<String> = [];
		args.push("bar");
		foo(args);
		eq(result[0], "bar");
		eq(result[1], "bar,foo");
	}
}
