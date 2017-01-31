package issues;

class Issue5931 {
	inline function something(arg:Int, func) {
		var func1 = func;
		func1(arg);
	}

	function callback(arg:Int) {
		trace('here1');
	}

	inline function new() {
		something(1, callback);
	}

	@:js('
		new issues_Issue5931();
	')
	static public function main() {
		var x = new Issue5931();
	}
}