package issues;

class Issue6715 {
	@:js('
		var f = function() {
			var x = 1;
		};
		issues_Issue6715.x = f;
		issues_Issue6715.x = f;
		issues_Issue6715.x = f;
		var x1 = 1;
		var x2 = 1;
		var x3 = 1;
	')
	@:analyzer(no_local_dce)
	static public function main() {
		insanity(function() var x = 1);
	}

	static var x;

	static inline function insanity(f:Void -> Void)
	{
		x = f;
		x = f;
		x = f;

		f();
		f();
		f();
	}
}