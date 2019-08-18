package issues;

class Issue6672 {
	@:js('
		var o = new issues_Issue6672();
		null.set_prop(o);
	')
	@:analyzer(no_optimize)
	static function test() {
		(null:IFace).prop = init(new Issue6672(), function(o) {});
	}

	@:js('
		null.set_prop(new issues_Issue6672());
	')
	static function test2() {
		(null:IFace).prop = init(new Issue6672(), function(o) {});
	}

	public static inline function init(o:Issue6672, action:Issue6672->Void):Issue6672 {
		action(o);
		return o;
	}

	function new() {}
}

interface IFace {
	var prop(get, set):Issue6672;
}