package unit.issues;

private typedef Recursive<P> = Recursive<P>->P;
private typedef YDef<P, R> = Recursive<P->R>;

@:callable
private abstract Y<P, R>(YDef<P, R>) from YDef<P, R> to YDef<P, R> {
	@:noUsing static public function pure<P, R>(f:P->R):Y<P, R> {
		return function(fn:Recursive<P->R>) return f;
	}

	public function reply():P->R {
		return this(this);
	}
}

class Issue9695 extends unit.Test {
	static var problem:Recursive<String>;

	function test() {
		var y = Y.pure((i:Int) -> i + 1);
		eq(42, y.reply()(41));

		problem = function(r:Recursive<String>) return "ok";
		eq("ok", problem(problem));
	}
}
