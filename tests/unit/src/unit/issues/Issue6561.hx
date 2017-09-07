package unit.issues;

private enum Log<A> {
	Log(msg:String):Log<String>;
}


class Issue6561 extends unit.Test {
	function test() {
		eq("hello", apply(Log("hello")));
	}

  	static function apply<A>(f:Log<A>):A {
		return switch f {
			case Log(msg):
      			msg;
		}
  	}
}