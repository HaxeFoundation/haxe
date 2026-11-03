package unit.issues;

typedef Issue8067Data = {
	final foo:String;
	final bar:{
		final foobar:String;
	}
}

class Issue8067 extends Test {
	function test() {
		eq("foo:final,bar:final,foobar:final", m());
	}

	static macro function m() {
		var fields = [];
		function loop(ct:haxe.macro.Expr.ComplexType) {
			switch ct {
				case TAnonymous(cfs):
					for (cf in cfs) {
						fields.push(cf.name + ":" + (cf.access.contains(AFinal) ? "final" : "var"));
						switch cf.kind {
							case FVar(t, _) | FProp(_, _, t, _) if (t != null):
								loop(t);
							case _:
						}
					}
				case _:
			}
		}
		var t = haxe.macro.Context.follow(haxe.macro.Context.getType("unit.issues.Issue8067.Issue8067Data"));
		loop(haxe.macro.TypeTools.toComplexType(t));
		return macro $v{fields.join(",")};
	}
}
