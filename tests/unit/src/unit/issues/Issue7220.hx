package unit.issues;

private enum Expr {
	Const(const:Int);
}

class Issue7220 extends unit.Test {
	function test() {
		eq(extract(Const(15)), 15);
	}

	static function extract(e:Expr):Int {
		switch e {
			case Const(const):
				return const;
		}
	}
}

