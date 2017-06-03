package unit.issues;

class Issue6299 extends unit.Test {
    public function test() {
        t(true);
    }
}

@:keep
enum Expr {
	EArray( e : Expr, index : Expr );
}
