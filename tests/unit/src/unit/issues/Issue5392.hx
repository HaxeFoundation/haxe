package unit.issues;

private abstract A(Int) from Int {
    public function size() {
        return switch (this) {
            case var size: size;
        }
    }
}

class Issue5392 extends unit.Test {
	function test() {
        var a = (10 : A);
        eq(10, a.size());
	}
}
