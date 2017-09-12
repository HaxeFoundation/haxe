package unit.issues;

@:forward
private abstract A({i:String}) {
	public function new(i) this = i;
}

class Issue6532 extends unit.Test {
	function test() {
		var a = new A({ i: "foo" });
        var x = switch a {
            case {i: extracted}: extracted;
        }
		eq("foo", x);
	}
}