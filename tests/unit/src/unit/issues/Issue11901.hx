package unit.issues;

private class Issue11901Helper {
	public var func:(Dynamic, Dynamic) -> Void;

	public function new(obj:Dynamic) {
		this.func = obj.test;
	}

	public function call(k:Dynamic, v:Dynamic) {
		func(k, v);
	}
}

class Issue11901 extends Test {
	function test() {
		var results:Array<String> = [];
		var a = new Issue11901Helper({
			test: function(k:Dynamic, v:Dynamic) {
				results.push('$k,$v');
			}
		});
		a.func("a", 1);
		a.call("b", 2);
		eq(results[0], "a,1");
		eq(results[1], "b,2");
	}
}
