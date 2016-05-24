package unit.issues;

class Issue5226 extends unit.Test {
	function test() {
		var from:HasFrom = 10;
		eq("20", from.get());
		var f:HasField = {};
		f.hasFrom = 20;
		eq("30", f.hasFrom.get());
	}
}

private abstract HasFrom(String) from String {
	@:from public static function fromInt(i:Int):HasFrom {
		return (i + 10) + '';
	}

	public function get() {
		return this;
	}
}

private abstract HasField(Dynamic) from Dynamic {
	public var hasFrom(get,set):HasFrom;

	private function get_hasFrom():HasFrom {
		return this.hasFrom;
	}

	private function set_hasFrom(f:HasFrom):HasFrom {
		return this.hasFrom = f;
	}
}