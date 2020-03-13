package unit.issues;

class Issue9202 extends unit.Test {
	public function test() {
		var path = getDotPath();
		eq("hi", path.first);
	}

	static function getDotPath() {
		return {first: "hi"}
	}
}

private typedef Separated<T> = {first:T}
