package unit.issues;

@:forward
private abstract OneOrMany<T>(Array<T>) from Array<T> to Array<T> {
	@:from
	static function fromSingle<T>(value:T):OneOrMany<T> {
		return [value];
	}
}

class Issue6071 extends unit.Test {
	function test() {
		eq("ab", getFirstString(["ab", "cd"]));
		eq(null, getFirstString([]));
	}

	static inline function firstItem<T>(items:Array<T>):T {
		return (items == null || items.length == 0) ? null : items[0];
	}

	static function getFirstString(strings:OneOrMany<String>) {
		return firstItem(strings);
	}
}
