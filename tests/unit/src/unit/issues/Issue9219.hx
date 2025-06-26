package unit.issues;

import unit.Test;

private class NotMain implements Hashable {
	public function new() { }
}

class Issue9219 extends Test {
	public function test() {
		var hs:Set<NotMain> = new HashSet();
		t(hs.remove(new NotMain()));
	}
}

private interface Hashable {}

private interface Collection<T> {
	function remove(val:T):Bool;
}

private interface Set<T> extends Collection<T> {}

private class HashSet<T:Hashable> implements Set<T> {
	public function new() {}

	public function remove(val:T):Bool {
		return true;
	}
}