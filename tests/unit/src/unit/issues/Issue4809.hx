package unit.issues;

private abstract Iter<T>(Iterator<T>) from Iterator<T> to Iterator<T> {}

class Issue4809 extends Test {
	function test() {
		var list:Iter<Int> = [1, 2, 3].iterator();
		var acc = "";
		for (item in list) {
			acc += item;
		}
		eq("123", acc);
	}
}