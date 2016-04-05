package unit.issues;

class Issue4360<T> extends Test {
	public function test() {
		//this fails
		var t : Abstr<Item> = new Abstr();
		//but this works
		// var t : Abstr<Item> = new Abstr(new Test());
		eq(t, null);
	}
}

private abstract Abstr<T>(Issue4360<T>) to Issue4360<T> {
	public function new (inst:Issue4360<T> = null) : Void {
		this = inst;
	}
}

private class Item {
    public function new () : Void { }
}
