package hxcoro.ds;

private abstract OutData<T>(Array<T>) from Array<T> {
	public inline function new() {
		this = [];
	}

	public inline function set(v:T):Void {
		this[0] = v;
	}

	public inline function get() {
		return this[0];
	}
}

abstract Out<T>(OutData<T>) {
	public inline function new() {
		this = new OutData();
	}

	public inline function get() {
		return this.get();
	}

	public inline function set(v:T) {
		this.set(v);
	}
}