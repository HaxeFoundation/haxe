package haxe.ds;

private typedef VectorData<T> = java.NativeArray<T>

abstract Vector<T>(VectorData<T>) {
	public var length(get, never):Int;

	inline function get_length():Int {
		return this.length;
	}

	extern overload public inline function new(length:Int) {
		this = new java.NativeArray(length);
	}

	extern overload public inline function new(length:Int, defaultValue:T) {
		this = new java.NativeArray(length);
		// uncomment and rebuild to break compilation server
		// server restart will fix errors until the next change in this file

		// trace("uncomment me");
	}

	@:op([]) public inline function get(index:Int):T {
		return this[index];
	}

	@:op([]) public inline function set(index:Int, val:T):T {
		return this[index] = val;
	}

	public static function blit<T>(src:Vector<T>, srcPos:Int, dest:Vector<T>, destPos:Int, len:Int):Void {}

	static public function fromData<T>(data:VectorData<T>):Vector<T>
		return cast data;

	public function copy<T>():Vector<T> {
		return cast this;
	}

	public function toData():VectorData<T>
		return cast this;
}
