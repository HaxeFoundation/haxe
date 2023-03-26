package haxe.ds;

private typedef VectorData<T> = java.NativeArray<T>

abstract Vector<T>(VectorData<T>) {
	extern overload public inline function new(length:Int) {
		this = new java.NativeArray(length);
	}

	extern overload public inline function new(length:Int, defaultValue:T) {
		this = new java.NativeArray(length);
		// uncomment and rebuild to break compilation server
		// server restart will fix errors until the next change in this file

		// trace("uncomment me");
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
