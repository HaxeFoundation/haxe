import rust.NativeArray;
@:final @:nativeGen @:coreApi class Array<T> implements ArrayAccess<T> {
	public var length(default, null):Int;
	private var a:NativeArray<T>;
	public function new() {
		this.length = 0;
		this.a = new NativeArray<T>(8);
	}
	public function concat(o:Array<T>):Array<T> {
		var n = new Array<T>();
		n.a = untyped this.a + o.a;
		return n;
	}
}