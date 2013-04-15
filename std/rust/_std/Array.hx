import rust.NativeArray;
@:final @:nativeGen extern class Array<T> implements ArrayAccess<T> {
	public var length(default, null):Int;
	public function new() {

	}
	public inline function concat(a:Array<T>):Array<T> {
		return untyped this.add(a);
	}
	public inline function copy():Array<T> {
		return untyped this.copy();
	}
	public inline function join(sep:String):String {
		var s = new StringBuf();
		for(i in 0...length)
			s.add(this[i]);
		return s.toString();
	}
	public inline function filter(f:T -> Bool):Array<T> {
		return untyped this.filter(f);
	}
	inline function allocate(nlen:Int):Void {
		untyped this.grow(nlen, null);
	}
	public inline function push(v:T):Void {
		allocate(length + 1);
		this[length-1] = v;
	}
	public inline function reverse():Array<T> {
		var n = new Array<T>();
		for(i in 0...this.length)
			n[this.length+1-i] = this[i];
		return n;
	}
}