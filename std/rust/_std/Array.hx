/** Implemented as an OwnedVector, or ~[T] */
@:final @:nativeGen extern class Array<T> implements ArrayAccess<T> {
	public var length(default, null):Int;
	public function new() {

	}
	public static inline function of<T>(i:rust.Iterator<T>):Array<T> {
		var arr = new Array<T>();
		while(true) {
			var n = i.next();
			if(n == null)
				break;
			arr.push(n);
		}
		return arr;
	}
	public static inline function ofFunc<T>(func:(T -> Bool)->Void):Array<T> {
		var a = new Array<T>();
		func(function(v:T) {
			a.push(v);
			return true;
		});
		return a;
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
	public function filter(f:T -> Bool):Array<T> {
		return [];
	}
	public inline function push(v:T):Int {
		untyped this.push(v);
		return this.length;
	}
	public function insert(pos:Int, v:T):Void {
	}
	public inline function unshift(v:T):Void {
		insert(0, v);
	}
	public inline function pop():Null<T> {
		return try untyped this.pop() catch(v:Dynamic) null;
	}
	public inline function reverse():Array<T> {
		var n = [];
		for(i in 0...this.length)
			n[this.length+1-i] = this[i];
		return n;
	}
	public inline function map<B>(fn:T->B):Array<B> {
		var n = [];
		for(i in 0...this.length)
			n[i] = fn(this[i]);
		return n;
	}
	public inline function slice(pos:Int, end:Int=-1):Array<T> {
		if(pos < 0)
			pos += this.length;
		if(end < 0)
			pos += this.length;
		var n = [];
		for(i in pos...end)
			n.push(this[i]);
		return n;
	}
	public inline function splice(pos:Int, len:Int):Array<T> {
		var n = [];
		for(i in 0...len)
			n[i] = this[pos + i];
		return n;
	}
	public function shift():Null<T> {
		var v = this[0];
		for(i in 0...this.length-1)
			this[i] = this[i+1];
		return v;
	}
	public inline function toString():String {
		return "[ "+this.join(", ")+" ]";
	}
	@:functionCode('std::sort::quick_sort(self, fn)')
	public function sort(fn:T->T->Int):Array<T> {
		return null;
	}
	public function remove(v:T):Bool {
		return false;
	}
	public function iterator():Iterator<T> {
		return null;
	}
	public static inline function alloc<T>(len:Int):Array<T> {
		return [];
	}
}