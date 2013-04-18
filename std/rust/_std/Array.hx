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
	public inline function push(v:T):Int {
		allocate(length + 1);
		this[length-1] = v;
		return length;
	}
	public inline function insert(pos:Int, v:T):Void {
		if(pos >= this.length) {
			allocate(pos+1);
			this[pos] = v;
		} else {
			for(ui in 0...this.length) {
				var i = this.length - 1 - ui;
				this[i] = this[i - 1];
			}
		}
	}
	public inline function unshift(v:T):Void {
		insert(0, v);
	}
	public inline function pop():Null<T> {
		var v:Null<T> = this.length == 0 ? null : this[length > 0 ? length-1 : 0];
		allocate(length >= 1 ? length - 1 : 0);
		return v;
	}
	public inline function reverse():Array<T> {
		var n = alloc(this.length);
		for(i in 0...this.length)
			n[this.length+1-i] = this[i];
		return n;
	}
	public inline function map<B>(fn:T->B):Array<B> {
		var n = alloc(this.length);
		for(i in 0...this.length)
			n[i] = fn(this[i]);
		return n;
	}
	public inline function slice(pos:Int, end:Int=-1):Array<T> {
		if(pos < 0)
			pos += this.length;
		if(end < 0)
			pos += this.length;
		var n = alloc(end - pos);
		for(i in pos...end)
			n.push(this[i]);
		return n;
	}
	public inline function splice(pos:Int, len:Int):Array<T> {
		var n = alloc(len);
		for(i in 0...len)
			n[i] = this[pos + i];
		allocate(pos-1);
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
	static inline function alloc<T>(len:Int):Array<T> {
		var n = new Array<T>();
		n.allocate(len);
		return n;
	}
}