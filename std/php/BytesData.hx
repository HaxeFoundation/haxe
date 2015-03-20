package php;



private class Wrapper {
	public var s : NativeString;
	public inline function new (s:NativeString) {
		this.s = s;
	}
}

abstract BytesData(Wrapper) {

	inline function new (x:Wrapper) this = x;

	inline function str ():php.NativeString return this.s;

	inline function setNativeString (val:NativeString):Void {
		this.s = val;
	}

	inline function get_length ():Int {
		return untyped __call__("strlen", str());
	}

	static inline function wrap (s:NativeString):Wrapper {
		return new Wrapper(s);
	}

	static inline function ofNativeString (s:NativeString) {
		return new BytesData( wrap(s));
	}

	public inline function set (index:Int, val:Int):Void {
		untyped __php__("{0}->s[{1}] = chr({2})", this, index, val);
	}

	public var length(get, never):Int;

	public inline function compare (other:BytesData):Int {
		return untyped __php__("{0} < {1} ? -1 : ({0} == {1} ? 0 : 1)", str(), other.str());
	}

	public inline function get (pos:Int):Int {
		return untyped __call__("ord", str()[pos]);
	}

	public inline function copy ():BytesData {
		return ofNativeString(str());
	}

	public inline function getString (pos:Int, len:Int):String {
		return untyped __call__("substr", str(), pos, len);
	}

	public inline function sub (pos:Int, len:Int):BytesData {
		return ofString(untyped __call__("substr", str(), pos, len));
	}

	public inline function blit (pos : Int, src : BytesData, srcpos : Int, len : Int):Void {
		setNativeString(untyped __php__("substr({0}, 0, {2}) . substr({1}, {3}, {4}) . substr({0}, {2}+{4})", str(), src.str(), pos, srcpos, len));
	}

	public inline function toString():String return cast str();

	public static inline function ofString (s:String) {
		return ofNativeString(cast s);
	}

	public static inline function alloc (length:Int) {
		return ofNativeString(untyped __call__("str_repeat", __call__("chr", 0), length));
	}
}