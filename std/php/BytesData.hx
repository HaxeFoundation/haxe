package php;

class BytesDataWrapper {
	public var s : php.NativeString;
	public inline function new (s:php.NativeString) { this.s = s;}
}

abstract BytesData(BytesDataWrapper) {

	inline function new (x:BytesDataWrapper) this = x;

	inline function str () return this.s;
	inline function raw () return this;

	public var length(get, never):Int;

	public inline function compare (other:BytesData):Int {
		var x = str();
		var y = other.raw().s;
		return untyped __php__("$x < $y ? -1 : ($x == $y ? 0 : 1)");
	}

	public inline function get_length ():Int {
		return untyped __call__("strlen", str());
	}

	public inline function get (pos:Int):Int {
		return untyped __call__("ord", str()[pos]);
	}

	public inline function copy ():BytesData {
		return new BytesData(new BytesDataWrapper(this.s));
	}

	public  function getString (pos:Int, len:Int):String {
		return untyped __call__("substr", str(), pos, len);
	}

	public  function sub (pos:Int, len:Int):BytesData {
		return ofString(untyped __call__("substr", str(), pos, len));
	}

	public function blit (pos : Int, src : BytesData, srcpos : Int, len : Int) {
		var x = str();
		var y = src.str();
		this.s = untyped __php__("substr($x, 0, $pos) . substr($y, $srcpos, $len) . substr($x, $pos+$len)");
	}

	public inline function toString():String return cast this.s;

	public function set (index:Int, val:Int):Void {
		var x = this;
		untyped __php__("$x->s[$index] = chr($val)");
	}



	public static inline function ofString (s:String) {
		return new BytesData( new BytesDataWrapper(cast s));
	}
	public static inline function alloc (length:Int) {
		return new BytesData(new BytesDataWrapper(untyped __call__("str_repeat", __call__("chr", 0), length)));
	}
}