package neko.db;

class ResultSet implements Iterator<Dynamic> {

	public var length : Int;
	public var nfields : Int;
	private var __r : Void;
	private var cache : Dynamic;

	private function new(r) {
		__r = r;
		length = result_get_length(r);
		nfields = result_get_nfields(r);
	}

	public function hasNext() {
		if( cache == null )
			cache = next();
		return (cache != null);
	}

	public function next() : Dynamic {
		var c = cache;
		if( c != null ) {
			cache = null;
			return c;
		}
		c = result_next(__r);
		if( c == null )
			return null;
		untyped {
			var f = __dollar__objfields(c);
			var i = 0;
			var l = __dollar__asize(f);
			while( i < l ) {
				var v = __dollar__objget(c,f[i]);
				if( __dollar__typeof(v) == __dollar__tstring )
					__dollar__objset(c,f[i],new String(v));
				i = i + 1;
			}
		}
		return c;
	}

	public function getResult( n : Int ) {
		return new String(result_get(__r,n));
	}

	public function getIntResult( n : Int ) : Int {
		return result_get_int(__r,n);
	}

	public function getFloatResult( n : Int ) : Float {
		return result_get_float(__r,n);
	}

	private static var result_get_length = neko.Lib.load("mysql","result_get_length",1);
	private static var result_get_nfields = neko.Lib.load("mysql","result_get_nfields",1);
	private static var result_next = neko.Lib.load("mysql","result_next",1);
	private static var result_get = neko.Lib.load("mysql","result_get",2);
	private static var result_get_int = neko.Lib.load("mysql","result_get_int",2);
	private static var result_get_float = neko.Lib.load("mysql","result_get_float",2);
	private static var result_set_conv_date = neko.Lib.load("mysql","result_set_conv_date",2);

}
