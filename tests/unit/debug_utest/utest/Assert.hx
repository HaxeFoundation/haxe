package utest;

class Assert {

	public static var C = 0;

	public static macro function pass(?msg:String) {
		return macro utest.Assert.C++;
	}

	public static macro function equals(a,b, ?_) {
		return macro { var _a = $a, _b = $b; if( _a != _b ) throw "assert:"+_a+"!="+_b; utest.Assert.C++; }
	}

	public static macro function floatEquals(a, b, ?_) {
		return macro { var _a = $a, _b = $b; if ( Math.abs(_a - _b) > 1e-5 ) throw "assert:"+_a+"!="+_b; utest.Assert.C++; }
	}

	public static macro function same(a, b, ?_) {
		switch( haxe.macro.Context.typeof(a) ) {
		case TInst(c,_) if( c.toString() == "Array" ):
			return macro {
				var _a = $a, _b = $b;
				if ( _a?.length != _b?.length ) throw "assert";
				for( i => v1 in _a )
					if( v1 != _b[i] && !utest.Assert.checkEqRec(v1,_b[i]) )
						throw "assert";
				utest.Assert.C++;
			};
		default:
			return macro utest.Assert.checkEqRec($a,$b);
		}
	}

	public static function checkEqRec(a:Dynamic,b:Dynamic) {
		if( a == b ) return true;
		if( a is Array && b is Array ) {
			var a : Array<Dynamic> = cast a;
			var b:Array<Dynamic> = cast b;
			if( a.length != b.length )
				return false;
			for( i => va in a )
				if( !checkEqRec(va,b[i]) )
					return false;
			return true;
		}
		if( Type.typeof(a) == TObject && Type.typeof(b) == TObject ) {
			var af = Reflect.fields(a);
			var bf = Reflect.fields(b);
			for( f in af ) {
				if( !bf.remove(f) ) return false;
				if( !checkEqRec(Reflect.field(a,f),Reflect.field(b,f)) )
					return false;
			}
			if( bf.length != 0 ) return false;
			return true;
		}
		return false;
	}

	public static macro function contains(v, a, _) {
		return macro {
			var _a = $a, _v = $v, found = false;
			for ( v2 in _a )
				if( v2 == _v ) {
					found = true;
					break;
				}
			if( !found )
				throw "assert";
			Assert.C++;
		}
	}

	public static macro function notNull(v, ?_) {
		return macro {if ( $v == null ) throw "assert"; Assert.C++;}
	}

	public static macro function isTrue(v, _) {
		return macro { if( !$v ) throw "assert"; Assert.C++; }
	}

	public static macro function isFalse(v, _) {
		return macro { if ($v) throw "assert"; Assert.C++; }
 	}

	public static macro function fail(?msg,?_) {
		if( msg == null )
			return macro throw "assert";
		return macro throw "assert:"+$msg;
	}

	public static macro function raises(f,_,_) {
		return macro { var exc = false; try f() catch( e : Dynamic ) exc = true; if( !exc ) throw "assert"; Assert.C++; }
	}

}