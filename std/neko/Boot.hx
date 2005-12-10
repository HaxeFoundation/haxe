class Boot {


	private static function __instanceof(o,cl) {
		untyped {
			switch __dollar__typeof(o) {
			case __dollar__tint: return (cl == Int || cl == Float);
			case __dollar__tfloat: return cl == Float;
			case __dollar__tbool: return cl == Bool;
			case __dollar__tobject:
				var c = o.__class__;
				while( c != null ) {
					if( cl == c[0] )
						return true;
					var a = c[1];
					var i = 0;
					var l = __dollar__asize(a);
					while( i < l ) {
						if( cl == a[i] )
							return true;
						i += 1;
					}
					c = c[2];
				}
				return false;
			default:
				return false;
			}
		}
	}

	private static function __init() {
		untyped {
			String = NekoString__;
			Array = NekoArray__;
			Int = __dollar__new(null);
			Float = __dollar__new(null);
		}
	}

}