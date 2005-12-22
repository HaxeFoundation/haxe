
class Reflect {

	public static function makeNew( cl : Dynamic, args : Array<Dynamic> ) : Dynamic {
		return untyped
		#flash
			switch( args.length ) {
			case 0:
				__new__(cl);
			case 1:
				__new__(cl,args[0]);
			case 2:
				__new__(cl,args[0],args[1]);
			case 3:
				__new__(cl,args[0],args[1],args[2]);
			case 4:
				__new__(cl,args[0],args[1],args[2],args[3]);
			case 5:
				__new__(cl,args[0],args[1],args[2],args[3],args[4]);
			default:
				throw "Too many arguments";
			}
		#else neko
			__dollar__call(__dollar__objget(cl,__dollar__hash("new".__s)),cl,args.__a)
		#else error
		#end
			;
	}

	public static function haveField( o : Dynamic, field : String ) : Bool {
		return untyped
		#flash
			o.hasOwnProperty(field)
		#else neko
			__dollar__typeof(o) == __dollar__tobject && __dollar__objfield(o,__dollar__hash(field.__s))
		#else error
		#end
			;
	}

	public static function field( o : Dynamic, field : String ) : Dynamic {
		return untyped
		#flash
			{
				var f = o[field];
				if( f == null && !o.hasOwnProperty(f) )
					throw ("No such field : " + field);
				return f;
			}
		#else neko
			{
				if( __dollar__typeof(o) != __dollar__tobject )
					throw ("No such field : " + field);
				var fh = __dollar__hash(field.__s);
				var f = __dollar__objget(o,fh);
				if( f == null && !__dollar__objfield(o,fh) )
					throw ("No such field : " + field);
				return f;
			}
		#else error
		#end
			;
	}

	public static function setField( o : Dynamic, field : String, value : Dynamic ) : Void {
		untyped {
		#flash
			o[field] = value;
		#else neko
			if( __dollar__typeof(o) == __dollar__tobject )
				__dollar__objset(o,__dollar__hash(field.__s),value);
		#else error
		#end
		}
	}

	public static function callField( o : Dynamic, func : Dynamic, args : Array<Dynamic> ) : Dynamic {
		return untyped
		#flash
			func.apply(o,args)
		#else neko
			__dollar__call(func,o,args.__a)
		#else error
		#end
			;
	}

	public static function fields( o : Dynamic ) : Array<String> {
		return untyped
		#flash
			__keys__(o)
		#else neko
			if( __dollar__typeof(o) != __dollar__tobject )
				new Array<String>();
			else {
				var a = __dollar__objfields(o);
				var i = 0, j = 0;
				var hclass = __dollar__hash("__class__".__s);
				var l = __dollar__asize(a);
				while( i < l ) {
					if( a[i] == hclass )
						i++;
					else {
						a[j] = new String(__dollar__field(a[i]));
						i++;
						j++;
					}
				}
				Array.new1(a,j);
			}
		#else error
		#end
			;
	}

}
