
/**
	An abstract type that represents a Class.
**/
enum Class {
}

/**
	An abstract type that represents an Enum.
**/
enum Enum {
}

class Type {

	/**
		Returns the class of an object
	**/
	public static function getClass( o : Dynamic ) : Class untyped {
		#if flash9
			var cname = __global__["flash.utils.getQualifiedClassName"](o);
			if( cname == "null" || cname == "Object" || cname.substr(0,8) == "builtin." )
				return null;
			if( o.hasOwnProperty("prototype") )
				return null;
			var c = __global__["flash.utils.getDefinitionByName"](cname);
			if( c.__isenum )
				return null;
			return c;
		#else flash
			return o.__class__;
		#else js
			if( o == null )
				return null;
			return o.__class__;
		#else neko
			if( __dollar__typeof(o) != __dollar__tobject )
				return null;
			var p = __dollar__objgetproto(o);
			if( p == null )
				return null;
			return p.__class__;
		#else error
		#end
	}

	/**
		Returns the class of an object
	**/
	public static function getEnum( o : Dynamic ) : Enum untyped {
		#if flash9
			var cname = __global__["flash.utils.getQualifiedClassName"](o);
			if( cname == "null" || cname == "Object" || cname.substr(0,8) == "builtin." )
				return null;
			// getClass(Class) should be null
			if( o.hasOwnProperty("prototype") )
				return null;
			var c = __global__["flash.utils.getDefinitionByName"](cname);
			if( !c.__isenum )
				return null;
			return c;
		#else flash
			return o.__enum__;
		#else js
			if( o == null )
				return null;
			return o.__enum__;
		#else neko
			if( __dollar__typeof(o) != __dollar__tobject )
				return null;
			return o.__enum__;
		#else error
		#end
	}


	/**
		Returns the super-class of a class
	**/
	public static function getSuperClass( c : Class ) : Class untyped {
		#if flash9
			var cname = __global__["flash.utils.getQualifiedSuperclassName"](c);
			if( cname == "Object" )
				return null;
			return __global__["flash.utils.getDefinitionByName"](cname);
		#else true
			return c.__super__;
		#end
	}


	/**
		Returns the complete name of the class of an object
	**/
	public static function getClassName( c : Class ) : String {
		#if flash9
			var n = untyped __global__["flash.utils.getQualifiedClassName"](c);
			return switch( n ) {
			case "int": "Int";
			case "Number": "Float";
			case "Boolean": "Bool";
			default: n;
			}
		#else true
			return untyped c.__name__.join(".");
		#end
	}

	/**
		Returns the complete name of the class of an object
	**/
	public static function getEnumName( e : Enum ) : String {
		#if flash9
			var n = untyped __global__["flash.utils.getQualifiedClassName"](e);
			return n;
		#else true
			return untyped e.__ename__.join(".");
		#end
	}

	/**
		Evaluates a class from a name
	**/
	public static function resolveClass( name : String ) : Class {
		var cl : Class;
		untyped {
		#if flash9
			try {
				cl = __global__["flash.utils.getDefinitionByName"](name);
				if( cl.__isenum )
					return null;
				return cl; // skip test below
			} catch( e : Dynamic ) {
				return null;
			}
		#else flash
			cl = __eval__(name);
		#else js
			cl = eval(name);
		#else neko
			var path = name.split(".");
			cl = Reflect.field(untyped neko.Boot.__classes,path[0]);
			var i = 1;
			while( cl != null && i < path.length ) {
				cl = Reflect.field(cl,name[i]);
				i += 1;
			}
		#else error
		#end
		// ensure that this is a class
		if( cl == null || cl.__interfaces__ == null )
			return null;
		}
		return cl;
	}


	/**
		Evaluates an enum from a name
	**/
	public static function resolveEnum( name : String ) : Enum {
		var e : Dynamic;
		untyped {
		#if flash9
			try {
				e = __global__["flash.utils.getDefinitionByName"](name);
				if( !e.__isenum )
					return null;
				return e;
			} catch( e : Dynamic ) {
				return null;
			}
		#else flash
			e = __eval__(name);
		#else js
			e = eval(name);
		#else neko
			var path = name.split(".");
			e = untyped field(neko.Boot.__classes,path[0]);
			var i = 1;
			while( e != null && i < path.length ) {
				e = field(cl,path[i]);
				i += 1;
			}
		#else error
		#end
		if( e == null || e.__ename__ == null )
			return null;
		}
		return e;
	}


	/**
		Change the class prototype of an object
	**/
	public static function setPrototype( obj : Dynamic, proto : Dynamic ) {
		#if flash9
			throw "Not implemented";
		#else flash
			obj.__proto__ = proto;
		#else js
			obj.__proto__ = proto;
		#else neko
			untyped __dollar__objsetproto(obj,proto);
		#else error
		#end
	}

}

