
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
			if( cname == "null" || cname == "Object" || cname == "int" || cname == "Number" || cname == "Boolean" )
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
			if( cname == "null" || cname.substr(0,8) == "builtin." )
				return null;
			// getEnum(Enum) should be null
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
			return untyped __global__["flash.utils.getQualifiedClassName"](c);
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
			try {
				cl = eval(name);
			} catch( e : Dynamic ) {
				cl = null;
			}
		#else neko
			var path = name.split(".");
			cl = Reflect.field(untyped neko.Boot.__classes,path[0]);
			var i = 1;
			while( cl != null && i < path.length ) {
				cl = Reflect.field(cl,path[i]);
				i += 1;
			}
		#else error
		#end
		// ensure that this is a class
		if( cl == null || cl.__name__ == null )
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
			try {
				e = eval(name);
			} catch( e : Dynamic ) {
				e = null;
			}
		#else neko
			var path = name.split(".");
			e = Reflect.field(neko.Boot.__classes,path[0]);
			var i = 1;
			while( e != null && i < path.length ) {
				e = Reflect.field(cl,path[i]);
				i += 1;
			}
		#else error
		#end
		// ensure that this is an enum
		if( e == null || e.__ename__ == null )
			return null;
		}
		return e;
	}


	/**
		Change the class prototype of an object
	**/
	public static function setClass( obj : Dynamic, cl : Class ) untyped {
		#if flash9
			throw "Not implemented";
		#else flash
			obj.__proto__ = cl.prototype;
		#else js
			obj.__proto__ = cl.prototype;
		#else neko
			__dollar__objsetproto(obj,cl.prototype);
		#else error
		#end
	}

	#if flash9
	static function describe( t : Dynamic, fact : Bool ) {
		var fields = new Array();
		var xml : Dynamic = untyped __global__["flash.utils.describeType"](t);
		if( fact )
			xml = xml.factory;
		var methods = xml.child("method");
		for( i in 0...methods.length() )
			fields.push( Std.string(untyped methods[i].attribute("name")) );
		var vars = xml.child("variable");
		for( i in 0...vars.length() )
			fields.push( Std.string(untyped vars[i].attribute("name")) );
		return fields;
	}
	#end

	/**
		Returns the list of instance fields
	**/
	public static function getInstanceFields( c : Class ) : Array<String> {
		#if flash9
			return describe(c,true);
		#else true
			var a = Reflect.fields(untyped c.prototype);
			c = untyped c.__super__;
			while( c != null ) {
				a = a.concat(Reflect.fields(untyped c.prototype));
				c = untyped c.__super__;
			}
			while( a.remove("__class__") ) {
				#if neko
				a.remove("__serialize");
				a.remove("__string");
				#end
			}
			return a;
		#end
	}

	/**
		Returns the list of class static fields
	**/
	public static function getClassFields( c : Class ) : Array<String> {
		#if flash9
			return describe(c,false);
		#else true
			var a = Reflect.fields(c);
			a.remove("__name__");
			a.remove("__interfaces__");
			a.remove("__super__");
			#if js
			a.remove("prototype");
			#end
			#if neko
			a.remove("__construct__");
			a.remove("prototype");
			a.remove("new");
			#end
			return a;
		#end
	}

	/**
		Returns all the available constructor names for an enum.
	**/
	public static function getEnumConstructs( e : Enum ) : Array<String> {
		#if flash9
			return describe(e,false);
		#else true
			var a = Reflect.fields(e);
			a.remove("__ename__");
			#if neko
			a.remove("prototype");
			#end
			return a;
		#end
	}

}

