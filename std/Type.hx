/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

/**
	The diffent possible runtime types of a value.
	See [Type] for the haXe Reflection API.
**/
enum ValueType {
	TNull;
	TInt;
	TFloat;
	TBool;
	TObject;
	TFunction;
	TClass( c : Class<Dynamic> );
	TEnum( e : Enum<Dynamic> );
	TUnknown;
}

/**
	The haXe Reflection API enables you to retreive informations about any value,
	Classes and Enums at runtime.
**/
extern class Type {

	/**
		Returns the class of a value or [null] if this value is not a Class instance.
	**/
	public static function getClass<T>( o : T ) : Class<T>;

	/**
		Returns the enum of a value or [null] if this value is not an Enum instance.
	**/
	public static function getEnum( o : Dynamic ) : Enum<Dynamic>;


	/**
		Returns the super-class of a class, or null if no super class.
	**/
	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic>;


	/**
		Returns the complete name of a class.
	**/
	public static function getClassName( c : Class<Dynamic> ) : String;

	/**
		Returns the complete name of an enum.
	**/
	public static function getEnumName( e : Enum<Dynamic> ) : String;

	/**
		Evaluates a class from a name. The class must have been compiled
		to be accessible.
	**/
	public static function resolveClass( name : String ) : Class<Dynamic>;


	/**
		Evaluates an enum from a name. The enum must have been compiled
		to be accessible.
	**/
	public static function resolveEnum( name : String ) : Enum<Dynamic>;

	/**
		Creates an instance of the given class with the list of constructor arguments.
	**/
	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T;
	/**
		Similar to [Reflect.createInstance] excepts that the constructor is not called.
		This enables you to create an instance without any side-effect.
	**/
	public static function createEmptyInstance<T>( cl : Class<T> ) : T;

	/**
		Create an instance of an enum by using a constructor name and parameters.
	**/
	public static function createEnum<T>( e : Enum<T>, constr : String, ?params : Array<Dynamic> ) : T;

	/**
		Create an instance of an enum by using a constructor index and parameters.
	**/
	public static function createEnumIndex<T>( e : Enum<T>, index : Int, ?params : Array<Dynamic> ) : T;

	/**
		Returns the list of instance fields.
	**/
	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String>;

	/**
		Returns the list of a class static fields.
	**/
	public static function getClassFields( c : Class<Dynamic> ) : Array<String> {
		#if flash9
			var a = describe(c,false);
			a.remove("__construct__");
			a.remove("prototype");
			return a;
		#elseif php
			if(untyped c.__qname__ == 'String') return ['fromCharCode'];
			if(untyped c.__qname__ == 'Array')  return [];
			untyped __php__("
			$rfl = $c->__rfl__();
			if($rfl === null) return new _hx_array(array());
			$ms = $rfl->getMethods();
			$r = array();
			while(list(, $m) = each($ms))
				if($m->isStatic()) $r[] = $m->getName();
			$ps = $rfl->getProperties();
			while(list(, $p) = each($ps))
				if($p->isStatic()) $r[] = $p->getName();
			");
			return untyped __php__("new _hx_array(array_unique($r))");
		#elseif cpp
			return untyped c.GetClassFields();
		#else
			var a = Reflect.fields(c);
			a.remove(__unprotect__("__name__"));
			a.remove(__unprotect__("__interfaces__"));
			a.remove(__unprotect__("__super__"));
			#if js
			a.remove("prototype");
			#end
			#if neko
			a.remove("__string");
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
	public static function getEnumConstructs( e : Enum<Dynamic> ) : Array<String> untyped {
		#if php
			if (__php__("$e->__tname__ == 'Bool'")) return ['true', 'false'];
			if (__php__("$e->__tname__ == 'Void'")) return [];
			return __call__("new _hx_array", e.__constructors);
		#elseif cpp
			return untyped e.GetClassFields();
		#else
			return untyped e.__constructs__;
		#end
	}

	/**
		Returns the runtime type of a value.
	**/
	public static function typeof( v : Dynamic ) : ValueType untyped {
		#if neko
			return switch( __dollar__typeof(v) ) {
			case __dollar__tnull: TNull;
			case __dollar__tint: TInt;
			case __dollar__tfloat: TFloat;
			case __dollar__tbool: TBool;
			case __dollar__tfunction: TFunction;
			case __dollar__tobject:
				var c = v.__class__;
				if( c != null )
					TClass(c);
				else {
					var e = v.__enum__;
					if( e != null )
						TEnum(e);
					else
						TObject;
				}
			default: TUnknown;
			}
		#elseif flash9
			var cname = __global__["flash.utils.getQualifiedClassName"](v);
			switch(cname) {
			case "null": return TNull;
			case "void": return TNull; // undefined
			case "int": return TInt;
			case "Number":
				// integers >28 bits are stored as Numbers in avm2
				if( (v < -0x10000000 || v >= 0x10000000) && Std.int(v) == v )
					return TInt;
				return TFloat;
			case "Boolean": return TBool;
			case "Object": return TObject;
			case "Function": return TFunction;
			default:
				var c : Dynamic = null;
				try {
					c = __global__["flash.utils.getDefinitionByName"](cname);
					if( v.hasOwnProperty("prototype") )
						return TObject;
					if( c.__isenum )
						return TEnum(c);
					return TClass(c);
				} catch( e : Dynamic ) {
					if( cname == "builtin.as$0::MethodClosure" || cname.indexOf("-") != -1 )
						return TFunction;
					return if( c == null ) TFunction else TClass(c);
				}
			}
			return null;
		#elseif (flash || js)
			switch( #if flash __typeof__ #else __js__("typeof") #end(v) ) {
			#if flash
			case "null": return TNull;
			#end
			case "boolean": return TBool;
			case "string": return TClass(String);
			case "number":
				// this should handle all cases : NaN, +/-Inf and Floats outside range
				if( Math.ceil(v) == v%2147483648.0 )
					return TInt;
				return TFloat;
			case "object":
				#if js
				if( v == null )
					return TNull;
				#end
				var e = v.__enum__;
				if( e != null )
					return TEnum(e);
				var c = v.__class__;
				if( c != null )
					return TClass(c);
				return TObject;
			case "function":
				if( v.__name__ != null )
					return TObject;
				return TFunction;
			case "undefined":
				return TNull;
			default:
				return TUnknown;
			}
		#elseif php
			if(v == null) return TNull;
			if(__call__("is_array", v)) {
				if(__call__("is_callable", v)) return TFunction;
				return TClass(Array);
			}
			if(__call__("is_string", v)) {
				if(__call__("_hx_is_lambda", v)) return TFunction;
				return TClass(String);
			}
			if(__call__("is_bool", v)) return TBool;
			if(__call__("is_int", v)) return TInt;
			if(__call__("is_float", v)) return TFloat;
			if(__php__("$v instanceof _hx_anonymous"))  return TObject;
			if(__php__("$v instanceof _hx_enum"))  return TObject;
			if(__php__("$v instanceof _hx_class"))  return TObject;

			var c = __php__("_hx_ttype(get_class($v))");

			if(__php__("$c instanceof _hx_enum"))  return TEnum(cast c);
			if(__php__("$c instanceof _hx_class")) return TClass(cast c);
			return TUnknown;
		#elseif cpp
			if (v==null) return TNull;
			var t:Int = untyped v.__GetType();
			switch(t)
			{
				case untyped __global__.vtBool : return TBool;
				case untyped __global__.vtInt : return TInt;
				case untyped __global__.vtFloat : return TFloat;
				case untyped __global__.vtFunction : return TFunction;
				case untyped __global__.vtObject : return TObject;
				case untyped __global__.vtEnum : return TEnum(v.__GetClass());
				default:
					return untyped TClass(v.__GetClass());
			}
		#else
			return TUnknown;
		#end
	}

	/**
		Recursively compare two enums constructors and parameters.
	**/
	public static function enumEq<T>( a : T, b : T ) : Bool untyped {
		if( a == b )
			return true;
		#if neko
			try {
				if( a.__enum__ == null || a.index != b.index )
					return false;
			} catch( e : Dynamic ) {
				return false;
			}
			for( i in 0...__dollar__asize(a.args) )
				if( !enumEq(a.args[i],b.args[i]) )
					return false;
		#elseif flash9
			try {
				if( a.index != b.index )
					return false;
				var ap : Array<Dynamic> = a.params;
				var bp : Array<Dynamic> = b.params;
				for( i in 0...ap.length )
					if( !enumEq(ap[i],bp[i]) )
						return false;
			} catch( e : Dynamic ) {
				return false;
			}
		#elseif php
			try {
				if( a.index != b.index )
					return false;
				for( i in 0...__call__("count", a.params))
					if(getEnum(untyped __php__("$a->params[$i]")) != null) {
						if(!untyped enumEq(__php__("$a->params[$i]"),__php__("$b->params[$i]")))
							return false;
					} else {
						if(!untyped __call__("_hx_equal", __php__("$a->params[$i]"),__php__("$b->params[$i]")))
							return false;
					}
			} catch( e : Dynamic ) {
				return false;
			}
		#elseif cpp
			return a==b;
		#elseif flash
			// no try-catch since no exception possible
			if( a[0] != b[0] )
				return false;
			for( i in 2...a.length )
				if( !enumEq(a[i],b[i]) )
					return false;
			var e = a.__enum__;
			if( e != b.__enum__ || e == null )
				return false;
		#else
			try {
				if( a[0] != b[0] )
					return false;
				for( i in 2...a.length )
					if( !enumEq(a[i],b[i]) )
						return false;
				var e = a.__enum__;
				if( e != b.__enum__ || e == null )
					return false;
			} catch( e : Dynamic ) {
				return false;
			}
		#end
		return true;
	}

	/**
		Returns the constructor of an enum
	**/
	public static function enumConstructor( e : Dynamic ) : String {
		#if neko
			return new String(e.tag);
		#elseif (flash9 || php)
			return e.tag;
		#elseif cpp
			return e.__Tag();
		#else
			return e[0];
		#end
	}

	/**
		Returns the parameters of an enum
	**/
	public static function enumParameters( e : Dynamic ) : Array<Dynamic> {
		#if neko
			return if( e.args == null ) [] else untyped Array.new1(e.args,__dollar__asize(e.args));
		#elseif flash9
			return if( e.params == null ) [] else e.params;
		#elseif cpp
			var result : Array<Dynamic> =  untyped e.__EnumParams();
			return result==null ? [] : result;
		#elseif php
			if(e.params == null)
				return [];
			else
				return untyped __php__("new _hx_array($e->params)");
		#else
			return e.slice(2);
		#end
	}

	/**
		Returns the index of the constructor of an enum
	**/
	public inline static function enumIndex( e : Dynamic ) : Int {
		#if (neko || flash9 || php)
			return e.index;
		#elseif cpp
			return e.__Index();
		#else
			return e[1];
		#end
	}

}

