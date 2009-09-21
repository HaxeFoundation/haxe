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
	The Reflect API is a way to manipulate values dynamicly through an
	abstract interface in an untyped manner. Use with care.
**/
class Reflect {

	/**
		Tells if an object has a field set. This doesn't take into account the object prototype (class methods).
	**/
	public #if php inline #end static function hasField( o : Dynamic, field : String ) : Bool untyped {
		#if flash9
			return o.hasOwnProperty( field );
		#elseif flash
			return this["hasOwnProperty"]["call"](o,field);
		#elseif js
			if( o.hasOwnProperty != null )
				return o.hasOwnProperty(field);
			var arr = fields(o);
			for( t in arr.iterator() )
				if( t == field ) return true;
			return false;
		#elseif neko
			return __dollar__typeof(o) == __dollar__tobject && __dollar__objfield(o,__dollar__hash(field.__s));
		#elseif cpp
			return o!=null && o.__HasField(field);
		#elseif php
			return __call__("_hx_has_field", o, field);
		#else
			return false;
		#end
	}

	/**
		Returns the field of an object, or null if [o] is not an object or doesn't have this field.
	**/
	public #if !php inline #end static function field( o : Dynamic, field : String ) : Dynamic untyped {
		#if flash9
			return (o == null) ? null : o[field];
		#elseif flash
			return o[field];
		#elseif cpp
			return o.__Field(field);
		#elseif js
			var v = null;
			try {
				v = o[field];
			} catch( e : Dynamic ) {
			}
			return v;
		#elseif neko
			return if( __dollar__typeof(o) != __dollar__tobject ) null else __dollar__objget(o,__dollar__hash(field.__s));
		#elseif php
			return __call__("_hx_field", o, field);
		#else
			return null;
		#end
	}

	/**
		Set an object field value.
	**/
	public inline static function setField( o : Dynamic, field : String, value : Dynamic ) : Void untyped {
		#if flash
			o[field] = value;
		#elseif js
			o[field] = value;
		#elseif cpp
			o.__SetField(field,value);
		#elseif neko
			if( __dollar__typeof(o) == __dollar__tobject )
				__dollar__objset(o,__dollar__hash(field.__s),value);
		#elseif php
			__setfield__(o, field, value);
		#end
	}

	/**
		Call a method with the given object and arguments.
	**/
	public #if !(php||cpp) inline #end static function callMethod( o : Dynamic, func : Dynamic, args : Array<Dynamic> ) : Dynamic untyped {
		#if flash9
			return func.apply(o,args);
		#elseif flash
			return func["apply"](o,args);
		#elseif js
			return func.apply(o,args);
		#elseif neko
			return __dollar__call(func,o,args.__neko());
		#elseif php
			if(__call__("is_string", o)) {
				if(args.length == 0) return __call__("call_user_func", field(o, func));
				else if(args.length == 1) return __call__("call_user_func", field(o, func), args[0]);
				else return __call__("call_user_func", field(o, func), args[0], args[1]);
			}
			return __php__("call_user_func_array(is_callable($func) ? $func : array($o, $func) , $args == null ? array() : $args->»a)");
		#elseif cpp
         var s:String = func;
         return untyped o.__Field(s).__Run(args);
		#else
			return null;
		#end
	}

	/**
		Returns the list of fields of an object, excluding its prototype (class methods).
	**/
	public static function fields( o : Dynamic ) : Array<String> untyped {
		if( o == null ) return new Array();
		#if flash9
			var a : Array<String> = __keys__(o);
			var i = 0;
			while( i < a.length ){
				if( !o.hasOwnProperty(a[i]) )
					a.splice(i,1);
				else
					++i;
			}
			return a;
		#elseif flash
			var a : Array<String> = __keys__(o);
			var i = 0;
			while( i < a.length ) {
				if( !a["hasOwnProperty"]["call"](o,a[i]) )
					a.splice(i,1);
				else
					++i;
			}
			return a;
		#elseif js
			var a = new Array();
			if( o.hasOwnProperty ) {
				__js__("
					for(var i in o)
						if( o.hasOwnProperty(i) )
							a.push(i);
				");
			} else {
				var t;
				try{ t = o.__proto__; } catch( e : Dynamic ) { t = null; }
				if( t != null )
					o.__proto__ = null;
				__js__("
					for(var i in o)
						if( i != \"__proto__\" )
							a.push(i);
				");
				if( t != null )
					o.__proto__ = t;
			}
			return a;
		#elseif cpp
			var a : Array<String> = [];
			o.__GetFields(a);
			return a;
		#elseif neko
			if( __dollar__typeof(o) != __dollar__tobject )
				return new Array<String>();
			else {
				var a = __dollar__objfields(o);
				var i = 0;
				var l = __dollar__asize(a);
				while( i < l ) {
					a[i] = new String(__dollar__field(a[i]));
					i++;
				}
				return Array.new1(a,l);
			}
		#elseif php
			return __php__('$o instanceof _hx_array')
					? __php__("new _hx_array(array('concat','copy','insert','iterator','length','join','pop','push','remove','reverse','shift','slice','sort','splice','toString','unshift'))")
					: (__call__('is_string', o)
						? __php__("new _hx_array(array('charAt','charCodeAt','indexOf','lastIndexOf','length','split','substr','toLowerCase','toString','toUpperCase'))")
						: __php__("new _hx_array(_hx_get_object_vars($o))"));
		#else
			return new Array();
		#end
	}

	/**
		Tells if a value is a function or not.
	**/
	public static function isFunction( f : Dynamic ) : Bool untyped {
		#if flash9
			return __typeof__(f) == "function";
		#elseif flash
			return __typeof__(f) == "function" && f.__name__ == null;
		#elseif js
			return __js__("typeof(f)") == "function" && f.__name__ == null;
		#elseif neko
			return __dollar__typeof(f) == __dollar__tfunction;
		#elseif php
			return __php__("(is_array($f) && is_callable($f)) || _hx_is_lambda($f)") || (__php__("is_array($f)") && hasField(__php__("$f[0]"), __php__("$f[1]")) && __php__("$f[1]") != "length");
		#elseif cpp
			return f!=null && f.__GetType() ==  __global__.vtFunction;
		#else
			return false;
		#end
	}

	/**
		Generic comparison function, does not work for methods, see [compareMethods]
	**/
	public static function compare<T>( a : T, b : T ) : Int {
		#if neko
		return untyped __dollar__compare(a,b);
		#else
		return ( a == b ) ? 0 : (((cast a) > (cast b)) ? 1 : -1);
		#end
	}

	/**
		Compare two methods closures. Returns true if it's the same method of the same instance.
		Does not work on Neko platform.
	**/
	public static function compareMethods( f1 : Dynamic, f2 : Dynamic ) : Bool {
		#if !neko
		if( f1 == f2 )
			return true;
		if( !isFunction(f1) || !isFunction(f2) )
			return false;
		#end
		#if neko
			return same_closure(f1,f2);
		#elseif flash9
			return false; // VM-level closures
		#elseif flash
			return untyped f1["f"] == f2["f"] && f1["o"] == f2["o"] && f1["f"] != null;
		#elseif js
			return f1.scope == f2.scope && f1.method == f2.method && f1.method != null;
		#elseif php
			if(untyped __call__("is_array", f1) && untyped __call__("is_array", f1))
				return untyped __php__("$f1[0] === $f2[0] && $f1[1] == $f2[1]");
			if(untyped __call__("is_string", f1) && untyped __call__("is_string", f2))
				return f1 == f2;
			return false;
		#elseif cpp
			return untyped __global__.__hxcpp_same_closure(f1,f2);
		#else
			return false;
		#end
	}

	/**
		Tells if a value is an object or not.

	**/
	public static function isObject( v : Dynamic ) : Bool untyped {
		#if neko
			return __dollar__typeof(v) == __dollar__tobject && v.__enum__ == null;
		#elseif flash9
			if( v == null )
				return false;
			var t = __typeof__(v);
			if( t == "object" ) {
				try {
					if( v.__enum__ == true )
						return false;
				} catch( e : Dynamic ) {
				}
				return true;
			}
			return (t == "string");
		#elseif flash
			var t = __typeof__(v);
			return (t == "string" || (t == "object" && !v.__enum__) || (t == "function" && v.__name__ != null));
		#elseif js
			if( v == null )
				return false;
			var t = __js__("typeof(v)");
			return (t == "string" || (t == "object" && !v.__enum__) || (t == "function" && v.__name__ != null));
		#elseif php
			if( v == null )
				return false;
			if(__call__("is_object", v))
				return __php__("$v instanceof _hx_anonymous") || Type.getClass(v) != null;
			if(__php__("is_string($v) && !_hx_is_lambda($v)")) return true;
			return false;
		#elseif cpp
			if (v==null) return false;
			var t:Int = v.__GetType();
			return t ==  __global__.vtObject || t==__global__.vtClass;
		#else
			return false;
		#end
	}

	/**
		Delete an object field.
	**/
	public static function deleteField( o : Dynamic, f : String ) : Bool untyped {
		#if flash9
			if( o.hasOwnProperty(f) != true ) return false;
			__delete__(o,f);
			return true;
		#elseif flash
			if( this["hasOwnProperty"]["call"](o,f) != true ) return false;
			__delete__(o,f);
			return true;
		#elseif js
			if( !hasField(o,f) ) return false;
			__js__("delete")(o[f]);
			return true;
		#elseif neko
			return __dollar__objremove(o,__dollar__hash(f.__s));
		#elseif php
			if(!hasField(o,f)) return false;
			untyped __php__("unset($o->$f)");
			return true;
		#else
			return false;
		#end
	}

	/**
		Make a copy of the fields of an object.
	**/
	public static function copy<T>( o : T ) : T {
		#if neko
			return untyped __dollar__new(o);
		#else
			#if php
				if(untyped __call__("is_string", o)) return o;
			#end
			var o2 : Dynamic = {};
			for( f in Reflect.fields(o) )
				Reflect.setField(o2,f,Reflect.field(o,f));
			return o2;
		#end
	}

	/**
		Transform a function taking an array of arguments into a function that can
		be called with any number of arguments.
	**/
	public static function makeVarArgs( f : Array<Dynamic> -> Dynamic ) : Dynamic {
		#if neko
			return untyped __dollar__varargs(function(a) { return f(Array.new1(a,__dollar__asize(a))); });
		#elseif flash9
			return function(__arguments__) { return f(__arguments__); };
		#elseif js
			return function() untyped {
				var a = new Array();
				for( i in 0...arguments.length )
					a.push(arguments[i]);
				return f(a);
			};
		#elseif flash
			return function() { return f(untyped __arguments__); };
		#elseif php
			untyped __php__("return array(new _hx_lambda(array('f' => &$f), null, array('args'), 'return call_user_func($f, new _hx_array($args));'), 'makeArgs')");
		#else
			return null;
		#end
	}

	#if neko
	static var same_closure = try neko.Lib.load("std","same_closure",2) catch( e : Dynamic ) function(f1,f2) return f1 == f2;
	#end

}
