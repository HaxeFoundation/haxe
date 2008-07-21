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
	public static function hasField( o : Dynamic, field : String ) : Bool untyped {
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
		#elseif php
			return __php__("
			(is_object($o) && (method_exists($o, $field) || isset($o->$field) || property_exists($o, $field)))
			||
			(is_string($o) && (in_array($field, array('toUpperCase', 'toLowerCase', 'charAt', 'charCodeAt', 'indexOf', 'lastIndexOf', 'split', 'substr', 'toString', 'length'))))
			||
			(is_array($o)  && (in_array($field, array('concat', 'copy', 'insert', 'iterator', 'join', 'pop', 'push', 'remove', 'reverse', 'shift', 'slice', 'sort', 'splice', 'unshift', 'toString', 'length'))))
			");
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
			if(hasField(o, field)) {
				if(__php__("$o instanceof __type__")) {
					if(__php__("is_callable(array($o->__tname__, $field))")) {
						return __php__("array($o->__tname__, $field)");
					} else {
						return __php__("eval('return '.$o->__tname__.'::$'.$field.';')");
					}
				} else if(__call__("is_string", o)) {
					if(field == 'length')
						return php.Boot.__len(o);
					else {
						switch(field) {
							case 'charAt':      return php.Boot.__closure(__php__("array('o' => $o)"), '$index', 'return substr($o, $index,1 );');
							case 'charCodeAt':  return php.Boot.__closure(__php__("array('o' => $o)"), '$index', 'return ord(substr($o, $index, 1));');
							case 'indexOf':     return php.Boot.__closure(__php__("array('o' => $o)"), '$value,$startIndex', 'return php_Boot::__index_of($o, $value, $startIndex);');
							case 'lastIndexOf': return php.Boot.__closure(__php__("array('o' => $o)"), '$value,$startIndex', 'return php_Boot::__last_index_of($o, $value, $startIndex);');
							case 'split':       return php.Boot.__closure(__php__("array('o' => $o)"), '$delimiter', 'return explode($delimiter, $o);');
							case 'substr':      return php.Boot.__closure(__php__("array('o' => $o)"), '$pos,$len', 'return php_Boot::__substr($o, $pos, $len);');
							case 'toUpperCase': return php.Boot.__closure(__php__("array('o' => $o)"), '', 'return strtoupper($o);');
							case 'toLowerCase': return php.Boot.__closure(__php__("array('o' => $o)"), '', 'return strtolower($o);');
							case 'toString':    return php.Boot.__closure(__php__("array('o' => $o)"), '', 'return $o;');
						}
						return null;
					}
				} else if(__call__("is_array", o)) {
					if(field == 'length')
						return php.Boot.__len(o);
					else
						switch(field) {
							case 'concat':   return php.Boot.__closure(__php__("array('o' => &$o)"), '$a', 'return array_merge($o, $a);');
							case 'join':     return php.Boot.__closure(__php__("array('o' => &$o)"), '$sep', 'return join($sep, $o);');
							case 'pop':      return php.Boot.__closure(__php__("array('o' => &$o)"), '', 'return array_pop($o);');
							case 'push':     return php.Boot.__closure(__php__("array('o' => &$o)"), '$x', 'return array_push($o, $x);');
							case 'reverse':  return php.Boot.__closure(__php__("array('o' => &$o)"), '', 'return rsort($o);');
							case 'shift':    return php.Boot.__closure(__php__("array('o' => &$o)"), '', 'return array_shift($o);');
							case 'slice':    return php.Boot.__closure(__php__("array('o' => &$o)"), '$pos,$end', 'return php_Boot::__array_slice(array(&$o), $pos, $end);');
							case 'sort':     return php.Boot.__closure(__php__("array('o' => &$o)"), '$f', 'return php_Boot::__array_sort($o, $f);');
							case 'splice':   return php.Boot.__closure(__php__("array('o' => &$o)"), '$pos,$len', 'return php_Boot::__array_splice(array(&$o), $pos, $len);');
							case 'toString': return php.Boot.__closure(__php__("array('o' => &$o)"), '', 'return "[".join(", ", $o)."]";');
							case 'unshift':  return php.Boot.__closure(__php__("array('o' => &$o)"), '$x', 'return array_unshift($o, $x);');
							case 'insert':   return php.Boot.__closure(__php__("array('o' => &$o)"), '$pos,$x', 'return php_Boot::__array_insert(array(&$o), $pos, $x);');
							case 'remove':   return php.Boot.__closure(__php__("array('o' => &$o)"), '$x', 'return php_Boot::__array_remove(array(&$o), $x);');
							case 'iterator': return php.Boot.__closure(__php__("array('o' => &$o)"), '', 'return new HArrayIterator($o);');
							case 'copy':     return php.Boot.__closure(__php__("array('o' => $o)"), '', 'return $o;');
						}
					return null;
				} else if(__php__("property_exists($o, $field)")) {
					if(__php__("is_array($o->$field) && is_callable($o->$field)")) {
						return __php__("$o->$field");
					} else if(__php__("is_string($o->$field) && php_Boot::__is_lambda($o->$field)")) {
						return __php__("array($o, $field)");
					} else {
						return __php__("$o->$field");
					}
				} else {
					return __php__("array($o, $field)");
				}
			} else {
				return null;
			}
		#else
			return null;
		#end
	}

	/**
		Set an object field value.
	**/
	public #if !php inline #end static function setField( o : Dynamic, field : String, value : Dynamic ) : Void untyped {
		#if flash
			o[field] = value;
		#elseif js
			o[field] = value;
		#elseif neko
			if( __dollar__typeof(o) == __dollar__tobject )
				__dollar__objset(o,__dollar__hash(field.__s),value);
		#elseif php
			untyped __php__("$o->$field = $value");
		#end
	}

	/**
		Call a method with the given object and arguments.
	**/
	public #if !php inline #end static function callMethod( o : Dynamic, func : Dynamic, args : Array<Dynamic> ) : Dynamic untyped {
		#if flash9
			return func.apply(o,args);
		#elseif flash
			return func["apply"](o,args);
		#elseif js
			return func.apply(o,args);
		#elseif neko
			return __dollar__call(func,o,args.__neko());
		#elseif php
			if(__call__("is_string", o) || __call__("is_array", o)) {
				if(args.length == 0) return field(o, func)();
				else if(args.length == 1) return field(o, func)(args[0]);
				else return field(o, func)(args[0], args[1]);
			}
			return __php__("call_user_func_array(is_callable($func) ? $func : array($o, $func) , $args)");
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
			return __call__('is_array', o) 
					? __call__('array', 'concat', 'copy', 'insert', 'iterator', 'length', 'join', 'pop', 'push', 'remove', 'reverse', 'shift', 'slice', 'sort', 'splice', 'toString', 'unshift') 
					: (__call__('is_string', o) 
						? __call__('array', 'charAt', 'charCodeAt', 'indexOf', 'lastIndexOf', 'length', 'split', 'substr', 'toLowerCase', 'toString', 'toUpperCase') 
						: __call__('array_keys', __call__('get_object_vars', o)));
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
			return __php__("(is_array($f) && is_callable($f)) || php_Boot::__is_lambda($f)") || (__php__("is_array($f)") && hasField(f[0], f[1]) && f[1] != "length");
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
		if( f1 == f2 )
			return true;
		if( !isFunction(f1) || !isFunction(f2) )
			return false;
		#if neko
			return false; // compare already done
		#elseif flash9
			return false; // VM-level closures
		#elseif flash
			return untyped f1["f"] == f2["f"] && f1["o"] == f2["o"] && f1["f"] != null;
		#elseif js
			return f1.scope == f2.scope && f1.method == f2.method && f1.method != null;
		#elseif php
			if(untyped __call__("is_array", f1) && untyped __call__("is_array", f1))
				return f1[0] == f2[0] && f1[1] == f2[1];
			if(untyped __call__("is_string", f1) && untyped __call__("is_string", f2))
				return f1 == f2;
			return false;
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
				return __php__("$v instanceof Anonymous") || Type.getClass(v) != null;
			if(__php__("is_string($v) && !php_Boot::__is_lambda($v)")) return true;
			if(__php__("is_array($v) && !is_callable($v)")) return true;
			return false;
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
				if(untyped __call__("is_string", o) || untyped __call__("is_array", o)) return o;
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
			return function() { 
				var args = untyped __call__("func_get_args");
				return f(args); 
			};
		#else
			return null;
		#end
	}

}
