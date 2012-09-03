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

@:core_api class Reflect {

	public inline static function hasField( o : Dynamic, field : String ) : Bool {
		return untyped __call__("_hx_has_field", o, field);
	}

	public static function field( o : Dynamic, field : String ) : Dynamic {
		return untyped __call__("_hx_field", o, field);
	}

	public inline static function setField( o : Dynamic, field : String, value : Dynamic ) : Void {
		untyped __setfield__(o, field, value);
	}

	public static function getProperty( o : Dynamic, field : String ) : Dynamic {
		if (null == o) return null;
		var cls : String = Std.is(o, Class) ? untyped __php__("$o->__tname__") : untyped __call__("get_class", o);
		var cls_vars : php.NativeArray = untyped __call__("get_class_vars", cls);
		if (untyped __php__("isset($cls_vars['__properties__']) && isset($cls_vars['__properties__']['get_'.$field]) && ($field = $cls_vars['__properties__']['get_'.$field])"))
			return untyped __php__("$o->$field()");
		else
			return untyped __php__("$o->$field");
	}

	public static function setProperty( o : Dynamic, field : String, value : Dynamic ) : Void untyped {
		if (null == o) return null;
		var cls : String = Std.is(o, Class) ? untyped __php__("$o->__tname__") : untyped __call__("get_class", o);
		var cls_vars : php.NativeArray = untyped __call__("get_class_vars", cls);
		if (untyped __php__("isset($cls_vars['__properties__']) && isset($cls_vars['__properties__']['set_'.$field]) && ($field = $cls_vars['__properties__']['set_'.$field])"))
			return untyped __php__("$o->$field($value)");
		else
			return untyped __php__("$o->$field = $value");
	}

	public static function callMethod( o : Dynamic, func : Dynamic, args : Array<Dynamic> ) : Dynamic untyped {
		if (__call__("is_string", o) && !__call__("is_array", func)) {
			return __call__("call_user_func_array", field(o, func), __field__(args, "»a"));
		}
		return __call__("call_user_func_array", __call__("is_callable", func) ? func : __call__("array", o, func), (null == args ? __call__("array") : __field__(args, "»a")));
	}

	public static function fields( o : Dynamic ) : Array<String> {
		if( o == null ) return new Array();
		return untyped __php__('$o instanceof _hx_array')
				? __php__("new _hx_array(array('concat','copy','insert','iterator','length','join','pop','push','remove','reverse','shift','slice','sort','splice','toString','unshift'))")
				: (__call__('is_string', o)
					? __php__("new _hx_array(array('charAt','charCodeAt','indexOf','lastIndexOf','length','split','substr','toLowerCase','toString','toUpperCase'))")
					: __php__("new _hx_array(_hx_get_object_vars($o))"));
	}

	public static function isFunction( f : Dynamic ) : Bool {
		return untyped __php__("(is_array($f) && is_callable($f)) || _hx_is_lambda($f)") || (__php__("is_array($f)") && hasField(__php__("$f[0]"), __php__("$f[1]")) && __php__("$f[1]") != "length");
	}

	public static function compare<T>( a : T, b : T ) : Int {
		return ( a == b ) ? 0 : (((cast a) > (cast b)) ? 1 : -1);
	}

	public static function compareMethods( f1 : Dynamic, f2 : Dynamic ) : Bool {
		if(untyped __call__("is_array", f1) && untyped __call__("is_array", f1))
			return untyped __php__("$f1[0] === $f2[0] && $f1[1] == $f2[1]");
		if(untyped __call__("is_string", f1) && untyped __call__("is_string", f2))
			return f1 == f2;
		return false;
	}

	public static function isObject( v : Dynamic ) : Bool {
		if( v == null )
			return false;
		if(untyped __call__("is_object", v))
			return untyped __php__("$v instanceof _hx_anonymous") || Type.getClass(v) != null;
		return untyped __php__("is_string($v) && !_hx_is_lambda($v)");
	}

	public static function deleteField( o : Dynamic, f : String ) : Bool {
		if(!hasField(o,f)) return false;
		untyped __php__("if(isset($o->»dynamics[$f])) unset($o->»dynamics[$f]); else if($o instanceof _hx_anonymous) unset($o->$f); else $o->$f = null");
		return true;
	}

	public static function copy<T>( o : T ) : T {
		if (untyped __call__("is_string", o)) return o;
		var o2 : Dynamic = {};
		for( f in Reflect.fields(o) )
			Reflect.setField(o2,f,Reflect.field(o,f));
		return o2;
	}

	public static function makeVarArgs( f : Array<Dynamic> -> Dynamic ) : Dynamic {
		return untyped __php__("array(new _hx_lambda(array(&$f), '_hx_make_var_args'), 'execute')");
	}


}
