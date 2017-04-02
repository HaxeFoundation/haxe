/*
 * Copyright (C)2005-2017 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
@:coreApi class Reflect {

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
			return untyped __call__("_hx_field", o, field);
	}

	public static function setProperty( o : Dynamic, field : String, value : Dynamic ) : Void {
		if (null == o) return null;
		var cls : String = Std.is(o, Class) ? untyped __php__("$o->__tname__") : untyped __call__("get_class", o);
		var cls_vars : php.NativeArray = untyped __call__("get_class_vars", cls);
		if (untyped __php__("isset($cls_vars['__properties__']) && isset($cls_vars['__properties__']['set_'.$field]) && ($field = $cls_vars['__properties__']['set_'.$field])"))
			untyped __php__("$o->$field($value)");
		else
			untyped __setfield__(o, field, value);
	}

	public static function callMethod( o : Dynamic, func : haxe.Constraints.Function, args : Array<Dynamic> ) : Dynamic untyped {
		return __call__("call_user_func_array", __call__("is_callable", func) ? func : __call__("array", o, func), (null == args ? __call__("array") : __field__(args, "a")));
	}

	public static function fields( o : Dynamic ) : Array<String> {
		if( o == null ) return new Array();
		return untyped __php__("$o instanceof _hx_array")
				? __php__("new _hx_array(array('concat','copy','insert','iterator','length','join','pop','push','remove','reverse','shift','slice','sort','splice','toString','unshift'))")
				: (__call__('is_string', o)
					? __php__("new _hx_array(array('charAt','charCodeAt','indexOf','lastIndexOf','length','split','substr','toLowerCase','toString','toUpperCase'))")
					: __php__("new _hx_array(_hx_get_object_vars($o))"));
	}

	public static function isFunction( f : Dynamic ) : Bool {
		return untyped __php__("(is_array($f) && is_callable($f)) || _hx_is_lambda($f)") || (__php__("is_array($f)") && hasField(__php__("$f[0]"), __php__("$f[1]")) && __php__("$f[1]") != "length");
	}

	public static function compare<T>( a : T, b : T ) : Int {
		return ( a == b ) ? 0 : untyped __php__("is_string($a)") ? untyped __php__("strcmp($a, $b)") : (((cast a) > (cast b)) ? 1 : -1);
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
			return untyped __php__("$v instanceof _hx_anonymous") || Type.getClass(v) != null
			  || untyped __php__("$v instanceof _hx_class")
			  || untyped __php__("$v instanceof _hx_enum");
		return untyped __php__("is_string($v) && !_hx_is_lambda($v)");
	}

	public static function isEnumValue( v : Dynamic ) : Bool {
		return untyped __php__("$v instanceof Enum");
	}

	public static function deleteField( o : Dynamic, field : String ) : Bool {
		if(!hasField(o,field)) return false;
		untyped __php__("if(isset($o->__dynamics[$field])) unset($o->__dynamics[$field]); else if($o instanceof _hx_anonymous) unset($o->$field); else $o->$field = null");
		return true;
	}

	public static function copy<T>( o : T ) : T {
		if (untyped __call__("is_string", o)) return o;
		var o2 : Dynamic = {};
		for( f in Reflect.fields(o) )
			Reflect.setField(o2,f,Reflect.field(o,f));
		return o2;
	}

	@:overload(function( f : Array<Dynamic> -> Void ) : Dynamic {})
	public static function makeVarArgs( f : Array<Dynamic> -> Dynamic ) : Dynamic {
		return untyped __php__("array(new _hx_lambda(array(&$f), '_hx_make_var_args'), 'execute')");
	}


}
