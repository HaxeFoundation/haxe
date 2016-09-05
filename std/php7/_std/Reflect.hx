/*
 * Copyright (C)2005-2016 Haxe Foundation
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

import php7.Boot;
import haxe.Constraints;

using php7.Global;

@:coreApi class Reflect {

	public static function hasField( o : Dynamic, field : String ) : Bool {
		return o.is_object() && o.property_exists(field);
	}

	public static function field( o : Dynamic, field : String ) : Dynamic {
		if (hasField(o, field)) {
			return untyped __php__("$o->$field");
		} else {
			return null;
		}
	}

	public static function setField( o : Dynamic, field : String, value : Dynamic ) : Void {
		return untyped __php__("$o->$field = $value");
	}

	public static function getProperty( o : Dynamic, field : String ) : Dynamic {
		if (o.is_object()) {
			if (Boot.hasGetter(o.get_class(), field)) {
				return untyped __php__("$o->{'get_$field'}()");
			} else {
				return untyped __php__("$o->$field");
			}
		}
		if (o.is_string() && field == 'length') {
			return Global.strlen(o);
		}
		return null;
	}

	public static function setProperty( o : Dynamic, field : String, value : Dynamic ) : Void {
		if (o.is_object()) {
			if (Boot.hasSetter(o.get_class(), field)) {
				untyped __php__("$o->{'set_$field'}($value)");
			} else {
				untyped __php__("$o->$field = $value");
			}
		}
	}

	public static function callMethod( o : Dynamic, func : Function, args : Array<Dynamic> ) : Dynamic {
		if (untyped __php__("$func instanceof \\Closure")) {
			if (o != null) {
				func = untyped func.bindTo(o);
			}
			return Global.call_user_func_array(func, @:privateAccess args.arr);
		} else {
			return untyped func.callWith(o, @:privateAccess args.arr);
		}
	}

	public static function fields( o : Dynamic ) : Array<String> {
		if (untyped __php__("$o instanceof \\StdClass")) {
			return @:privateAccess Array.wrap(Global.get_object_vars(o).array_keys());
		}
		return [];
	}

	public static function isFunction( f : Dynamic ) : Bool {
		return untyped __php__("$f instanceof \\Closure");
	}

	public static function compare<T>( a : T, b : T ) : Int {
		if (a == b) return 0;
		if (Global.is_string(a)){
			return Global.strcmp(cast a, cast b);
		} else {
			return ((cast a) > (cast b) ? 1 : -1);
		}
	}

	public static function compareMethods( f1 : Dynamic, f2 : Dynamic ) : Bool {
		var hxClosure = Boot.closureHxClass().phpClassName;
		if (untyped __php__("$f1 instanceof $hxClosure && $f2 instanceof $hxClosure")) {
			return f1.equals(f2);
		} else {
			return f1 == f2;
		}
	}

	public static function isObject( v : Dynamic ) : Bool {
		var hxEnum = cast Enum;
		if (untyped __php__("$v instanceof $hxEnum->phpClassName")) {
			return false;
		} else {
			return v.is_object() || v.is_string();
		}
	}

	public static function isEnumValue( v : Dynamic ) : Bool {
		var hxEnum = cast Enum;
		return untyped __php__("$v instanceof $hxEnum->phpClassName");
	}

	public static function deleteField( o : Dynamic, field : String ) : Bool {
		if (hasField(o, field)) {
			untyped __php__("unset($o->$field)");
			return true;
		} else {
			return false;
		}
	}

	public static function copy<T>( o : T ) : T {
		if (Global.is_object(o)) {
			var fields = Global.get_object_vars(cast o);
			var hxAnon = Boot.getHxAnon().phpClassName;
			return untyped __php__("new $hxAnon($fields)");
		} else {
			return null;
		}
	}

	@:overload(function( f : Array<Dynamic> -> Void ) : Dynamic {})
	public static function makeVarArgs( f : Array<Dynamic> -> Dynamic ) : Dynamic {
		return function () {
			return Global.call_user_func_array(f, Global.func_get_args());
		}
	}


}
