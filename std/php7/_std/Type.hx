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

import php7.Global;
import php7.Boot;
import php7.reflection.ReflectionClass;
import haxe.extern.EitherType;

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

@:coreApi class Type {

	public static function getClass<T>( o : T ) : Class<T> {
		if(Global.is_object(o) && !Boot.isClass(o) && !Boot.isEnumValue(o)) {
			return cast Boot.getClass(Global.get_class(cast o));
		} else if(Boot.is(o, cast String)) {
			return cast String;
		} else {
			return null;
		}
	}

	public static function getEnum( o : EnumValue ) : Enum<Dynamic> {
		if(o == null) return null;
		return cast Boot.getClass(Global.get_class(cast o));
	}

	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic> {
		if(c == null) return null;
		var parentClass = Global.get_parent_class((cast c).phpClassName);
		if(!parentClass) return null;
		return cast Boot.getClass(parentClass);
	}

	public static function getClassName( c : Class<Dynamic> ) : String {
		if(c == null) return null;
		return Boot.getHaxeName(cast c);
	}

	public static function getEnumName( e : Enum<Dynamic> ) : String {
		return getClassName(cast e);
	}

	public static function resolveClass( name : String ) : Class<Dynamic> {
		if (name == null) return null;
		if (name == 'String') return String;

		var phpClass = Boot.getPhpName(name);
		if (!Global.class_exists(phpClass)) return null;

		var hxClass = Boot.getClass(phpClass);
		if (Boot.is(hxClass, Boot.getClass('Enum'))) return null;

		return cast hxClass;
	}

	public static function resolveEnum( name : String ) : Enum<Dynamic> {
		if (name == null) return null;
		
		var phpClass = Boot.getPhpName(name);
		if (!Global.class_exists(phpClass)) return null;

		var hxClass = Boot.getClass(phpClass);
		if (!Boot.is(hxClass, Boot.getClass('Enum'))) return null;

		return cast hxClass;
	}

	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T {
		if (String == cast cl) return args[0];

		var phpName = getPhpName(cl);
		if (phpName == null) return null;
		
		return untyped __php__("new $phpName(...$args->arr)");
	}

	public static function createEmptyInstance<T>( cl : Class<T> ) : T {
		if (String == cast cl) return cast '';
		if (Array == cast cl) return cast [];

		var phpName = getPhpName(cl);
		if (phpName == null) return null;
		
		var reflection = new ReflectionClass(phpName);
		return reflection.newInstanceWithoutConstructor();
	}

	public static function createEnum<T>( e : Enum<T>, constr : String, ?params : Array<Dynamic> ) : T {
		if (e == null || constr == null) return null;

		var phpName = getPhpName(e);
		if (phpName == null) return null;

		if (params == null) {
			return untyped __php__("$phpName::$constr()");
		} else {
			return untyped __php__("$phpName::$constr(...$params->arr)");
		}
	}

	public static function createEnumIndex<T>( e : Enum<T>, index : Int, ?params : Array<Dynamic> ) : T {
		if (e == null || index == null) return null;

		var phpName = getPhpName(e);
		if (phpName == null) return null;

		var constr = untyped __php__("$phpName::__hx__list()[$index]");
		if (constr == null) return null;

		if (params == null) {
			return untyped __php__("$phpName::$constr()");
		} else {
			return untyped __php__("$phpName::$constr(...$params->arr)");
		}
	}

	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String> {
		if(untyped c.__qname__ == 'String') return ['substr', 'charAt', 'charCodeAt', 'indexOf', 'lastIndexOf', 'split', 'toLowerCase', 'toUpperCase', 'toString', 'length'];
		if(untyped c.__qname__ == 'Array') return  ['push', 'concat', 'join', 'pop', 'reverse', 'shift', 'slice', 'sort', 'splice', 'toString', 'copy', 'unshift', 'insert', 'remove', 'iterator', 'length'];
		untyped __php__("
		$rfl = $c->__rfl__();
		if($rfl === null) return new _hx_array(array());
		$r = array();
		$internals = array('__construct', '__call', '__get', '__set', '__isset', '__unset', '__toString');
		$ms = $rfl->getMethods();
		while(list(, $m) = each($ms)) {
			$n = $m->getName();
			if(!$m->isStatic() && !in_array($n, $internals)) $r[] = $n;
		}
		$ps = $rfl->getProperties();
		while(list(, $p) = each($ps))
			if(!$p->isStatic() && ($name = $p->getName()) !== '__dynamics') $r[] = $name;
		");
		return untyped __php__("new _hx_array(array_values(array_unique($r)))");
	}

	public static function getClassFields( c : Class<Dynamic> ) : Array<String> {
		if(untyped c.__qname__ == 'String') return ['fromCharCode'];
		if(untyped c.__qname__ == 'Array')  return [];
		untyped __php__("
		$rfl = $c->__rfl__();
		if($rfl === null) return new _hx_array(array());
		$ms = $rfl->getMethods(ReflectionMethod::IS_STATIC);
		$r = array();
		while(list(, $m) = each($ms)) {
			$cls = $m->getDeclaringClass();
			if($cls->getName() == $c->__tname__) $r[] = $m->getName();
		}
		$ps = $rfl->getProperties(ReflectionMethod::IS_STATIC);
		while(list(, $p) = each($ps)) {
			$cls = $p->getDeclaringClass();
			if($cls->getName() == $c->__tname__ && ($name = $p->getName()) !== '__properties__') $r[] = $name;
		}
		");
		return untyped __php__("new _hx_array(array_values(array_unique($r)))");
	}

	public static function getEnumConstructs( e : Enum<Dynamic> ) : Array<String> untyped {
		if (__php__("$e->__tname__ == 'Bool'")) return ['true', 'false'];
		if (__php__("$e->__tname__ == 'Void'")) return [];
		return __call__("new _hx_array", e.__constructors);
	}

	public static function typeof( v : Dynamic ) : ValueType untyped {
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
	}

	public static function enumEq<T>( a : T, b : T ) : Bool untyped {
		if( a == b )
			return true;
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
		return true;
	}

	public static function enumConstructor( e : EnumValue ) : String {
		return untyped e.tag;
	}

	public static function enumParameters( e : EnumValue ) : Array<Dynamic> untyped {
		if(e.params == null)
			return [];
		else
			return __php__("new _hx_array($e->params)");
	}

	public inline static function enumIndex( e : EnumValue ) : Int {
		return untyped e.index;
	}

	public static function allEnums<T>( e : Enum<T> ) : Array<T> {
		var all = [];
		for( c in getEnumConstructs(e) ) {
			var v = Reflect.field(e,c);
			if( !Reflect.isFunction(v) )
				all.push(v);
		}
		return all;
	}

	/**
		Get corresponding PHP name for specified `type`.
		Returns `null` if `type` does not exist.
	**/
	static function getPhpName( type:EitherType<Class<Dynamic>,Enum<Dynamic>> ) : Null<String> {
		var haxeName = Boot.getHaxeName(cast type);
		
		return (haxeName == null ? null : Boot.getPhpName(haxeName));		 
	}
}

