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
package haxe.rtti;

private typedef MetaObject = {
	?fields:Dynamic<Dynamic<Null<Array<Dynamic>>>>,
	?statics:Dynamic<Dynamic<Null<Array<Dynamic>>>>,
	?obj:Dynamic<Null<Array<Dynamic>>>,
}

/**
	An API to access classes and enums metadata at runtime.

	@see <https://haxe.org/manual/cr-rtti.html>
**/
class Meta {

	/**
		Returns the metadata that were declared for the given type (class or enum)
	**/
	public static function getType( t : Dynamic ) : Dynamic<Array<Dynamic>> {
		var meta = getMeta(t);
		return (meta == null || meta.obj == null) ? {} : meta.obj;
	}

	// Could move this to Type.hx?
	private static function isInterface(t:Dynamic):Bool {
		#if java
			return java.Lib.toNativeType(t).isInterface();
	#elseif cs
			return cs.Lib.toNativeType(t).IsInterface;
		#elseif (flash && as3)
			return untyped flash.Lib.describeType(t).factory.extendsClass.length() == 0;
		#elseif (php && !php7)
			return untyped __php__("{0} instanceof _hx_interface", t);
		#else
			throw "Something went wrong";
		#end
	}

	private static function getMeta(t:Dynamic):MetaObject
	{
#if (php && php7)
		return php.Boot.getMeta(t.phpClassName);
#elseif (java || cs || php || (flash && as3))
		#if php
		t.__ensureMeta__();
		#end
		var ret = Reflect.field(t, "__meta__");
		if (ret == null && Std.is(t,Class))
		{
			if (isInterface(t))
			{
				var name = Type.getClassName(t),
				    cls = Type.resolveClass(name + '_HxMeta');
				if (cls != null)
					return Reflect.field(cls, "__meta__");
			}
		}
		return ret;
#elseif hl
		var t : hl.BaseType = t;
		return t.__meta__;
#else
		return untyped t.__meta__;
#end
	}

	/**
		Returns the metadata that were declared for the given class static fields
	**/
	public static function getStatics( t : Dynamic ) : Dynamic<Dynamic<Array<Dynamic>>> {
		var meta = getMeta(t);
		return (meta == null || meta.statics == null) ? {} : meta.statics;
	}

	/**
		Returns the metadata that were declared for the given class fields or enum constructors
	**/
	public static function getFields( t : Dynamic ) : Dynamic<Dynamic<Array<Dynamic>>> {
		var meta = getMeta(t);
		return (meta == null || meta.fields == null) ? {} : meta.fields;
	}

}
