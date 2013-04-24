/*
 * Copyright (C)2005-2012 Haxe Foundation
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

/**
	An api to access classes and enums metadata at runtime.
**/
class Meta {

	/**
		Returns the metadata that were declared for the given type (class or enum)
	**/
	public static function getType( t : Dynamic ) : Dynamic<Array<Dynamic>> {
		#if (java || cs)
		var meta : Dynamic = Reflect.field(t, "__meta__");
		#else
		var meta : Dynamic = untyped t.__meta__;
		#end
		return (meta == null || meta.obj == null) ? {} : meta.obj;
	}

	/**
		Returns the metadata that were declared for the given class static fields
	**/	
	public static function getStatics( t : Dynamic ) : Dynamic<Dynamic<Array<Dynamic>>> {
		#if (java || cs)
		var meta : Dynamic = Reflect.field(t, "__meta__");
		#else
		var meta : Dynamic = untyped t.__meta__;
		#end
		return (meta == null || meta.statics == null) ? {} : meta.statics;
	}

	/**
		Returns the metadata that were declared for the given class fields or enum constructors
	**/
	public static function getFields( t : Dynamic ) : Dynamic<Dynamic<Array<Dynamic>>> {
		#if (java || cs)
		var meta : Dynamic = Reflect.field(t, "__meta__");
		#else
		var meta : Dynamic = untyped t.__meta__;
		#end
		return (meta == null || meta.fields == null) ? {} : meta.fields;
	}

}