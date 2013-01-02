/*
 * Copyright (C)2005-2013 Haxe Foundation
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

package haxe.macro;

import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;

using Lambda;

/**
	This class provides some utility methods to work with types. It is
	best used through 'using haxe.macro.TypeTools' syntax and then provides
	additional methods on haxe.macro.Type instances.
**/
class TypeTools {
	#if macro
	
	/**
		Follows all typedefs of [t] to reach the actual type.
		
		If [once] is true, this function does not call itself recursively,
		otherwise it does. This can be useful in cases where intermediate
		typedefs might be of interest.
		
		Affected types are monomorphs (TMono) and typedefs (TType(t,pl)).
		
		If [t] is null, an internal exception is thrown.
		
		Usage example:
			var t = Context.typeof(macro null); // TMono(<mono>)
			var ts = Context.typeof(macro "foo"); //TInst(String,[])
			Context.unify(t, ts);
			trace(t); // TMono(<mono>)
			trace(t.follow()); //TInst(String,[])		
	**/
	static public inline function follow( t : Type, ?once : Bool ) : Type
		return Context.follow(t, once)
		
	/**
		Returns a syntax-level type corresponding to Type [t].
		
		This function is mostly inverse to ComplexTypeTools.toType(), but may
		lose some information on types that do not have a corresponding syntax
		version, such as monomorphs. In these cases, the result is null.
		
		If [t] is null, an internal exception is thrown.
	**/
	static public inline function toComplexType( t : Type ) : ComplexType
		return Context.toComplexType(t)
		
	/**
		Tries to extract the class instance stored inside [t].
		
		If [t] is a class instance TInst(c,pl), c is returned.
		
		If [t] is of a different type, an exception of type TypeError is thrown.

		If [t] is null, an internal exception is thrown.
	**/
	static public function getClass( t : Type ) return switch(follow(t)) {
		case TInst(c, _): c.get();
		case _: throw InvalidType(t, "Class instance expected");
	}
	
	/**
		Tries to extract the enum instance stored inside [t].
		
		If [t] is an enum instance TEnum(e,pl), e is returned.
		
		If [t] is of a different type, an exception of type TypeError is thrown.

		If [t] is null, an internal exception is thrown.
	**/	
	static public function getEnum( t : Type ) return switch(follow(t)) {
		case TEnum(e, _): e.get();
		case _: throw InvalidType(t, "Enum instance expected");
	}	

	/**
		Converts type [t] to a human-readable String representation.
	**/
	static public function toString( t : Type ) : String return Context.load("s_type", 1)(t)
	#end
	
}