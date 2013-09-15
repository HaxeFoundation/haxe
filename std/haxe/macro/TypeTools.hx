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
		Follows all typedefs of `t` to reach the actual type.
		
		If `once` is true, this function does not call itself recursively,
		otherwise it does. This can be useful in cases where intermediate
		typedefs might be of interest.
		
		Affected types are monomorphs `TMono` and typedefs `TType(t,pl)`.
		
		If `t` is null, an internal exception is thrown.
		
		Usage example:
			var t = Context.typeof(macro null); // TMono(<mono>)
			var ts = Context.typeof(macro "foo"); //TInst(String,[])
			Context.unify(t, ts);
			trace(t); // TMono(<mono>)
			trace(t.follow()); //TInst(String,[])
	**/
	static public inline function follow( t : Type, ?once : Bool ) : Type
		return Context.follow(t, once);
		
	/**
		Returns true if `t1` and `t2` unify, false otherwise.
	**/
	static public inline function unify( t1 : Type, t2:Type ) : Bool
		return Context.unify(t1, t2);
		
	/**
		Returns a syntax-level type corresponding to Type `t`.
		
		This function is mostly inverse to `ComplexTypeTools.toType`, but may
		lose some information on types that do not have a corresponding syntax
		version, such as monomorphs. In these cases, the result is null.
		
		If `t` is null, an internal exception is thrown.
	**/
	static public inline function toComplexType( t : Type ) : ComplexType
		return Context.toComplexType(t);
		
	/**
		Tries to extract the class instance stored inside `t`.
		
		If `t` is a class instance `TInst(c,pl)`, c is returned.
		
		If `t` is of a different type, an exception of type String is thrown.

		If `t` is null, the result is null.
	**/
	static public function getClass( t : Type ) return t == null ? null : switch(follow(t)) {
		case TInst(c, _): c.get();
		case _: throw "Class instance expected";
	}
	
	/**
		Tries to extract the enum instance stored inside `t`.
		
		If `t` is an enum instance `TEnum(e,pl)`, e is returned.
		
		If `t` is of a different type, an exception of type String is thrown.

		If `t` is null, the result is null.
	**/
	static public function getEnum( t : Type ) return t == null ? null : switch(follow(t)) {
		case TEnum(e, _): e.get();
		case _: throw "Enum instance expected";
	}

	/**
		Applies the type parameters `typeParameters` to type `t` with the given
		types `concreteTypes`.
		
		This function replaces occurences of type parameters in `t` if they are
		part of `typeParameters`. The array index of such a type parameter is
		then used to lookup the concrete type in `concreteTypes`.
		
		If `typeParameters.length` is not equal to `concreteTypes.length`, an
		exception of type `String` is thrown.
		
		If `typeParameters.length` is 0, `t` is returned unchanged.
		
		If either argument is `null`, the result is unspecified.
	**/
	static public function applyTypeParameters(t:Type, typeParameters:Array<TypeParameter>, concreteTypes:Array<Type>):Type {
		if (typeParameters.length != concreteTypes.length)
			throw 'Incompatible arguments: ${typeParameters.length} type parameters and ${concreteTypes.length} concrete types';
		else if (typeParameters.length == 0)
			return t;
		return Context.load("apply_params", 3)(typeParameters.map(function(tp) return {name:untyped tp.name.__s, t:tp.t}), concreteTypes, t);
	}
	
	/**
		Converts type `t` to a human-readable String representation.
	**/
	static public function toString( t : Type ) : String return new String(Context.load("s_type", 1)(t));
	#end
	
	/**
		Resolves the field named `name` on class `c`.
		
		If `isStatic` is true, the classes' static fields are checked. Otherwise
		the classes' member fields are checked.
		
		If the field is found, it is returned. Otherwise if `c` has a super
		class, `findField` recursively checks that super class. Otherwise null
		is returned.
		
		If any argument is null, the result is unspecified.
	**/
	static public function findField(c:ClassType, name:String, isStatic:Bool = false):Null<ClassField> {
		var field = (isStatic ? c.statics : c.fields).get().find(function(field) return field.name == name);
		return if(field != null) field;
			else if (c.superClass != null) findField(c.superClass.t.get(), name, isStatic);
			else null;
	}
	
	/**
		Gets the value of a reference `r`.
		
		If `r` is null, the result is unspecified. Otherwise `r.get()` is
		called.
	**/
	static inline function deref<T>(r:Ref<T>):T {
		return r.get();
	}
}