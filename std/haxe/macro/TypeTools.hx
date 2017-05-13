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
#if hl
@:hlNative("macro")
#end
class TypeTools {

	static function nullable(complexType : ComplexType) : ComplexType return macro : Null<$complexType>;

	static function toField(cf : ClassField) : Field return {
		function varAccessToString(va : VarAccess, getOrSet : String) : String return {
			switch (va) {
				case AccNormal: "default";
				case AccNo: "null";
				case AccNever: "never";
				case AccResolve: throw "Invalid TAnonymous";
				case AccCall: getOrSet;
				case AccInline: "default";
				case AccRequire(_, _): "default";
			}
		}
		if (cf.params.length == 0) {
			name: cf.name,
			doc: cf.doc,
			access: cf.isPublic ? [ APublic ] : [ APrivate ],
			kind: switch([ cf.kind, cf.type ]) {
				case [ FVar(read, write), ret ]:
					FProp(
						varAccessToString(read, "get"),
						varAccessToString(write, "set"),
						toComplexType(ret),
						null);
				case [ FMethod(_), TFun(args, ret) ]:
					FFun({
						args: [
							for (a in args) {
								name: a.name,
								opt: a.opt,
								type: toComplexType(a.t),
							}
						],
						ret: toComplexType(ret),
						expr: null,
					});
				default:
					throw "Invalid TAnonymous";
			},
			pos: cf.pos,
			meta: cf.meta.get(),
		} else {
			throw "Invalid TAnonymous";
		}
	}


	/**
		Returns a syntax-level type corresponding to Type `t`.

		This function is mostly inverse to `ComplexTypeTools.toType`, but may
		lose some information on types that do not have a corresponding syntax
		version, such as monomorphs. In these cases, the result is null.

		If `t` is null, an internal exception is thrown.
	**/
	public static function toComplexType(type : Null<Type>) : Null<ComplexType> return
	{
		#if macro
		Context.toComplexType(type);
		#else
		switch (type) {
			case null:
				null;
			case TMono(_.get() => t):
				t == null ? null : toComplexType(t);
			case TEnum(_.get() => baseType, params):
				TPath(toTypePath(baseType, params));
			case TInst(_.get() => classType, params):
				switch (classType.kind) {
					case KTypeParameter(_):
						TPath({
							name: classType.name,
							pack: [],
						});
					default:
						TPath(toTypePath(classType, params));
				}
			case TType(_.get() => baseType, params):
				TPath(toTypePath(baseType, params));
			case TFun(args, ret):
				TFunction(
					[ for (a in args) a.opt ? nullable(toComplexType(a.t)) : toComplexType(a.t) ],
					toComplexType(ret));
			case TAnonymous(_.get() => { fields: fields }):
				TAnonymous([ for (cf in fields) toField(cf) ]);
			case TDynamic(t):
				if (t == null) {
					macro : Dynamic;
				} else {
					var ct = toComplexType(t);
					macro : Dynamic<$ct>;
				}
			case TLazy(f):
				toComplexType(f());
			case TAbstract(_.get() => baseType, params):
				TPath(toTypePath(baseType, params));
			default:
				throw "Invalid type";
		}
		#end
	}

	static function toTypePath(baseType : BaseType, params : Array<Type>) : TypePath return {
		var module = baseType.module;
		{
			pack: baseType.pack,
			name: module.substring(module.lastIndexOf(".") + 1),
			sub: baseType.name,
			params: [ for (t in params) TPType(toComplexType(t)) ],
		}
	}


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
		Like `follow`, follows all typedefs of `t` to reach the actual type.

		Will however follow also abstracts to their underlying implementation,
		if they are not a @:coreType abstract

		If `t` is null, an internal exception is thrown.

		Usage example:
			var t = Context.typeof(macro new Map<String, String>());
			trace(t); // TAbstract(Map,[TInst(String,[]),TInst(String,[])])
			trace(t.followWithAbstracts()); // TInst(haxe.ds.StringMap, [TInst(String,[])])
	**/
	static public inline function followWithAbstracts( t : Type, once : Bool = false ) : Type
		return Context.followWithAbstracts(t, once);

	/**
		Returns true if `t1` and `t2` unify, false otherwise.
	**/
	static public inline function unify( t1 : Type, t2:Type ) : Bool
		return Context.unify(t1, t2);

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

		This function replaces occurrences of type parameters in `t` if they are
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
		#if neko
		return Context.load("apply_params", 3)(typeParameters, concreteTypes, t);
		#else
		return applyParams(typeParameters, concreteTypes, t);
		#end
	}

	#if !neko
	private static function applyParams( typeParameters:Array<TypeParameter>, concreteTypes:Array<Type>, t:Type ) : Type {
		return null;
	}
	#end


	/**
		Transforms `t` by calling `f` on each of its subtypes.

		If `t` is a compound type, `f` is called on each of its components.

		Otherwise `t` is returned unchanged.

		The following types are considered compound:
			- TInst, TEnum, TType and TAbstract with type parameters
			- TFun
			- TAnonymous

		If `t` or `f` are null, the result is unspecified.
	**/
	static public function map(t:Type, f:Type -> Type):Type {
		return switch(t) {
			case TMono(tm):
				switch(tm.get()) {
					case null: t;
					case t: f(t);
				}
			case TEnum(_, []) | TInst(_, []) | TType(_, []):
				t;
			case TEnum(en, tl):
				TEnum(en, tl.map(f));
			case TInst(cl, tl):
				TInst(cl, tl.map(f));
			case TType(t2, tl):
				TType(t2, tl.map(f));
			case TAbstract(a, tl):
				TAbstract(a, tl.map(f));
			case TFun(args, ret):
				TFun(args.map(function(arg) return {
					name: arg.name,
					opt: arg.opt,
					t: f(arg.t)
				}), f(ret));
			case TAnonymous(an):
				t; // TODO: Ref?
			case TDynamic(t2):
				t == t2 ? t : TDynamic(f(t2));
			case TLazy(ft):
				var ft = ft();
				var ft2 = f(ft);
				ft == ft2 ? t : ft2;
		}
	}

	/**
		Calls function `f` on each component of type `t`.

		If `t` is not a compound type, this operation has no effect.

		The following types are considered compound:
			- TInst, TEnum, TType and TAbstract with type parameters
			- TFun
			- TAnonymous

		If `t` or `f` are null, the result is unspecified.
	**/
	static public function iter(t:Type, f:Type -> Void):Void {
		switch (t) {
			case TMono(tm):
				var t = tm.get();
				if (t != null) f(t);
			case TEnum(_, tl) | TInst(_, tl) | TType(_, tl) | TAbstract(_, tl):
				for (t in tl) f(t);
			case TDynamic(t2):
				if (t != t2) f(t2);
			case TLazy(ft):
				f(ft());
			case TAnonymous(an):
				for (field in an.get().fields) f(field.type);
			case TFun(args, ret):
				for (arg in args) f(arg.t);
				f(ret);
		}
	}

	/**
		Converts type `t` to a human-readable String representation.
	**/
	static public function toString( t : Type ) : String {
		#if neko
		return Context.load("s_type", 1)(t);
		#else
		return null;
		#end
	}

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
}
