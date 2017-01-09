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

/**
	The (dot-)path of the runtime type.
**/
typedef Path = String

/**
	A list of strings representing the targets where the type is available.
**/
typedef Platforms = List<String>

/**
	The function argument runtime type information.
**/
typedef FunctionArgument = { name : String, opt : Bool, t : CType, ?value:String }

/**
	The runtime member types.
**/
enum CType {
	CUnknown;
	CEnum( name : Path, params : List<CType> );
	CClass( name : Path, params : List<CType> );
	CTypedef( name : Path, params : List<CType> );
	CFunction( args : List<FunctionArgument>, ret : CType );
	CAnonymous( fields : List<ClassField> );
	CDynamic( ?t : CType );
	CAbstract( name : Path, params : List<CType> );
}

/**
	The type parameters in the runtime type information.
**/
typedef PathParams = {
	/**
		The path of the type.
	**/
	var path : Path;

	/**
		The list of parameters types.
	**/
	var params : List<CType>;
}

/**
	An array of strings representing the names of the type parameters the type 
	has. As of Haxe 3.2.0, this does not include the constraints.
**/
typedef TypeParams = Array<String> // no constraints

/**
	Represents the runtime rights of a type.
**/
enum Rights {
	RNormal;
	RNo;
	RCall( m : String );
	RMethod;
	RDynamic;
	RInline;
}

/**
	The list of runtime metadata.
**/
typedef MetaData = Array<{ name : String, params : Array<String> }>;

/**
	The runtime class field information.
	
	@see <https://haxe.org/manual/cr-rtti-structure.html#class-field-information>
**/
typedef ClassField = {
	/**
		The name of the field.
	**/
	var name : String;

	/**
		The type of the field.
	**/
	var type : CType;

	/**
		Whether or not the field is public.
	**/
	var isPublic : Bool;

	/**
		Whether or not the field overrides another field.
	**/
	var isOverride : Bool;

	/**
		The documentation of the field. This information is only available 
		if the compiler flag `-D use_rtti_doc` was in place. Otherwise, or 
		if the field has no documentation, the value is `null`.
	**/
	var doc : Null<String>;

	/**
		The [read access](https://haxe.org/manual/dictionary.html#define-read-access) 
		behavior of the field.
	**/
	var get : Rights;

	/**
		The [write access](https://haxe.org/manual/dictionary.html#define-write-access)
		behavior of the field.
	**/
	var set : Rights;

	/**
		An array of strings representing the names of the type parameters 
		the field has. 
	**/
	var params : TypeParams;

	/**
		A list of strings representing the targets where the field is available.
	**/
	var platforms : Platforms;

	/**
		The meta data the field was annotated with.
	**/
	var meta : MetaData;

	/**
		The line number where the field is defined. This information is only 
		available if the field has an expression. 
		Otherwise the value is `null`.
	**/
	var line : Null<Int>;

	/**
		The list of available overloads for the fields or `null` if no overloads 
		exists.
	**/
	var overloads : Null<List<ClassField>>;

	/**
		The actual expression of the field or `null` if there is no expression. 
	**/
	var expr : Null<String>;
}

/**
	The general runtime type information.
**/
typedef TypeInfos = {
	/**
		The type path of the type.
	**/
	var path : Path;

	/**
		The type path of the module containing the type.
	**/
	var module : Path;

	/**
		The full slash path of the .hx file containing the type. 
		This might be `null` in case there is no such file, e.g. if the
		type is defined through a macro.
	**/
	var file : Null<String>;

	/**
		An array of strings representing the names of the type parameters the 
		type has. 
	**/
	var params : TypeParams;

	/**
		The documentation of the type. This information is only available
		if the compiler flag `-D use_rtti_doc` was in place. Otherwise, or if
		the constructor has no documentation, the value is `null`.
	**/
	var doc : Null<String>;

	/**
		Whether or not the type is [private](https://haxe.org/manual/dictionary.html#define-private-type).
	**/
	var isPrivate : Bool;

	/**
		A list of strings representing the targets where the type is available.
	**/
	var platforms : Platforms;

	/**
		The [metadata](https://haxe.org/manual/lf-metadata.html) the type was 
		annotated with.
	**/
	var meta : MetaData;
}

/**
	The runtime class definition information.
**/
typedef Classdef = {> TypeInfos,
	/**
		Whether or not the class is [extern](https://haxe.org/manual/lf-externs.html).
	**/
	var isExtern : Bool;

	/**
		Whether or not the class is actually an [interface](https://haxe.org/manual/types-interfaces.html).
	**/
	var isInterface : Bool;

	/**
		The class' parent class defined by its type path and list of type 
		parameters.
	**/
	var superClass : Null<PathParams>;

	/**
		The list of interfaces defined by their type path and list of type 
		parameters.
	**/
	var interfaces : List<PathParams>;

	/**
		The list of member [class fields](https://haxe.org/manual/class-field.html).
	**/
	var fields : List<ClassField>;

	/**
		The list of static class fields.
	**/
	var statics : List<ClassField>;

	/**
		The type which is dynamically implemented by the class or `null` if no
		such type exists.
	**/
	var tdynamic : Null<CType>;
}

/**
	The runtime enum constructor information.
	
	@see <https://haxe.org/manual/cr-rtti-structure.html#enum-constructor-information>
**/
typedef EnumField = {
	/**
		The name of the constructor.
	**/
	var name : String;

	/**
		The list of arguments the constructor has or `null` if no arguments are 
		available.
	**/
	var args : Null<List<{ name : String, opt : Bool, t : CType }>>;

	/**
		The documentation of the constructor. This information is only available
		if the compiler flag `-D use_rtti_doc` was in place. Otherwise, or if
		the constructor has no documentation, the value is `null`.
	**/
	var doc : String;

	/**
		A list of strings representing the targets where the constructor is
		available.
	**/
	var platforms : Platforms;

	/**
		The meta data the constructor was annotated with.
	**/
	var meta : MetaData;
}

/**
	The enum runtime type information.
	
	@see <https://haxe.org/manual/cr-rtti-structure.html#enum-type-information>
**/
typedef Enumdef = {> TypeInfos,
	/**
		Whether or not the enum is [extern](https://haxe.org/manual/lf-externs.html).
	**/
	var isExtern : Bool;

	/**
		The list of enum constructors.
	**/
	var constructors : List<EnumField>;
}

/**
	The typedef runtime information.
**/
typedef Typedef = {> TypeInfos,
	/**
		The type of the typedef.
	**/
	var type : CType;

	/**
		The types of the typedef, by platform.
	**/
	var types : Map<String,CType>; // by platform
}

/**
	The abstract type runtime information.
	
	@see <https://haxe.org/manual/cr-rtti-structure.html#abstract-type-information>
**/
typedef Abstractdef = {> TypeInfos,
	var to : Array<{t:CType, field:Null<String>}>;
	var from : Array<{t:CType, field:Null<String>}>;
	var impl : Classdef;
	var athis : CType;
}

/**
	The tree types of the runtime type.
**/
enum TypeTree {
	TPackage( name : String, full : String, subs : Array<TypeTree> );
	TClassdecl( c : Classdef );
	TEnumdecl( e : Enumdef );
	TTypedecl( t : Typedef );
	TAbstractdecl( a : Abstractdef );
}

/**
	List of `TypeTree`.
**/
typedef TypeRoot = Array<TypeTree>

/**
	Contains type and equality checks functionalities for RTTI.
**/
class TypeApi {

	public static function typeInfos( t : TypeTree ) : TypeInfos {
		var inf : TypeInfos;
		switch( t ) {
		case TClassdecl(c): inf = c;
		case TEnumdecl(e): inf = e;
		case TTypedecl(t): inf = t;
		case TAbstractdecl(a): inf = a;
		case TPackage(_,_,_): throw "Unexpected Package";
		}
		return inf;
	}

	/**
		Returns `true` if the given `CType` is a variable or `false` if it is a
		function.
	**/
	public static function isVar( t : CType ) {
		return switch( t ) {
		case CFunction(_,_): false;
		default: true;
		}
	}

	static function leq<T>( f : T -> T -> Bool, l1 : List<T>, l2 : List<T> ) {
		var it = l2.iterator();
		for( e1 in l1 ) {
			if( !it.hasNext() )
				return false;
			var e2 = it.next();
			if( !f(e1,e2) )
				return false;
		}
		if( it.hasNext() )
			return false;
		return true;
	}

	/**
		Unlike `r1 == r2`, this function performs a deep equality check on 
		the given `Rights` instances.

		If `r1` or `r2` are `null`, the result is unspecified.
	**/
	public static function rightsEq( r1 : Rights, r2 : Rights ) {
		if( r1 == r2 )
			return true;
		switch( r1 ) {
		case RCall(m1):
			switch( r2 ) {
			case RCall(m2):
				return m1 == m2;
			default:
			}
		default:
		}
		return false;
	}

	/**
		Unlike `t1 == t2`, this function performs a deep equality check on 
		the given `CType` instances.

		If `t1` or `t2` are `null`, the result is unspecified.
	**/
	public static function typeEq( t1 : CType, t2 : CType ) {
		switch( t1 ) {
		case CUnknown: return t2 == CUnknown;
		case CEnum(name,params):
			switch( t2 ) {
			case CEnum(name2,params2):
				return name == name2 && leq(typeEq,params,params2);
			default:
			}
		case CClass(name,params):
			switch( t2 ) {
			case CClass(name2,params2):
				return name == name2 && leq(typeEq,params,params2);
			default:
			}
		case CAbstract(name,params):
			switch( t2 ) {
			case CAbstract(name2,params2):
				return name == name2 && leq(typeEq,params,params2);
			default:
			}
		case CTypedef(name,params):
			switch( t2 ) {
			case CTypedef(name2,params2):
				return name == name2 && leq(typeEq,params,params2);
			default:
			}
		case CFunction(args,ret):
			switch( t2 ) {
			case CFunction(args2,ret2):
				return leq(function(a:FunctionArgument,b:FunctionArgument) {
					return a.name == b.name && a.opt == b.opt && typeEq(a.t,b.t);
				},args,args2) && typeEq(ret,ret2);
			default:
			}
		case CAnonymous(fields):
			switch( t2 ) {
			case CAnonymous(fields2):
				return leq(function(a,b) return fieldEq(a,b),fields,fields2);
			default:
			}
		case CDynamic(t):
			switch( t2 ) {
			case CDynamic(t2):
				if( (t == null) != (t2 == null) )
					return false;
				return t == null || typeEq(t,t2);
			default:
			}
		}
		return false;
	}

	/**
		Unlike `f1 == f2`, this function performs a deep equality check on 
		the given `ClassField` instances.

		If `f1` or `f2` are `null`, the result is unspecified.
	**/
	public static function fieldEq( f1 : ClassField, f2 : ClassField ) {
		if( f1.name != f2.name )
			return false;
		if( !typeEq(f1.type,f2.type) )
			return false;
		if( f1.isPublic != f2.isPublic )
			return false;
		if( f1.doc != f2.doc )
			return false;
		if( !rightsEq(f1.get,f2.get) )
			return false;
		if( !rightsEq(f1.set,f2.set) )
			return false;
		if( (f1.params == null) != (f2.params == null) )
			return false;
		if( f1.params != null && f1.params.join(":") != f2.params.join(":") )
			return false;
		return true;
	}

	/**
		Unlike `c1 == c2`, this function performs a deep equality check on 
		the arguments of the enum constructors, if exists.

		If `c1` or `c2` are `null`, the result is unspecified.
	**/
	public static function constructorEq( c1 : EnumField, c2: EnumField ) {
		if( c1.name != c2.name )
			return false;
		if( c1.doc != c2.doc )
			return false;
		if( (c1.args == null) != (c2.args == null) )
			return false;
		if( c1.args != null && !leq(function(a,b) { return a.name == b.name && a.opt == b.opt && typeEq(a.t,b.t); },c1.args,c2.args) )
			return false;
		return true;
	}

}

/**
	The CTypeTools class contains some extra functionalities for handling
	`CType` instances.
**/
class CTypeTools {
	/**
		Get the string representation of `CType`.
	**/
	static public function toString(t:CType):String {
		return switch (t) {
			case CUnknown:
				"unknown";
			case CClass(name, params), CEnum(name, params), CTypedef(name, params), CAbstract(name, params):
				nameWithParams(name, params);
			case CFunction(args, ret):
				if (args.length == 0) {
					"Void -> " +toString(ret);
				} else {
					args.map(functionArgumentName).join(" -> ")+" -> "+toString(ret);
				}
			case CDynamic(d):
				if (d == null) {
					"Dynamic";
				} else {
					"Dynamic<" + toString(d) + ">";
				}
			case CAnonymous(fields):
				"{ " + fields.map(classField).join(", ");
		}
	}

	static function nameWithParams(name:String, params:List<CType>) {
		if (params.length == 0) {
			return name;
		}
		return name + "<" + params.map(toString).join(", ") + ">";
	}

	static function functionArgumentName(arg:FunctionArgument) {
		return (arg.opt ? "?" : "") + (arg.name == "" ? "" : arg.name + ":") + toString(arg.t) + (arg.value == null ? "" : " = " +arg.value);
	}

	static function classField(cf:ClassField) {
		return cf.name + ":" +toString(cf.type);
	}
}