/*
 * Copyright (c) 2005-2009, The haXe Project Contributors
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
package haxe.rtti;

typedef Path = String

typedef Platforms = List<String>

enum CType {
	CUnknown;
	CEnum( name : Path, params : List<CType> );
	CClass( name : Path, params : List<CType> );
	CTypedef( name : Path, params : List<CType> );
	CFunction( args : List<{ name : String, opt : Bool, t : CType }>, ret : CType );
	CAnonymous( fields : List<ClassField> );
	CDynamic( ?t : CType );
	CAbstract( name : Path, params : List<CType> );
}

typedef PathParams = {
	var path : Path;
	var params : List<CType>;
}

typedef TypeParams = Array<String> // no contraints

enum Rights {
	RNormal;
	RNo;
	RCall( m : String );
	RMethod;
	RDynamic;
	RInline;
}

typedef MetaData = Array<{ name : String, params : Array<String> }>;

typedef ClassField = {
	var name : String;
	var type : CType;
	var isPublic : Bool;
	var isOverride : Bool;
	var doc : String;
	var get : Rights;
	var set : Rights;
	var params : TypeParams;
	var platforms : Platforms;
	var meta : MetaData;
	var line : Null<Int>;
}

typedef TypeInfos = {
	var path : Path;
	var module : Path;
	var file : Null<String>;
	var params : TypeParams;
	var doc : String;
	var isPrivate : Bool;
	var platforms : Platforms;
	var meta : MetaData;
}

typedef Classdef = {> TypeInfos,
	var isExtern : Bool;
	var isInterface : Bool;
	var superClass : PathParams;
	var interfaces : List<PathParams>;
	var fields : List<ClassField>;
	var statics : List<ClassField>;
	var tdynamic : Null<CType>;
}

typedef EnumField = {
	var name : String;
	var args : Null<List<{ name : String, opt : Bool, t : CType }>>;
	var doc : String;
	var platforms : Platforms;
	var meta : MetaData;
}

typedef Enumdef = {> TypeInfos,
	var isExtern : Bool;
	var constructors : List<EnumField>;
}

typedef Typedef = {> TypeInfos,
	var type : CType;
	var types : Hash<CType>; // by platform
}

typedef Abstractdef = {> TypeInfos,
	var subs : Array<CType>;
	var supers : Array<CType>;
}

enum TypeTree {
	TPackage( name : String, full : String, subs : Array<TypeTree> );
	TClassdecl( c : Classdef );
	TEnumdecl( e : Enumdef );
	TTypedecl( t : Typedef );
	TAbstractdecl( a : Abstractdef );
}

typedef TypeRoot = Array<TypeTree>

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
				return leq(function(a,b) {
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
