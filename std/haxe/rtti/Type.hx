package haxe.rtti;

typedef Path = String

typedef Platforms = List<String>

enum Type {
	TUnknown;
	TEnum( name : Path, params : List<Type> );
	TClass( name : Path, params : List<Type> );
	TTypedef( name : Path, params : List<Type> );
	TFunction( args : List<{ name : String, opt : Bool, t : Type }>, ret : Type );
	TAnonymous( fields : List<{ name : String, t : Type  }> );
	TDynamic( ?t : Type );
}

typedef PathParams = {
	var path : Path;
	var params : List<Type>;
}

typedef TypeParams = Array<String> // no contraints

enum Rights {
	RNormal;
	RNo;
	RMethod( m : String );
	RDynamic;
	RF9Dynamic;
}

typedef ClassField = {
	var name : String;
	var type : Type;
	var isPublic : Bool;
	var doc : String;
	var get : Rights;
	var set : Rights;
	var params : TypeParams;
	var platforms : Platforms;
}

typedef TypeInfos = {
	var path : Path;
	var module : Path;
	var params : TypeParams;
	var doc : String;
	var isPrivate : Bool;
	var platforms : Platforms;
}

typedef Classdef = {> TypeInfos,
	var isExtern : Bool;
	var isInterface : Bool;
	var superClass : PathParams;
	var interfaces : List<PathParams>;
	var fields : List<ClassField>;
	var statics : List<ClassField>;
	var tdynamic : Null<Type>;
}

typedef EnumField = {
	var name : String;
	var args : Null<List<{ name : String, opt : Bool, t : Type }>>;
	var doc : String;
	var platforms : Platforms;
}

typedef Enumdef = {> TypeInfos,
	var isExtern : Bool;
	var constructors : List<EnumField>;
}

typedef Typedef = {> TypeInfos,
	var type : Type;
	var types : Hash<Type>; // by platform
}

enum TypeTree {
	TPackage( name : String, full : String, subs : Array<TypeTree> );
	TClassdecl( c : Classdef );
	TEnumdecl( e : Enumdef );
	TTypedecl( t : Typedef );
}

typedef TypeRoot = Array<TypeTree>

class TypeApi {

	public static function typeInfos( t : TypeTree ) : TypeInfos {
		var inf : TypeInfos;
		switch( t ) {
		case TClassdecl(c): inf = c;
		case TEnumdecl(e): inf = e;
		case TTypedecl(t): inf = t;
		case TPackage(_,_,_): throw "Unexpected Package";
		}
		return inf;
	}

	public static function isVar( t : Type ) {
		return switch( t ) {
		case TFunction(_,_): false;
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
		case RMethod(m1):
			switch( r2 ) {
			case RMethod(m2):
				return m1 == m2;
			default:
			}
		default:
		}
		return false;
	}

	public static function typeEq( t1 : Type, t2 : Type ) {
		switch( t1 ) {
		case TUnknown: return t2 == TUnknown;
		case TEnum(name,params):
			switch( t2 ) {
			case TEnum(name2,params2):
				return name == name2 && leq(typeEq,params,params2);
			default:
			}
		case TClass(name,params):
			switch( t2 ) {
			case TClass(name2,params2):
				return name == name2 && leq(typeEq,params,params2);
			default:
			}
		case TTypedef(name,params):
			switch( t2 ) {
			case TTypedef(name2,params2):
				return name == name2 && leq(typeEq,params,params2);
			default:
			}
		case TFunction(args,ret):
			switch( t2 ) {
			case TFunction(args2,ret2):
				return leq(function(a,b) {
					return a.name == b.name && a.opt == b.opt && typeEq(a.t,b.t);
				},args,args2) && typeEq(ret,ret2);
			default:
			}
		case TAnonymous(fields):
			switch( t2 ) {
			case TAnonymous(fields2):
				return leq(function(a,b) {
					return a.name == b.name && typeEq(a.t,b.t);
				},fields,fields2);
			default:
			}
		case TDynamic(t):
			switch( t2 ) {
			case TDynamic(t2):
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
