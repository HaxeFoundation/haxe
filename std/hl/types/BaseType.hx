package hl.types;

@:keep
class BaseType {
	public var __type__ : Type;
	public var __meta__ : Dynamic;
	public var __implementedBy__ : NativeArray<Type>;
	public function check( v : Dynamic ) {
		var t = Type.getDynamic(v);
		if( __implementedBy__ == null ) {
			if( t.safeCast(__type__) )
				return true;		
			return false;
		}
		for( i in __implementedBy__ )
			if( t.safeCast(i) )
				return true;
		return false;
	}
}

@:keep
class Class extends BaseType {
	public var __name__ : String;
	public var __constructor__ : Dynamic;
}

@:keep
class Enum extends BaseType {
	public var __ename__ : String;
	public var __emap__ : NativeBytesMap;
	public var __constructs__ : Array<String>;
	public var __evalues__ : NativeArray<Dynamic>;
	function new(t,vals) @:privateAccess {
		__type__ = t;
		__evalues__ = vals;
		__ename__ = t.getName();
		__emap__ = new NativeBytesMap();
		__constructs__ = new Array();
		var cl = t.getEnumFields();
		for( i in 0...cl.length ) {
			var name = cl[i];
			__emap__.set(name, i);
			__constructs__.push(String.fromUCS2(name));
		}
		std.Type.register(__ename__.bytes,this);
	}
}

@:keep
class CoreType extends Class {
}

@:keep
class CoreEnum extends Enum {
}
