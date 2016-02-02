package hl.types;

@:keep
class BaseType {
	public var __type__ : Type;
	public var __meta__ : Dynamic;
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
			__constructs__.push(String.__alloc__(name, name.ucs2Length(0)));
		}
		std.Type.register(__ename__.bytes,this);
	}
}