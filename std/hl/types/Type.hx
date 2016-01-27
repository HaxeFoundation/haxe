package hl.types;

@:enum
abstract TypeKind(Int) {
	public var HVoid = 0;
	public var HI8 = 1;
	public var HI16 = 2;
	public var HI32 = 3;
	public var HF32 = 4;
	public var HF64 = 5;
	public var HBool = 6;
	public var HBytes = 7;
	public var HDyn = 8;
	public var HFun = 9;
	public var HObj = 10;
	public var HArray = 11;
	public var HType = 12;
	public var HRef = 13;
	public var HVirtual = 14;
	public var HDynObj = 15;
	public var HAbstract = 16;
	public var HEnum = 17;
	public var HNull = 18;
}

@:coreType abstract Type {

	public var kind(get,never) : TypeKind;

	@:extern inline function get_kind() : TypeKind {
		return untyped $tkind(this);
	}

	@:hlNative("std","type_check") public function check( v : Dynamic ) : Bool {
		return false;
	}

	@:extern public static inline function getDynamic( v : Dynamic ) : Type {
		return untyped $tdyntype(v);
	}

	@:extern public static inline function get<T>( v : T ) : Type {
		return untyped $ttype(v);
	}

	@:hlNative("std","type_instance_fields") public function getInstanceFields() : NativeArray<Bytes> {
		return null;
	}
}