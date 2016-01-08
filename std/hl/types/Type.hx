package hl.types;

@:coreType abstract Type {
	@:extern static inline function get( v : Dynamic ) {
		return untyped $gettype(v);
	}
	@:hlNative("std","type_instance_fields") public function getInstanceFields() : NativeArray<Bytes> {
		return null;
	}
}