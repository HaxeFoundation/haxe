package hl.types;

@:coreType abstract Ref<T> {

	@:extern @:from public static inline function make<T>( v : T ) {
		return new Ref<T>(v);
	}

	@:extern public inline function new( v : T ) {
		this = untyped $ref(v);
	}
	@:extern public inline function get() : T {
		return untyped $unref(this);
	}
	@:extern public inline function set( v : T ) : Void {
		return untyped $setref(this,v);
	}
}