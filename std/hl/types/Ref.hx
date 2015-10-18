package hl.types;

@:coreType abstract Ref<T> {
	public inline function new( v : T ) {
		this = untyped $ref(v);
	}
	public inline function get() : T {
		return untyped $unref(this);
	}
	public inline function set( v : T ) : Void {
		return untyped $setref(this,v);
	}
}