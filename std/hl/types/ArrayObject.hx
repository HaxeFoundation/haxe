package hl.types;

@:coreType abstract ArrayObject<T> {
	public var length(get,never):Int;	
	@:extern public inline function new( length : Int ) {
		this = untyped $aalloc(length);
	}
	@:extern inline function get_length() : Int {
		return untyped $asize(this);
	}
	@:extern @:arrayAccess inline function get( pos : Int ) : T {
		return untyped $aget(this,pos);
	}
	@:extern @:arrayAccess inline function set( pos : Int, value : T ) : T {
		untyped $aset(this,pos,value);
		return value;
	}
}