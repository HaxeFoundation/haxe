package hl.types;

@:coreType abstract BytesAccess<T> from Bytes to Bytes {

	public var sizeBits(get, never) : Int;
	public var nullValue(get, never) : T;


	@:isExtern inline function get_sizeBits() {
		return untyped $bytes_sizebits(this);
	}

	@:isExtern inline function get_nullValue() {
		return untyped $bytes_nullvalue(this);
	}

	@:arrayAccess public inline function get( pos : Int ) : T {
		return untyped $bget(this,pos);
	}

	@:arrayAccess public inline function set( pos : Int, value : T ) : T {
		untyped $bset(this,pos,value);
		return value;
	}

}