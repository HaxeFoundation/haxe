package hl.types;

@:coreType abstract Bytes {
	public inline function new( v : Int ) {
		this = untyped $balloc(v);
	}
	public inline function blit( pos : Int, src : Bytes, srcPos : Int, len : Int ) {
		untyped $bblit(this, pos, src, srcPos, len);
	}
	@:arrayAccess inline function get( pos : Int ) : Int {
		return untyped $bget(this,pos);
	}
	@:arrayAccess inline function set( pos : Int, value : Int ) : Int {
		untyped $bset(this,pos,value);
		return value;
	}
}