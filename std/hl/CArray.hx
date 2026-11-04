package hl;

#if (hl_ver >= version("1.14.0"))
/**
	CArray is a compact array where all objects are memory aligned and stored as a single GC block.
	You must hold a reference to the CArray while any of the objects it contains is still referenced somewhere.
 **/
abstract CArray<T>(Abstract<"hl_carray">) {

	@:arrayAccess inline function get( index : Int ) : T return untyped this[index];

	public inline function unsafeSet( index : Int, v : T ) : T return untyped this[index] = v;

	public static inline function alloc<T>( cl : Class<T>, size : Int ) : CArray<T> {
		return cast alloc_carray( (cast cl:BaseType).__type__ , size );
	}

	@:hlNative("?std","alloc_carray")
	static function alloc_carray( t : hl.Type, size : Int ) : CArray<Dynamic> {
		return null;
	}

	public static function fromBytes<T>( bytes : haxe.io.Bytes ) : CArray<T> {
		return bytes == null ? null : hl.Api.unsafeCast(@:privateAccess bytes.b);
	}

	#if (hl_ver >= version("1.16.0"))

	public function toBytes( cl : Class<T>, count : Int ) : haxe.io.Bytes {
		if( this == null )
			return null;
		var size = (cast cl:BaseType).__type__.getDataSize();
		return @:privateAccess new haxe.io.Bytes(hl.Api.unsafeCast(this),size * count);
	}

	public inline function blit( cl : Class<T>, pos : Int, src : CArray<T>, srcPos : Int, srcLen : Int ) : Void {
		carray_blit( cast this, (cast cl:BaseType).__type__, pos, src, srcPos, srcLen );
	}

	@:hlNative("?std","carray_blit")
	static function carray_blit( dst: CArray<Dynamic>, t : hl.Type, pos : Int, src : CArray<Dynamic>, srcPos : Int, srcLen : Int ) : Void {
	}
	#end

}
#end
