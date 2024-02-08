package hl;

#if (hl_ver >= version("1.14.0"))
/**
	CArray is a compact array where all objects are memory aligned and stored as a single GC block.
	You must hold a reference to the CArray while any of the objects it contains is still referenced somewhere.
 **/
abstract CArray<T>(Abstract<"hl_carray">) {

	@:arrayAccess inline function get( index : Int ) : T return untyped this[index];

	public inline function unsafeSet( index : Int, v : T ) return untyped this[index] = v;

	public static inline function alloc<T>( cl : Class<T>, size : Int ) : CArray<T> {
		return cast alloc_carray( (cast cl:BaseType).__type__ , size );
	}

	@:hlNative("?std","alloc_carray")
	static function alloc_carray( t : hl.Type, size : Int ) : CArray<Dynamic> {
		return null;
	}

}
#end