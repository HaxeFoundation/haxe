package hl;

#if (hl_ver >= version("1.13.0"))
/**
	CArray is a compact array where all objects are memory aligned and stored as a single GC block.
	You must hold a reference to the CArray while any of the objects it contains is still referenced somewhere.
 **/
abstract CArray<T>(Abstract<"hl_carray">) {

	public var length(get,never) : Int;

	inline function get_length() return getLen(cast this);

	@:arrayAccess inline function get( index : Int ) : T return getIndex(cast this, index);

	public static function alloc<T>( cl : Class<T>, size : Int ) : CArray<T> {
		return cast alloc_carray( (cast cl:BaseType).__type__ , size );
	}

	@:hlNative("?std","carray_get")
	static function getIndex( arr : CArray<Dynamic>, index : Int ) : Dynamic {
		return null;
	}

	@:hlNative("?std","carray_length")
	static function getLen( arr : CArray<Dynamic> ) : Int {
		return 0;
	}

	@:hlNative("?std","alloc_carray")
	static function alloc_carray( t : hl.Type, size : Int ) : CArray<Dynamic> {
		return null;
	}

}
#end