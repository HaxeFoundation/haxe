package hl.types;

@:coreType abstract Bytes {

	@:extern public inline function new( v : Int ) {
		this = untyped $balloc(v);
	}

	@:extern public inline function blit( pos : Int, src : Bytes, srcPos : Int, len : Int ) {
		untyped $bblit(this, pos, src, srcPos, len);
	}

	@:extern @:arrayAccess public inline function getUI8( pos : Int ) : Int {
		return untyped $bgeti8(this,pos);
	}

	@:extern @:arrayAccess public inline function setUI8( pos : Int, value : Int ) : Int {
		untyped $bseti8(this,pos,value);
		return value;
	}

	@:extern public inline function getI32( pos : Int ) : Int {
		return untyped $bgeti32(this,pos);
	}

	public inline function getUI16( pos : Int ) : Int {
		return getUI8(pos) | (getUI8(pos+1) << 8);
	}

	public inline function setUI16( pos : Int, v : Int ) {
		setUI8(pos, v);
		setUI8(pos + 1, v>>8);
	}

	@:extern public inline function getF32( pos : Int ) : F32 {
		return untyped $bgetf32(this,pos);
	}

	@:extern public inline function getF64( pos : Int ) : Float {
		return untyped $bgetf64(this,pos);
	}

	@:extern public inline function setI32( pos : Int, value : Int ) : Void {
		untyped $bseti32(this, pos, value);
	}

	@:extern public inline function setF32( pos : Int, value : F32 ) : Void {
		untyped $bsetf32(this, pos, value);
	}

	@:extern public inline function setF64( pos : Int, value : Float ) : Void {
		untyped $bsetf64(this, pos, value);
	}

	@:hlNative("std","parse_int")
	public function parseInt( pos : Int, size : Int ) : Null<Int> {
		return null;
	}

	@:hlNative("std","parse_float")
	public function parseFloat( pos : Int, size : Int ) : Float {
		return 0.;
	}

	@:hlNative("std","bytes_compare")
	public function compare( pos : Int, bytes : Bytes, bytesPos : Int, size : Int ) : Int {
		return 0;
	}

	@:hlNative("std","bytes_find")
	public function find( pos : Int, size : Int, bytes : Bytes, bytesPos : Int, bytesSize : Int ) : Int {
		return 0;
	}

	@:hlNative("std","bytes_fill")
	public function fill( pos : Int, size : Int, v : Int ) : Void {
	}

	public function sub( pos : Int, size : Int ) {
		var b = new Bytes(size);
		b.blit(0, this, pos, size);
		return b;
	}


	/**
		Count the number of UTF8 chars into the given Bytes data.
	**/
	@:hlNative("std","utf8length")
	public function _utf8Length( pos : Int, size : Int ) : Int {
		return 0;
	}

	@:hlNative("std", "ucs2length")
	function ucs2Length( bytePos : Int ) : Int {
		return 0;
	}

	/**
		Count the number of bytes until we reach \0
	**/
	@:hlNative("std","byteslength")
	function _bytesLength( pos : Int ) : Int {
		return 0;
	}

	@:hlNative("std","hash")
	function hash() : Int {
		return 0;
	}

	/**
		Decode the utf8 char at the given position
	**/
	@:hlNative("std","utf8char")
	public function _utf8Char( pos : Int, charPos : Int ) : Int {
		return 0;
	}

	/**
		Gives the byte position for the utf8 char starting at pos.
	**/
	@:hlNative("std","utf8pos")
	public function _utf8Pos( pos : Int, charPos : Int ) : Int {
		return 0;
	}

	@:hlNative("std","value_to_string")
	public static function ofValue( v : Dynamic, size : Ref<Int> ) : Bytes {
		return null;
	}
}