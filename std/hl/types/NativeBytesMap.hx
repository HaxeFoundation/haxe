package hl.types;

typedef NativeBytesMapData = NativeAbstract<"hl_bytes_map">;

abstract NativeBytesMap(NativeBytesMapData) {

	@:extern public inline function new() {
		this = alloc();
	}

	@:hlNative("std","hballoc") static function alloc() : NativeBytesMapData {
		return null;
	}

	@:hlNative("std","hbset")
	public function set( key : Bytes, value : Dynamic ) {
	}

	@:hlNative("std","hbexists")
	public function exists( key : Bytes ) : Bool {
		return false;
	}

	@:hlNative("std","hbget")
	public function get( key : Bytes ) : Dynamic {
		return null;
	}

	@:hlNative("std","hbremove")
	public function remove( key : Bytes ) : Bool {
		return false;
	}

	@:hlNative("std","hbkeys")
	public function keysArray() : NativeArray<Bytes> {
		return null;
	}

	@:hlNative("std","hbvalues")
	public function valuesArray() : NativeArray<Dynamic> {
		return null;
	}

	@:extern public inline function iterator() {
		return new NativeArray.NativeArrayIterator<Dynamic>(valuesArray());
	}

}