package hl.types;

class NativeBytesMapIterator {
	var arr : NativeArray<Dynamic>;
	var pos : Int;
	var length : Int;
	
	public inline function new(h:NativeBytesMap) {
		this.arr = h.valuesArray();
		pos = 0;
		length = arr.length;
	}
	
	public inline function hasNext() {
		return pos < length;
	}
	
	public inline function next() {
		return arr[pos++];
	}
}

abstract NativeBytesMap(NativeAbstract<"BytesMap">) {

	@:extern public inline function new() {
		this = alloc();
	}
	
	@:hlNative("std","hballoc") function alloc() : NativeAbstract<"BytesMap"> {
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
		return new NativeBytesMapIterator(cast this);
	}

}