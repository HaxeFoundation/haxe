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
		this = untyped $hballoc();
	}

	@:extern public inline function set( key : Bytes, value : Dynamic ) {
		untyped $hbset(this, key, value);
	}

	@:extern public inline function exists( key : Bytes ) : Bool {
		return untyped $hbexists(this, key);
	}
	
	@:extern public inline function get( key : Bytes ) : Dynamic {
		return untyped $hbget(this, key);
	}

	@:extern public inline function remove( key : Bytes ) : Bool {
		return untyped $hbremove(this, key);
	}

	@:extern public inline function keysArray() : NativeArray<Bytes> {
		return untyped $hbkeys(this);
	}

	@:extern public inline function valuesArray() : NativeArray<Dynamic> {
		return untyped $hbvalues(this);
	}

	@:extern public inline function iterator() {
		return new NativeBytesMapIterator(cast this);
	}

}