package hl.types;

abstract NativeIntMap(NativeAbstract<"IntMap">) {

	@:extern public inline function new() {
		this = alloc();
	}
	
	@:hlNative("std","hialloc") function alloc() : NativeAbstract<"IntMap"> {
		return null;
	}

	@:hlNative("std","hiset")
	public function set( key : Int, value : Dynamic ) {
	}

	@:hlNative("std","hiexists")
	public function exists( key : Int ) : Bool {
		return false;
	}
	
	@:hlNative("std","higet")
	public function get( key : Int ) : Dynamic {
		return null;
	}

	@:hlNative("std","hiremove")
	public function remove( key : Int ) : Bool {
		return false;
	}

	@:hlNative("std","hikeys")
	public function keysArray() : NativeArray<Int> {
		return null;
	}

	@:hlNative("std","hivalues")
	public function valuesArray() : NativeArray<Dynamic> {
		return null;
	}

	@:extern public inline function iterator() {
		return new NativeArray.NativeArrayIterator<Dynamic>(valuesArray());
	}

}