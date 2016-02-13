package hl.types;

typedef NativeObjectMapData = NativeAbstract<"hl_obj_map">;

abstract NativeObjectMap(NativeObjectMapData) {

	@:extern public inline function new() {
		this = alloc();
	}
	
	@:hlNative("std","hoalloc") static function alloc() : NativeObjectMapData {
		return null;
	}

	@:hlNative("std","hoset")
	public function set( key : Dynamic, value : Dynamic ) {
	}

	@:hlNative("std","hoexists")
	public function exists( key : Dynamic ) : Bool {
		return false;
	}
	
	@:hlNative("std","hoget")
	public function get( key : Dynamic ) : Dynamic {
		return null;
	}

	@:hlNative("std","horemove")
	public function remove( key : Dynamic ) : Bool {
		return false;
	}

	@:hlNative("std","hokeys")
	public function keysArray() : NativeArray<Dynamic> {
		return null;
	}

	@:hlNative("std","hovalues")
	public function valuesArray() : NativeArray<Dynamic> {
		return null;
	}

	@:extern public inline function iterator() {
		return new NativeArray.NativeArrayIterator<Dynamic>(valuesArray());
	}

}