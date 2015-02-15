package haxe.io;

typedef ArrayBufferViewData = #if js js.html.ArrayBufferView #else ArrayBufferViewImpl #end

#if !js
class ArrayBufferViewImpl {
	public var bytes : haxe.io.Bytes;
	public var byteOffset : Int;
	public var byteLength : Int;
	public function new(bytes, pos, length) {
		this.bytes = bytes;
		this.byteOffset = pos;
		this.byteLength = length;
	}
	public function sub( begin : Int, ?length : Int ) {
		if( length == null ) length = byteLength - begin;
		if( begin < 0 || length < 0 || begin + length > byteLength ) throw Error.OutsideBounds;
		return new ArrayBufferViewImpl(bytes, byteOffset + begin, length);
	}
}
#end

abstract ArrayBufferView(ArrayBufferViewData) {

	public static var EMULATED(get,never) : Bool;
	static #if !js inline #end function get_EMULATED() {
		return #if js (cast js.html.ArrayBuffer) == js.html.compat.ArrayBuffer #else false #end;
	}

	public var buffer(get,never) : haxe.io.Bytes;
	public var byteOffset(get, never) : Int;
	public var byteLength(get, never) : Int;

	public inline function new( size : Int ) {
		#if js
		this = new js.html.Uint8Array(size);
		#else
		this = new ArrayBufferViewData(haxe.io.Bytes.alloc(size), 0, size);
		#end
	}

	inline function get_byteOffset() return this.byteOffset;
	inline function get_byteLength() return this.byteLength;
	function get_buffer() : haxe.io.Bytes {
		#if js
		return haxe.io.Bytes.ofData(this.buffer);
		#else
		return this.bytes;
		#end
	}

	public inline function sub( begin : Int, ?length : Int ) {
		#if js
		return fromData(new js.html.Uint8Array(this.buffer.slice(begin, length == null ? null : begin+length)));
		#else
		return fromData(this.sub(begin,length));
		#end
	}
	
	public inline function getData() : ArrayBufferViewData {
		return this;
	}
	
	public static inline function fromData( a : ArrayBufferViewData ) : ArrayBufferView {
		return cast a;
	}

}