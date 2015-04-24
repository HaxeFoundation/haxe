package haxe.io;

typedef ArrayBufferViewData = js.html.ArrayBufferView;

abstract ArrayBufferView(ArrayBufferViewData) {

	public static var EMULATED(get,never) : Bool;
	static function get_EMULATED() {
		return (cast js.html.ArrayBuffer) == js.html.compat.ArrayBuffer;
	}

	public var buffer(get,never) : haxe.io.Bytes;
	public var byteOffset(get, never) : Int;
	public var byteLength(get, never) : Int;

	public inline function new( size : Int ) {
		this = new js.html.Uint8Array(size);
	}

	inline function get_byteOffset() return this.byteOffset;
	inline function get_byteLength() return this.byteLength;
	function get_buffer() : haxe.io.Bytes {
		return haxe.io.Bytes.ofData(this.buffer);
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