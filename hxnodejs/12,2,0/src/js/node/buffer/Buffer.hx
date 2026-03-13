/*
 * Copyright (C)2014-2020 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package js.node.buffer;

import haxe.io.Bytes;
import haxe.io.UInt8Array;
#if haxe4
import js.lib.ArrayBuffer;
import js.lib.ArrayBufferView;
import js.lib.Object;
import js.lib.Uint8Array;
#else
import js.html.ArrayBuffer;
import js.html.ArrayBufferView;
import js.html.Uint8Array;
#end

/**
	The `Buffer` class is a global type for dealing with binary data directly. It can be constructed in a variety of ways.

	@see https://nodejs.org/api/buffer.html#buffer_class_buffer
**/
@:jsRequire("buffer", "Buffer")
extern class Buffer extends Uint8Array {
	/**
		Allocates a new buffer.

		@see https://nodejs.org/api/buffer.html#buffer_new_buffer_array
		@see https://nodejs.org/api/buffer.html#buffer_new_buffer_arraybuffer_byteoffset_length
		@see https://nodejs.org/api/buffer.html#buffer_new_buffer_buffer
		@see https://nodejs.org/api/buffer.html#buffer_new_buffer_size
		@see https://nodejs.org/api/buffer.html#buffer_new_buffer_string_encoding
	**/
	@:deprecated
	@:overload(function(array:Array<Int>):Void {})
	@:overload(function(arrayBuffer:ArrayBuffer, ?byteOffset:Int, ?length:Int):Void {})
	@:overload(function(buffer:UInt8Array):Void {})
	@:overload(function(size:Int):Void {})
	function new(string:String, ?encoding:String):Void;

	/**
		Allocates a new `Buffer` of `size` bytes. If `fill` is `undefined`, the `Buffer` will be zero-filled.

		@see https://nodejs.org/api/buffer.html#buffer_class_method_buffer_alloc_size_fill_encoding
	**/
	@:overload(function(size:Int, fill:String, ?encoding:String):Buffer {})
	@:overload(function(size:Int, fill:Uint8Array):Buffer {})
	@:overload(function(size:Int, fill:Int):Buffer {})
	static function alloc(size:Int):Buffer;

	/**
		Allocates a new `Buffer` of `size` bytes. If `size` is larger than
		`buffer.constants.MAX_LENGTH` or smaller than 0, `ERR_INVALID_OPT_VALUE`
		is thrown. A zero-length `Buffer` is created if `size` is 0.

		@see https://nodejs.org/api/buffer.html#buffer_class_method_buffer_allocunsafe_size
	**/
	static function allocUnsafe(size:Int):Buffer;

	/**
		Allocates a new `Buffer` of `size` bytes. If `size` is larger than
		`buffer.constants.MAX_LENGTH` or smaller than 0, `ERR_INVALID_OPT_VALUE`
		is thrown. A zero-length `Buffer` is created if `size` is 0.

		@see https://nodejs.org/api/buffer.html#buffer_class_method_buffer_allocunsafeslow_size
	**/
	static function allocUnsafeSlow(size:Int):Buffer;

	/**
		Returns the actual byte length of a string. This is not the same as
		`String.prototype.length` since that returns the number of characters in
		a string.

		@see https://nodejs.org/api/buffer.html#buffer_class_method_buffer_bytelength_string_encoding
	**/
	#if (haxe_ver >= 3.3)
	// it need extern SharedArrayBuffer for Node
	// @:overload(function(string:SharedArrayBuffer):Int {})
	@:overload(function(string:String, ?encoding:String):Int {})
	@:overload(function(string:ArrayBufferView):Int {})
	@:overload(function(string:ArrayBuffer):Int {})
	static function byteLength(string:Buffer):Int;
	#end

	#if (haxe_ver >= 3.3)
	@:deprecated("In haxe 3.3+, use Buffer.byteLength instead!")
	#end
	inline static function _byteLength(string:String, ?encoding:String):Int
		return untyped Buffer['byteLength'](string, encoding);

	/**
		Compares `buf1` to `buf2` typically for the purpose of sorting arrays of
		`Buffer` instances. This is equivalent to calling `buf1.compare(buf2)`.

		@see https://nodejs.org/api/buffer.html#buffer_class_method_buffer_compare_buf1_buf2
	**/
	@:native("compare")
	static function compareBuffers(buf1:Uint8Array, buf2:Uint8Array):Int;

	/**
		Returns a new `Buffer` which is the result of concatenating all the `Buffer` instances in the `list` together.

		@see https://nodejs.org/api/buffer.html#buffer_class_method_buffer_concat_list_totallength
	**/
	static function concat<T:Uint8Array>(list:Array<T>, ?totalLength:Int):Buffer;

	/**
		Allocates a new `Buffer`.

		@see https://nodejs.org/api/buffer.html#buffer_class_method_buffer_from_array
		@see https://nodejs.org/api/buffer.html#buffer_class_method_buffer_from_arraybuffer_byteoffset_length
		@see https://nodejs.org/api/buffer.html#buffer_class_method_buffer_from_buffer
		@see https://nodejs.org/api/buffer.html#buffer_class_method_buffer_from_object_offsetorencoding_length
		@see https://nodejs.org/api/buffer.html#buffer_class_method_buffer_from_string_encoding
	**/
	// it need extern SharedArrayBuffer for node
	// @:overload(function(arrayBuffer:SharedArrayBuffer, ?byteOffset:Int, ?length:Int):Buffer {})
	@:overload(function(array:Array<Int>):Buffer {})
	@:overload(function(arrayBuffer:ArrayBuffer, ?byteOffset:Int, ?length:Int):Buffer {})
	@:overload(function(buffer:Uint8Array):Buffer {})
	@:overload(function(object:{}, ?offset:Int, ?length:Int):Buffer {})
	@:overload(function(object:{}, ?encoding:String, ?length:Int):Buffer {})
	static function from(string:String, ?encoding:String):Buffer;

	/**
		Returns `true` if `obj` is a `Buffer`, `false` otherwise.

		@see https://nodejs.org/api/buffer.html#buffer_class_method_buffer_isbuffer_obj
	**/
	static function isBuffer(obj:Dynamic):Bool;

	/**
		Returns `true` if `encoding` contains a supported character encoding, or `false` otherwise.

		@see https://nodejs.org/api/buffer.html#buffer_class_method_buffer_isencoding_encoding
	**/
	static function isEncoding(encoding:String):Bool;

	/**
		This is the size (in bytes) of pre-allocated internal `Buffer` instances used for pooling. This value may be modified.

		@see https://nodejs.org/api/buffer.html#buffer_class_property_buffer_poolsize
	**/
	static var poolSize:Int;

	// buf[index]
	// var buffer:ArrayBuffer;

	/**
		When setting `byteOffset` in `Buffer.from(ArrayBuffer, byteOffset, length)`
		or sometimes when allocating a buffer smaller than `Buffer.poolSize` the
		buffer doesn't start from a zero offset on the underlying `ArrayBuffer`.

		@see https://nodejs.org/api/buffer.html#buffer_buf_byteoffset
	**/
	static var byteOffset(default, null):Int;

	/**
		Compares `buf` with `target` and returns a number indicating whether `buf` comes before, after,
		or is the same as `target` in sort order. Comparison is based on the actual sequence of bytes in each `Buffer`.

		@see https://nodejs.org/api/buffer.html#buffer_buf_compare_target_targetstart_targetend_sourcestart_sourceend
	**/
	@:overload(function(target:Uint8Array):Int {})
	function compare(target:Uint8Array, ?targetStart:Int, ?targetEnd:Int, ?sourceStart:Int, ?sourceEnd:Int):Int;

	/**
		Copies data from a region of `buf` to a region in `target` even if the `target` memory region overlaps with `buf`.

		@see https://nodejs.org/api/buffer.html#buffer_buf_copy_target_targetstart_sourcestart_sourceend
	**/
	@:overload(function(target:Uint8Array):Void {})
	function copy(target:Uint8Array, ?targetStart:Int, ?sourceStart:Int, ?sourceEnd:Int):Void;

	/**
		Creates and returns an iterator of `[index, byte]` pairs from the contents of `buf`.

		@see https://nodejs.org/api/buffer.html#buffer_buf_entries
	**/
	function entries():js.node.Iterator<js.node.KeyValue<Int, Int>>;

	/**
		Returns `true` if both `buf` and `otherBuffer` have exactly the same bytes, `false` otherwise.

		@see https://nodejs.org/api/buffer.html#buffer_buf_equals_otherbuffer
	**/
	function equals(otherBuffer:Uint8Array):Bool;

	/**
		Fills `buf` with the specified `value`. If the `offset` and `end` are not given, the entire `buf` will be filled:

		@see https://nodejs.org/api/buffer.html#buffer_buf_fill_value_offset_end_encoding
	**/
	@:overload(function(value:Uint8Array, ?offset:Int, ?end:Int):Buffer {})
	@:overload(function(value:Int, ?offset:Int, ?end:Int):Buffer {})
	function fill(value:String, ?offset:Int, ?end:Int, ?encoding:String):Buffer;

	/**
		Equivalent to `buf.indexOf() !== -1`.

		@see https://nodejs.org/api/buffer.html#buffer_buf_includes_value_byteoffset_encoding
	**/
	@:overload(function(value:Uint8Array, ?byteOffset:Int):Bool {})
	@:overload(function(value:Int, ?byteOffset:Int):Bool {})
	function includes(value:String, ?byteOffset:Int, ?encoding:String):Bool;

	/**
		If `value` is:

		- a string, `value` is interpreted according to the character encoding in `encoding`.
		- a `Buffer` or Uint8Array, `value` will be used in its entirety. To compare a partial `Buffer`, use buf.slice().
		- a number, `value` will be interpreted as an unsigned 8-bit integer value between `0` and `255`.

		@see https://nodejs.org/api/buffer.html#buffer_buf_indexof_value_byteoffset_encoding
	**/
	@:overload(function(value:Uint8Array, ?byteOffset:Int):Int {})
	@:overload(function(value:Int, ?byteOffset:Int):Int {})
	function indexOf(value:String, ?byteOffset:Int, ?encoding:String):Int;

	/**
		Creates and returns an iterator of `buf` keys (indices).

		@see https://nodejs.org/api/buffer.html#buffer_buf_keys
	**/
	function keys():js.node.Iterator<Int>;

	/**
		Identical to `buf.indexOf()`, except the last occurrence of `value` is found
		rather than the first occurrence.

		@see https://nodejs.org/api/buffer.html#buffer_buf_lastindexof_value_byteoffset_encoding
	**/
	@:overload(function(value:Uint8Array, ?byteOffset:Int):Int {})
	@:overload(function(value:Int, ?byteOffset:Int):Int {})
	function lastIndexOf(value:String, ?byteOffset:Int, ?encoding:String):Int;

	// var length(default, null):Int;
	// these functions need BigInt implementation.
	// /**
	// 	Reads a signed 64-bit integer from `buf` at the specified `offset` with the specified endian format
	// 	(`readBigInt64BE()` returns big endian, `readBigInt64LE()` returns little endian).
	// 	@see https://nodejs.org/api/buffer.html#buffer_buf_readbigint64be_offset
	// **/
	// function readBigInt64BE(?offset:Int):BigInt;
	// /**
	// 	Reads a signed 64-bit integer from `buf` at the specified `offset` with the specified endian format
	// 	(`readBigInt64BE()` returns big endian, `readBigInt64LE()` returns little endian).
	// 	@see https://nodejs.org/api/buffer.html#buffer_buf_readbigint64le_offset
	// **/
	// function readBigInt64LE(?offset:Int):BigInt;
	// /**
	// 	Reads an unsigned 64-bit integer from `buf` at the specified `offset` with specified endian format
	// 	(`readBigUInt64BE()` returns big endian, `readBigUInt64LE()` returns little endian).
	// 	@see https://nodejs.org/api/buffer.html#buffer_buf_readbiguint64be_offset
	// **/
	// function readBigUInt64BE(?offset:Int):BigInt;
	// /**
	// 	Reads an unsigned 64-bit integer from `buf` at the specified `offset` with specified endian format
	// 	(`readBigUInt64BE()` returns big endian, `readBigUInt64LE()` returns little endian).
	// 	@see https://nodejs.org/api/buffer.html#buffer_buf_readbiguint64le_offset
	// **/
	// function readBigUInt64LE(?offset:Int):BigInt;

	/**
		Reads a 64-bit double from `buf` at the specified `offset` with specified endian format
		(`readDoubleBE()` returns big endian, `readDoubleLE()` returns little endian).

		@see https://nodejs.org/api/buffer.html#buffer_buf_readdoublebe_offset
	**/
	function readDoubleBE(?offset:Int):Float;

	/**
		Reads a 64-bit double from `buf` at the specified `offset` with specified endian format
		(`readDoubleBE()` returns big endian, `readDoubleLE()` returns little endian).

		@see https://nodejs.org/api/buffer.html#buffer_buf_readdoublele_offset
	**/
	function readDoubleLE(?offset:Int):Float;

	/**
		Reads a 32-bit float from `buf` at the specified `offset` with specified endian format
		(`readFloatBE()` returns big endian, `readFloatLE()` returns little endian).

		@see https://nodejs.org/api/buffer.html#buffer_buf_readfloatbe_offset
	**/
	function readFloatBE(?offset:Int):Float;

	/**
		Reads a 32-bit float from `buf` at the specified `offset` with specified endian format
		(`readFloatBE()` returns big endian, `readFloatLE()` returns little endian).

		@see https://nodejs.org/api/buffer.html#buffer_buf_readfloatle_offset
	**/
	function readFloatLE(?offset:Int):Float;

	/**
		Reads a signed 8-bit integer from `buf` at the specified `offset`.

		https://nodejs.org/api/buffer.html#buffer_buf_readint8_offset
	**/
	function readInt8(?offset:Int):Int;

	/**
		Reads a signed 16-bit integer from `buf` at the specified `offset` with the specified endian format
		(`readInt16BE()` returns big endian, `readInt16LE()` returns little endian).

		@see https://nodejs.org/api/buffer.html#buffer_buf_readint16be_offset
	**/
	function readInt16BE(?offset:Int):Int;

	/**
		Reads a signed 16-bit integer from `buf` at the specified `offset` with the specified endian format
		(`readInt16BE()` returns big endian, `readInt16LE()` returns little endian).

		@see https://nodejs.org/api/buffer.html#buffer_buf_readint16le_offset
	**/
	function readInt16LE(?offset:Int):Int;

	/**
		Reads a signed 32-bit integer from buf at the specified offset with the specified endian format
		(`readInt32BE()` returns big endian, `readInt32LE()` returns little endian).

		@see https://nodejs.org/api/buffer.html#buffer_buf_readint32be_offset
	**/
	function readInt32BE(?offset:Int):Int;

	/**
		Reads a signed 32-bit integer from buf at the specified offset with the specified endian format
		(`readInt32BE()` returns big endian, `readInt32LE()` returns little endian).

		@see https://nodejs.org/api/buffer.html#buffer_buf_readint32be_offset
	**/
	function readInt32LE(?offset:Int):Int;

	/**
		Reads `byteLength` number of bytes from `buf` at the specified `offset` and interprets the result
		as a two's complement signed value. Supports up to 48 bits of accuracy.

		@see https://nodejs.org/api/buffer.html#buffer_buf_readintbe_offset_bytelength
	**/
	function readIntBE(offset:Int, byteLength:Int):Int;

	/**
		Reads `byteLength` number of bytes from `buf` at the specified `offset` and interprets the result
		as a two's complement signed value. Supports up to 48 bits of accuracy.

		@see https://nodejs.org/api/buffer.html#buffer_buf_readintle_offset_bytelength
	**/
	function readIntLE(offset:Int, byteLength:Int):Int;

	/**
		Reads an unsigned 8-bit integer from `buf` at the specified `offset`.

		@see https://nodejs.org/api/buffer.html#buffer_buf_readuint8_offset
	**/
	function readUInt8(?offset:Int):Int;

	/**
		Reads an unsigned 16-bit integer from `buf` at the specified `offset` with specified endian format
		`readUInt16BE()` returns big endian, `readUInt16LE()` returns little endian).

		@see https://nodejs.org/api/buffer.html#buffer_buf_readuint16be_offset
	**/
	function readUInt16BE(?offset:Int):Int;

	/**
		Reads an unsigned 16-bit integer from `buf` at the specified `offset` with specified endian format
		(`readUInt16BE()` returns big endian, `readUInt16LE()` returns little endian).

		@see https://nodejs.org/api/buffer.html#buffer_buf_readuint16le_offset
	**/
	function readUInt16LE(?offset:Int):Int;

	/**
		Reads an unsigned 32-bit integer from `buf` at the specified `offset` with specified endian format
		(`readUInt32BE()` returns big endian, `readUInt32LE()` returns little endian).

		@see https://nodejs.org/api/buffer.html#buffer_buf_readuint32be_offset
	**/
	function readUInt32BE(?offset:Int):Int;

	/**
		Reads an unsigned 32-bit integer from `buf` at the specified `offset` with specified endian format
		(`readUInt32BE()` returns big endian, `readUInt32LE()` returns little endian).

		@see https://nodejs.org/api/buffer.html#buffer_buf_readuint32be_offset
	**/
	function readUInt32LE(?offset:Int):Int;

	/**
		Returns a new `Buffer` that references the same memory as the original,
		but offset and cropped by the `start` and `end` indices.

		@see https://nodejs.org/api/buffer.html#buffer_buf_subarray_start_end
	**/
	#if haxe4
	function subarray(?start:Int, ?end:Int):Buffer;
	#else
	override function subarray(start:Int, ?end:Int):Buffer;
	#end

	/**
		Returns a new `Buffer` that references the same memory as the original,
		but offset and cropped by the `start` and `end` indices.

		@see https://nodejs.org/api/buffer.html#buffer_buf_slice_start_end
	**/
	function slice(?start:Int, ?end:Int):Buffer;

	/**
		Interprets `buf` as an array of unsigned 16-bit integers and swaps the
		byte order in-place. Throws `ERR_INVALID_BUFFER_SIZE` if `buf.length`
		is not a multiple of 2.

		@see https://nodejs.org/api/buffer.html#buffer_buf_swap16
	**/
	function swap16():Buffer;

	/**
		Interprets `buf` as an array of unsigned 32-bit integers and swaps the
		byte order in-place. Throws `ERR_INVALID_BUFFER_SIZE` if `buf.length`
		is not a multiple of 4.

		@see https://nodejs.org/api/buffer.html#buffer_buf_swap32
	**/
	function swap32():Buffer;

	/**
		Interprets `buf` as an array of 64-bit numbers and swaps byte order in-place.
		Throws `ERR_INVALID_BUFFER_SIZE` if `buf.length` is not a multiple of 8.

		@see https://nodejs.org/api/buffer.html#buffer_buf_swap64
	**/
	function swap64():Buffer;

	/**
		Returns a JSON representation of `buf`. `JSON.stringify()` implicitly calls
		this function when stringifying a `Buffer` instance.

		@see https://nodejs.org/api/buffer.html#buffer_buf_tojson
	**/
	function toJSON():Dynamic;

	/**
		Decodes `buf` to a string according to the specified character encoding in `encoding`.
		`start` and `end` may be passed to decode only a subset of `buf`.

		@see https://nodejs.org/api/buffer.html#buffer_buf_tostring_encoding_start_end
	**/
	@:overload(function(?encoding:String, ?start:Int, ?end:Int):String {})
	function toString():String;

	/**
		Creates and returns an iterator for `buf` values (bytes). This function is called automatically
		when a `Buffer` is used in a `for..of` statement.

		@see https://nodejs.org/api/buffer.html#buffer_buf_values
	**/
	function values():js.node.Iterator<Int>;

	/**
		Writes `string` to `buf` at `offset` according to the character encoding in `encoding`.
		The `length` parameter is the number of bytes to write.
		If `buf` did not contain enough space to fit the entire `string`, only part of string will be written.
		However, partially encoded characters will not be written.

		@see https://nodejs.org/api/buffer.html#buffer_buf_write_string_offset_length_encoding
	**/
	function write(string:String, ?offset:Int, ?length:Int, ?encoding:String):Int;

	// these functions need BigInt Implementation.
	// /**
	// 	Writes `value` to `buf` at the specified `offset` with specified endian format (`writeBigInt64BE()` writes big endian, `writeBigInt64LE()` writes little endian).
	// 	@see https://nodejs.org/api/buffer.html#buffer_buf_writebigint64be_value_offset
	// **/
	// function writeBigInt64BE(value:Int, ?offset:Int):BigInt;
	// /**
	// 	Writes `value` to `buf` at the specified `offset` with specified endian format (`writeBigInt64BE()` writes big endian, `writeBigInt64LE()` writes little endian).
	// 	@see https://nodejs.org/api/buffer.html#buffer_buf_writebigint64le_value_offset
	// **/
	// function writeBigInt64LE(value:Int, ?offset:Int):BigInt;

	/**
		Writes `value` to `buf` at the specified `offset` with specified endian format
		(`writeDoubleBE()` writes big endian, `writeDoubleLE()` writes little endian).
		`value` should be a valid 64-bit double. Behavior is undefined when `value` is anything other than a 64-bit double.

		@see https://nodejs.org/api/buffer.html#buffer_buf_writedoublebe_value_offset
	**/
	function writeDoubleBE(value:Float, ?offset:Int):Void;

	/**
		Writes `value` to `buf` at the specified `offset` with specified endian format
		(`writeDoubleBE()` writes big endian, `writeDoubleLE()` writes little endian).
		`value` should be a valid 64-bit double. Behavior is undefined when `value` is anything other than a 64-bit double.

		@see https://nodejs.org/api/buffer.html#buffer_buf_writedoublele_value_offset
	**/
	function writeDoubleLE(value:Float, ?offset:Int):Void;

	/**
		Writes `value` to `buf` at the specified `offset` with specified endian format
		(`writeFloatBE()` writes big endian, `writeFloatLE()` writes little endian).
		`value` should be a valid 32-bit float. Behavior is undefined when `value` is anything other than a 32-bit float.

		@see https://nodejs.org/api/buffer.html#buffer_buf_writefloatbe_value_offset
	**/
	function writeFloatBE(value:Float, ?offset:Int):Void;

	/**
		Writes `value` to `buf` at the specified `offset` with specified endian format
		(`writeFloatBE()` writes big endian, `writeFloatLE()` writes little endian).
		`value` should be a valid 32-bit float. Behavior is undefined when `value` is anything other than a 32-bit float.

		@see https://nodejs.org/api/buffer.html#buffer_buf_writefloatle_value_offset
	**/
	function writeFloatLE(value:Float, ?offset:Int):Void;

	/**
		Writes `value` to `buf` at the specified `offset`. `value` should be a valid signed 8-bit integer.
		Behavior is undefined when `value` is anything other than a signed 8-bit integer.

		@see https://nodejs.org/api/buffer.html#buffer_buf_writeint8_value_offset
	**/
	function writeInt8(value:Int, ?offset:Int):Void;

	/**
		Writes `value` to `buf` at the specified `offset` with specified endian format
		(`writeInt16BE()` writes big endian, `writeInt16LE()` writes little endian).
		`value` should be a valid signed 16-bit integer.
		Behavior is undefined when value is anything other than a signed 16-bit integer.

		@see https://nodejs.org/api/buffer.html#buffer_buf_writeint16be_value_offset
	**/
	function writeInt16BE(value:Int, ?offset:Int):Void;

	/**
		Writes `value` to `buf` at the specified `offset` with specified endian format
		(`writeInt16BE()` writes big endian, `writeInt16LE()` writes little endian).
		`value` should be a valid signed 16-bit integer.
		Behavior is undefined when value is anything other than a signed 16-bit integer.

		@see https://nodejs.org/api/buffer.html#buffer_buf_writeint16le_value_offset
	**/
	function writeInt16LE(value:Int, ?offset:Int):Void;

	/**
		Writes `value` to `buf` at the specified `offset` with specified endian format
		(`writeInt32BE()` writes big endian, `writeInt32LE()` writes little endian).
		`value` should be a valid signed 32-bit integer.
		Behavior is undefined when `value` is anything other than a signed 32-bit integer.

		@see https://nodejs.org/api/buffer.html#buffer_buf_writeint32be_value_offset
	**/
	function writeInt32BE(value:Int, ?offset:Int):Void;

	/**
		Writes `value` to `buf` at the specified `offset` with specified endian format
		(`writeInt32BE()` writes big endian, `writeInt32LE()` writes little endian).
		`value` should be a valid signed 32-bit integer.
		Behavior is undefined when `value` is anything other than a signed 32-bit integer.

		@see https://nodejs.org/api/buffer.html#buffer_buf_writeint32le_value_offset
	**/
	function writeInt32LE(value:Int, ?offset:Int):Void;

	/**
		Writes `byteLength` bytes of `value` to `buf` at the specified `offset`.
		Supports up to 48 bits of accuracy. Behavior is undefined when `value` is anything other than a signed integer.

		@see https://nodejs.org/api/buffer.html#buffer_buf_writeintbe_value_offset_bytelength
	**/
	function writeIntBE(value:Int, offset:Int, byteLength:Int):Int;

	/**
		Writes `byteLength` bytes of `value` to `buf` at the specified `offset`.
		Supports up to 48 bits of accuracy. Behavior is undefined when `value` is anything other than a signed integer.

		@see https://nodejs.org/api/buffer.html#buffer_buf_writeintle_value_offset_bytelength
	**/
	function writeIntLE(value:Int, offset:Int, byteLength:Int):Int;

	/**
		Writes `value` to `buf` at the specified `offset`. `value` should be a valid unsigned 8-bit integer.
		Behavior is undefined when `value` is anything other than an unsigned 8-bit integer.

		@see https://nodejs.org/api/buffer.html#buffer_buf_writeuint8_value_offset
	**/
	function writeUInt8(value:Int, ?offset:Int):Void;

	/**
		Writes `value` to `buf` at the specified `offset` with specified endian format
		(`writeUInt16BE()` writes big endian, `writeUInt16LE()` writes little endian).
		`value` should be a valid unsigned 16-bit integer.
		Behavior is undefined when `value` is anything other than an unsigned 16-bit integer.

		@see https://nodejs.org/api/buffer.html#buffer_buf_writeuint16be_value_offset
	**/
	function writeUInt16BE(value:Int, ?offset:Int):Void;

	/**
		Writes `value` to `buf` at the specified `offset` with specified endian format
		(`writeUInt16BE()` writes big endian, `writeUInt16LE()` writes little endian).
		`value` should be a valid unsigned 16-bit integer.
		Behavior is undefined when `value` is anything other than an unsigned 16-bit integer.

		@see https://nodejs.org/api/buffer.html#buffer_buf_writeuint16le_value_offset
	**/
	function writeUInt16LE(value:Int, ?offset:Int):Void;

	/**
		Writes `value` to `buf` at the specified `offset` with specified endian format
		(`writeUInt32BE()` writes big endian, `writeUInt32LE()` writes little endian).
		`value` should be a valid unsigned 32-bit integer.
		Behavior is undefined when `value` is anything other than an unsigned 32-bit integer.

		@see https://nodejs.org/api/buffer.html#buffer_buf_writeuint32be_value_offset
	**/
	function writeUInt32BE(value:Int, ?offset:Int):Void;

	/**
		Writes `value` to `buf` at the specified `offset` with specified endian format
		(`writeUInt32BE()` writes big endian, `writeUInt32LE()` writes little endian).
		`value` should be a valid unsigned 32-bit integer.
		Behavior is undefined when `value` is anything other than an unsigned 32-bit integer.

		@see https://nodejs.org/api/buffer.html#buffer_buf_writeuint32le_value_offset
	**/
	function writeUInt32LE(value:Int, ?offset:Int):Void;

	/**
		Default: `50`

		Returns the maximum number of bytes that will be returned when `buf.inspect()` is called.
		This can be overridden by user modules.
		See `util.inspect()` for more details on `buf.inspect()` behavior.

		This is a property on the `buffer` module returned by `require('buffer')`, not on the `Buffer` global or a `Buffer` instance.

		@see https://nodejs.org/api/buffer.html#buffer_buffer_inspect_max_bytes
	**/
	static var INSPECT_MAX_BYTES(get, set):Int;

	private static inline function get_INSPECT_MAX_BYTES():Int {
		return BufferModule.INSPECT_MAX_BYTES;
	}
	private static inline function set_INSPECT_MAX_BYTES(bytes:Int):Int {
		return BufferModule.INSPECT_MAX_BYTES = bytes;
	}

	/**
		An alias for `buffer.constants.MAX_LENGTH`.

		This is a property on the `buffer` module returned by `require('buffer')`, not on the `Buffer` global or a `Buffer` instance.

		@see https://nodejs.org/api/buffer.html#buffer_buffer_kmaxlength
	**/
	static var kMaxLength(get, never):Int;

	private static inline function get_kMaxLength():Int {
		return BufferModule.kMaxLength;
	}

	/**
		Re-encodes the given `Buffer` or `Uint8Array` instance from one character encoding to another.
		Returns a new `Buffer` instance.

		@see https://nodejs.org/api/buffer.html#buffer_buffer_transcode_source_fromenc_toenc
	**/
	static inline function transcode(source:Uint8Array, fromEnc:String, toEnc:String):Buffer {
		return BufferModule.transcode(source, fromEnc, toEnc);
	};

	/**
		`buffer.constants` is a property on the `buffer` module returned by `require('buffer')`,
		not on the `Buffer` global or a `Buffer` instance.

		@see https://nodejs.org/api/buffer.html#buffer_buffer_constants
	**/
	static var constants(default, never):BufferConstants;

	private static inline function get_constants():BufferConstants {
		return BufferModule.constants;
	}

	/**
		Create `haxe.io.Bytes` object that uses the same underlying data storage as `this` buffer.
		Any modifications done using the returned object will be reflected in the `this` buffer.
	**/
	inline function hxToBytes():Bytes {
		return Helper.bytesOfBuffer(this);
	}

	/**
		Create `Buffer` object from `haxe.io.Bytes` using the same underlying data storage.
		Any modifications done using the returned object will be reflected in given `haxe.io.Bytes` object.
	**/
	static inline function hxFromBytes(b:Bytes):Buffer {
		var data = @:privateAccess b.b;
		return Buffer.from(data.buffer, data.byteOffset, b.length);
	}
}

@:dce
private class Helper {
	public static function bytesOfBuffer(b:Buffer):haxe.io.Bytes untyped {
		var o = Object.create(haxe.io.Bytes.prototype);
		// the following is basically a haxe.io.Bytes constructor,
		// but using given buffer instead of creating new Uint8Array
		o.length = b.byteLength;
		o.b = b;
		b.bufferValue = b;
		b.hxBytes = o;
		b.bytes = b;
		return o;
	}
}

@:jsRequire("buffer")
private extern class BufferModule {
	static var INSPECT_MAX_BYTES:Int;
	static var kMaxLength(default, never):Int;
	static function transcode(source:Uint8Array, fromEnc:String, toEnc:String):Buffer;
	static var constants(default, never):BufferConstants;
}

typedef BufferConstants = {
	/**
		On 32-bit architectures, this value is `(2^30)-1` (`~1GB`).
		On 64-bit architectures, this value is `(2^31)-1` (`~2GB`).

		@see https://nodejs.org/api/buffer.html#buffer_buffer_constants_max_length
	**/
	var MAX_LENGTH(default, never):Int;

	/**
		Represents the largest `length` that a `string` primitive can have, counted
		in UTF-16 code units.

		@see https://nodejs.org/api/buffer.html#buffer_buffer_constants_max_string_length
	**/
	var MAX_STRING_LENGTH(default, never):Int;
}
