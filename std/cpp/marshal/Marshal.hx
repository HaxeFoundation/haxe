package cpp.marshal;

import cpp.Char;
import cpp.Char16;
import cpp.Float32;
import cpp.Float64;
import cpp.UInt8;
import cpp.UInt16;
import cpp.UInt32;
import cpp.UInt64;
import cpp.Int8;
import cpp.Int16;
import cpp.Int32;
import cpp.Int64;
import cpp.Pointer;

@:semantics(value)
@:cpp.ValueType({ namespace : [ 'cpp', 'marshal' ] })
final extern class Marshal {
	/**
	 * Returns a view to ASCII encoded characters of the given string. If the internal encoding of the string is not ASCII an exception is raised.
	 * 
	 * @param s String to return the view of.
	 */
	static overload function asCharView(s:String):View<Char>;

	/**
	 * Returns a view to UTF16 encoded characters of the given string. If the internal encoding of the string is not UTF16 an exception is raised.
	 * 
	 * @param s String to return the view of.
	 */
	static overload function asWideCharView(s:String):View<Char16>;

	static function read<T>(view:View<UInt8>):T;

	static function readPointer<T>(view:View<UInt8>):Pointer<T>;
	static function readInt8(view:View<UInt8>):Int8;
	static function readInt16(view:View<UInt8>):Int16;
	static function readInt32(view:View<UInt8>):Int32;
	static function readInt64(view:View<UInt8>):Int64;
	static function readUInt8(view:View<UInt8>):UInt8;
	static function readUInt16(view:View<UInt8>):UInt16;
	static function readUInt32(view:View<UInt8>):UInt32;
	static function readUInt64(view:View<UInt8>):UInt64;
	static function readFloat32(view:View<UInt8>):Float32;
	static function readFloat64(view:View<UInt8>):Float64;

	static function readLittleEndianPointer<T>(view:View<UInt8>):Pointer<T>;
	static function readLittleEndianInt16(view:View<UInt8>):Int16;
	static function readLittleEndianInt32(view:View<UInt8>):Int32;
	static function readLittleEndianInt64(view:View<UInt8>):Int64;
	static function readLittleEndianUInt16(view:View<UInt8>):UInt16;
	static function readLittleEndianUInt32(view:View<UInt8>):UInt32;
	static function readLittleEndianUInt64(view:View<UInt8>):UInt64;
	static function readLittleEndianFloat32(view:View<UInt8>):Float32;
	static function readLittleEndianFloat64(view:View<UInt8>):Float64;

	static function readBigEndianPointer<T>(view:View<UInt8>):Pointer<T>;
	static function readBigEndianInt16(view:View<UInt8>):Int16;
	static function readBigEndianInt32(view:View<UInt8>):Int32;
	static function readBigEndianInt64(view:View<UInt8>):Int64;
	static function readBigEndianUInt16(view:View<UInt8>):UInt16;
	static function readBigEndianUInt32(view:View<UInt8>):UInt32;
	static function readBigEndianUInt64(view:View<UInt8>):UInt64;
	static function readBigEndianFloat32(view:View<UInt8>):Float32;
	static function readBigEndianFloat64(view:View<UInt8>):Float64;

	static function write<T>(view:View<UInt8>, value:T):Void;
	static function writePointer<T>(view:View<UInt8>, value:Pointer<T>):Void;
	static function writeInt8(view:View<UInt8>, value:Int8):Void;
	static function writeInt16(view:View<UInt8>, value:Int16):Void;
	static function writeInt32(view:View<UInt8>, value:Int32):Void;
	static function writeInt64(view:View<UInt8>, value:Int64):Void;
	static function writeUInt8(view:View<UInt8>, value:UInt8):Void;
	static function writeUInt16(view:View<UInt8>, value:UInt16):Void;
	static function writeUInt32(view:View<UInt8>, value:UInt32):Void;
	static function writeUInt64(view:View<UInt8>, value:UInt64):Void;
	static function writeFloat32(view:View<UInt8>, value:Float32):Void;
	static function writeFloat64(view:View<UInt8>, value:Float64):Void;

	static function writeLittleEndianPointer<T>(view:View<UInt8>, value:Pointer<T>):Void;
	static function writeLittleEndianInt16(view:View<UInt8>, value:Int16):Void;
	static function writeLittleEndianInt32(view:View<UInt8>, value:Int32):Void;
	static function writeLittleEndianInt64(view:View<UInt8>, value:Int64):Void;
	static function writeLittleEndianUInt16(view:View<UInt8>, value:UInt16):Void;
	static function writeLittleEndianUInt32(view:View<UInt8>, value:UInt32):Void;
	static function writeLittleEndianUInt64(view:View<UInt8>, value:UInt64):Void;
	static function writeLittleEndianFloat32(view:View<UInt8>, value:Float32):Void;
	static function writeLittleEndianFloat64(view:View<UInt8>, value:Float64):Void;

	static function writeBigEndianPointer<T>(view:View<UInt8>, value:Pointer<T>):Void;
	static function writeBigEndianInt16(view:View<UInt8>, value:Int16):Void;
	static function writeBigEndianInt32(view:View<UInt8>, value:Int32):Void;
	static function writeBigEndianInt64(view:View<UInt8>, value:Int64):Void;
	static function writeBigEndianUInt16(view:View<UInt8>, value:UInt16):Void;
	static function writeBigEndianUInt32(view:View<UInt8>, value:UInt32):Void;
	static function writeBigEndianUInt64(view:View<UInt8>, value:UInt64):Void;
	static function writeBigEndianFloat32(view:View<UInt8>, value:Float32):Void;
	static function writeBigEndianFloat64(view:View<UInt8>, value:Float64):Void;
}