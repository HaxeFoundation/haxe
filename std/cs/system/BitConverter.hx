/*
 * Copyright (C)2005-2019 Haxe Foundation
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

package cs.system;

/**
	Extern for System.BitConverter
**/
@:native("System.BitConverter")
extern class BitConverter {
	static final IsLittleEndian:Bool;

	static function DoubleToInt64Bits(value:Float):haxe.Int64;
	static function Int64BitsToDouble(value:haxe.Int64):Float;
	static function SingleToInt32Bits(value:Single):Int;
	static function Int32BitsToSingle(value:Int):Single;

	@:overload static function GetBytes(value:Bool):cs.NativeArray<cs.UInt8>;
	@:overload static function GetBytes(value:Single):cs.NativeArray<cs.UInt8>;
	@:overload static function GetBytes(value:Float):cs.NativeArray<cs.UInt8>;
	@:overload static function GetBytes(value:Int):cs.NativeArray<cs.UInt8>;
	@:overload static function GetBytes(value:haxe.Int64):cs.NativeArray<cs.UInt8>;

	static function ToBoolean(value:cs.NativeArray<cs.UInt8>, startIndex:Int):Bool;
	static function ToDouble(value:cs.NativeArray<cs.UInt8>, startIndex:Int):Float;
	static function ToInt16(value:cs.NativeArray<cs.UInt8>, startIndex:Int):Int;
	static function ToInt32(value:cs.NativeArray<cs.UInt8>, startIndex:Int):Int;
	static function ToInt64(value:cs.NativeArray<cs.UInt8>, startIndex:Int):haxe.Int64;
	static function ToSingle(value:cs.NativeArray<cs.UInt8>, startIndex:Int):Single;
	static function ToUInt16(value:cs.NativeArray<cs.UInt8>, startIndex:Int):Int;
	static function ToUInt32(value:cs.NativeArray<cs.UInt8>, startIndex:Int):Int;
	static function ToUInt64(value:cs.NativeArray<cs.UInt8>, startIndex:Int):haxe.Int64;
	@:overload static function ToString(value:cs.NativeArray<cs.UInt8>):String;
	@:overload static function ToString(value:cs.NativeArray<cs.UInt8>, startIndex:Int):String;
	@:overload static function ToString(value:cs.NativeArray<cs.UInt8>, startIndex:Int, length:Int):String;
}
