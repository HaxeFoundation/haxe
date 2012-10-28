/*
 * Copyright (C)2005-2012 Haxe Foundation
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
import cs.NativeArray;
import cs.NativeArray;
import cs.StdTypes;
import haxe.Int64;

@:native('System.BitConverter') extern class BitConverter 
{
	static var IsLittleEndian(default, null):Bool;
	static function DoubleToInt64Bits(v:Float):Int64;
	static function Int64BitsToDouble(v:Int64):Float;
	static function GetBytes(d:Dynamic):NativeArray<UInt8>;
	static function ToBoolean(b:NativeArray<UInt8>, startIndex:Int):Bool;
	static function ToChar(b:NativeArray<UInt8>, startIndex:Int):Char16;
	static function ToDouble(b:NativeArray<UInt8>, startIndex:Int):Float;
	static function ToInt16(b:NativeArray<UInt8>, startIndex:Int):Int16;
	static function ToInt32(b:NativeArray<UInt8>, startIndex:Int):Int;
	static function ToInt64(b:NativeArray<UInt8>, startIndex:Int):Int64;
	static function ToSingle(b:NativeArray<UInt8>, startIndex:Int):Single;
}