/*
 * Copyright (C)2005-2015 Haxe Foundation
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

// This file is generated from typedarray.webidl line 217:0. Do not edit!

package js.html;

@:native("DataView")
extern class DataView extends ArrayBufferView
{
	/** @throws DOMError */
	function new( buffer : ArrayBuffer, ?byteOffset : Int, ?byteLength : Int ) : Void;
	function getInt8( byteOffset : Int ) : Int;
	function getUint8( byteOffset : Int ) : Int;
	function getInt16( byteOffset : Int, ?littleEndian : Bool ) : Int;
	function getUint16( byteOffset : Int, ?littleEndian : Bool ) : Int;
	function getInt32( byteOffset : Int, ?littleEndian : Bool ) : Int;
	function getUint32( byteOffset : Int, ?littleEndian : Bool ) : Int;
	function getFloat32( byteOffset : Int, ?littleEndian : Bool ) : Float;
	function getFloat64( byteOffset : Int, ?littleEndian : Bool ) : Float;
	function setInt8( byteOffset : Int, value : Int ) : Void;
	function setUint8( byteOffset : Int, value : Int ) : Void;
	function setInt16( byteOffset : Int, value : Int, ?littleEndian : Bool ) : Void;
	function setUint16( byteOffset : Int, value : Int, ?littleEndian : Bool ) : Void;
	function setInt32( byteOffset : Int, value : Int, ?littleEndian : Bool ) : Void;
	function setUint32( byteOffset : Int, value : Int, ?littleEndian : Bool ) : Void;
	function setFloat32( byteOffset : Int, value : Float, ?littleEndian : Bool ) : Void;
	function setFloat64( byteOffset : Int, value : Float, ?littleEndian : Bool ) : Void;
}